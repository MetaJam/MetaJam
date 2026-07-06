#' Initialize Meta-Regression Models
#'
#' Adds one group per block to the `metaRegModels` Array and sets
#' dynamic Group titles. When required variables are missing or a
#' block is empty, a title-only placeholder is set on the text
#' element (same pattern as `initText()`).
#'
#' @param self The jamovi `self` object.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
initMetaRegModels <- function(self, requiredVars) {
  modelsArray <- self$results$metaRegModels
  options <- self$options
  blocks <- options$metaRegBlocks
  hasVars <- hasRequiredVars(options, requiredVars)

  # TODO: We suspect that initializing a large number of items (e.g., 50) via
  # `addItem` might be a performance bottleneck due to R6 cloning overhead and
  # UI rendering time. We should verify this performance claim in the future. If
  # large arrays of models cause significant lag, we may need to discuss this
  # issue with the jamovi authors.
  for (i in seq_along(blocks)) {
    # 1. Initialize array items
    modelsArray$addItem(key = i)
    group <- modelsArray$get(key = i)
    terms <- blocks[[i]]

    # 2. Build and set group title
    if (length(terms) == 0) {
      title <- paste0("Meta-Regression - Model ", i)
    } else {
      termStrings <- vapply(
        terms,
        function(t) jmvcore::stringifyTerm(t, raise = TRUE),
        character(1)
      )
      title <- paste0(
        "Meta-Regression - Model ",
        i,
        ": ~ ",
        paste(termStrings, collapse = " + ")
      )
    }

    group$setTitle(title)

    # 3. Set text placeholder when vars missing or block empty
    if (group$metaRegText$visible && (!hasVars || length(terms) == 0)) {
      group$metaRegText$setContent(asHtml(title = "Model Summary"))
    }
  }

  invisible(NULL)
}


#' Compute Meta-Regression Models for All Blocks
#'
#' Iterates over `options$metaRegBlocks`, building a formula for each
#' block and calling `meta::metareg()`. Returns a list of models
#' (NULL entries for empty blocks). Cross-cycle caching is performed
#' per-block via the corresponding `metaRegText` results element.
#'
#' @param self The jamovi `self` object.
#' @return A list of `metareg` objects (NULL entries for empty blocks),
#'   or `NULL` if the main model is not available.
#' @noRd
computeMetaRegModels <- function(self) {
  blocks <- self$options$metaRegBlocks

  # Early exit if no meta-regression blocks are defined. We check this first to
  # avoid forcing the active binding for `self$model` when no models will be
  # computed.
  if (sum(lengths(blocks)) == 0) {
    return()
  }

  options <- self$options
  modelsArray <- self$results$metaRegModels
  models <- vector("list", length(blocks))
  missing <- integer()

  # We must restore cached meta-regression models BEFORE entering the
  # calculation path. If a model is missing from the cache during later
  # lifecycle phases (e.g., image rendering or save/export), it means its
  # calculation failed during the .run() phase and threw an error. In these
  # later phases, jamovi clears self$data and it becomes NULL. If we attempted
  # to recalculate the missing model with NULL data, it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this, we
  # explicitly check if data is NULL below and safely abort, ensuring the true
  # error is shown.
  for (i in seq_along(blocks)) {
    terms <- blocks[[i]]
    if (length(terms) == 0) {
      next
    }

    cacheElement <- modelsArray$get(key = i)$metaRegText

    # Cross-cycle cache (restored via clearWith)
    cached <- cacheElement$state
    if (!is.null(cached)) {
      models[[i]] <- cached
      next
    }

    missing <- c(missing, i)
  }

  if (length(missing) == 0) {
    return(models)
  }

  model <- self$model
  if (is.null(model)) {
    return(models)
  }

  data <- self$data
  # jamovi lifecycle guard: A user in jamovi cannot pass NULL data; during a
  # normal .run() cycle, jamovi always provides a data.frame (with at least
  # one row). The ONLY time self$data is NULL is during later internal phases
  # like image rendering or save/export, when jamovi actively clears it. In
  # these later stages, we rely purely on cached models. If a model is missing
  # from the cache, it means an error occurred during the .run() phase. We do
  # not need to calculate it again. Furthermore, we cannot calculate it anyway
  # because using NULL data would crash with a new, confusing error. Returning
  # the partially restored cache here safely aborts the attempt and preserves
  # the original .run() error.
  if (is.null(data)) {
    return(models)
  }

  # Only selected moderators are appended to the cached meta object. They are
  # stored under B64 names so meta::bubble() can parse formula text safely and
  # so user columns cannot collide with meta's internal dot-prefixed columns.
  moderators <- c(options$metaRegCovs, options$metaRegFactors)
  b64Map <- buildB64Map(moderators)

  data[options$metaRegCovs] <- lapply(
    data[options$metaRegCovs],
    jmvcore::toNumeric
  )

  moderatorData <- data[moderators]
  names(moderatorData) <- jmvcore::toB64(moderators)
  model$data[names(moderatorData)] <- moderatorData

  for (i in missing) {
    terms <- blocks[[i]]
    cacheElement <- modelsArray$get(key = i)$metaRegText

    termsB64 <- lapply(terms, jmvcore::toB64)
    composed <- jmvcore::composeTerms(termsB64)
    formula <- as.formula(paste("~", paste(composed, collapse = " + ")))

    models[[i]] <- decodeB64Conditions(
      meta::metareg(
        model,
        formula,
        intercept = options$metaRegIntercept
      ),
      b64Map
    )

    models[[i]]$.metajam <- list(b64Map = b64Map)

    # meta::metareg is an S3 generic. R's S3 dispatch injects the local
    # execution environment (which in Jamovi contains the heavy R6 `self`
    # wrapper) into the formula as `.GenericCallEnv`. This causes the Jamovi
    # `self` object to be serialized alongside the model, massively bloating the
    # save state. We sever this link by zeroing out the formula environments.
    if (!is.null(models[[i]]$formula.mods)) {
      environment(models[[i]]$formula.mods) <- baseenv()
    }
    if (!is.null(models[[i]]$.meta$formula)) {
      environment(models[[i]]$.meta$formula) <- baseenv()
    }
    # Prevent future do.call() paths from caching the full source model in the
    # call.
    models[[i]]$.meta$call <- NULL

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Populate Meta-Regression Text for All Models
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards per block: skips when hidden, already filled (clearWith
#' cache hit), or block model is NULL. Unlike other populate
#' functions, the NULL check here is NOT redundant — it handles
#' empty blocks where the user has not added any terms yet.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateMetaRegTexts <- function(self) {
  modelsArray <- self$results$metaRegModels
  options <- self$options

  for (i in seq_along(options$metaRegBlocks)) {
    textResult <- modelsArray$get(key = i)$metaRegText

    if (!textResult$visible || textResult$isFilled()) {
      next
    }

    metaRegModel <- self$metaRegModels[[i]]
    if (is.null(metaRegModel)) {
      next
    }

    scaleLabel <- getMetaRegScaleLabel(metaRegModel)
    b64Map <- metaRegModel$.metajam$b64Map

    # metafor's print methods build the "Model Results" table from
    # rownames(x$beta) before fixed-width printing. Decode these names before
    # summary() is captured so table alignment is calculated from displayed
    # names, not internal B64 names. Source checkpoints:
    # - metafor/R/print.rma.uni.r: lines 240-247
    # - metafor/R/print.rma.glmm.r: lines 133-138
    # - metafor/R/print.rma.mv.r: lines 396-403
    rownames(metaRegModel$beta) <- decodeB64(
      rownames(metaRegModel$beta),
      b64Map
    )
    rownames(metaRegModel$beta)[
      rownames(metaRegModel$beta) == "intrcpt"
    ] <- "Intercept"

    textResult$setContent(
      asHtml(
        summary(metaRegModel),
        cat(
          "\nNote: Estimates and confidence intervals are on the",
          scaleLabel,
          "scale."
        ),
        title = "Model Summary"
      )
    )
  }

  invisible(NULL)
}


#' Render the Bubble Plot via meta::bubble()
#'
#' Draws a bubble plot for a meta-regression model using meta's own bubble
#' function, which handles transformations, reference lines, labels, and
#' categorical covariates internally.
#'
#' Note: In the case of a factor variable with more than two groups, the
#' function generates multiple plots (one for each group against the reference).
#' Because the function prints them sequentially, the Jamovi viewport will only
#' display the last plot. However, if the plot is exported as a PDF, all
#' generated plots are fully rendered and visible.
#'
#' Additionally, for multivariable meta-regression models, it shows only one
#' plot, which is plotted against the first term (covariate) in the model while
#' adjusting for the other terms.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderBubblePlot <- function(self, key) {
  metaRegModel <- self$metaRegModels[[key]]
  options <- self$options

  if (is.null(metaRegModel)) {
    return(FALSE)
  }

  b64Map <- metaRegModel$.metajam$b64Map
  args <- list(
    x = metaRegModel,
    regline = options$bubbleRegline,
    studlab = options$bubbleStudyLabel
  )

  xlab <- bubbleB64Xlab(metaRegModel, b64Map)
  if (!is.null(xlab)) {
    args$xlab <- xlab
  }

  decodeB64Conditions(do.call(meta::bubble, args), b64Map)

  TRUE
}


#' Build a decoded x-axis label for B64 bubble plots
#'
#' Mirrors only meta::bubble.metareg's naming branches, not its plotting logic.
#' Source checkpoints in meta/R/bubble.R:
#' - lines 244-248 recover the first covariate name from formula text
#' - lines 277-285 keep factor moderator xlab empty
#' - lines 299-311 build continuous moderator xlab and multi-covariate xlab
#' - lines 497-499 draw factor level labels directly on the x-axis
#'
#' @param metaRegModel A `metareg` object.
#' @param b64Map Named character vector from `buildB64Map()`.
#' @return A decoded x-axis label, or `NULL` to keep bubble's native label.
#' @noRd
bubbleB64Xlab <- function(metaRegModel, b64Map) {
  charform <- as.character(metaRegModel$.meta$formula)[2]
  splitform <- strsplit(charform, " ")[[1]]
  covarName <- splitform[1]

  if (covarName %in% c("1", "-1")) {
    covarName <- splitform[3]
  }

  covar <- metaRegModel$.meta$x$data[[covarName]]
  if (is.character(covar) || is.factor(covar)) {
    return()
  }

  originalName <- decodeB64(covarName, b64Map)
  covarNames <- names(stats::coef(metaRegModel))
  covarNames <- covarNames[covarNames != "intrcpt"]

  if (length(covarNames) > 1) {
    return(paste0(
      "Covariate ",
      originalName,
      " (meta-regression: ",
      decodeB64(charform, b64Map),
      ")"
    ))
  }

  paste("Covariate", originalName)
}


#' Get Scale Label for Meta-Regression Output
#'
#' Returns a human-readable label describing the scale of the regression
#' estimates (e.g., "Log Odds Ratio", "Standardised Mean Difference"). Works
#' with any standard meta analysis type (metacont, metabin, metaprop, metarate,
#' metainc, etc.).
#'
#' Since backtransf is always FALSE, xlab_meta never returns "".
#'
#' NOTE: For future metagen() support with custom transforms, pass func.transf /
#' func.backtransf to xlab_meta. See meta:::xlab_meta in meta/R/meta-xlab.R
#' L150-161 and bubble.R L349-362 for the empty-label fallback.
#'
#' @param metaRegModel A `metareg` object.
#' @return A character string label.
#' @noRd
getMetaRegScaleLabel <- function(metaRegModel) {
  meta:::xlab_meta(metaRegModel$.meta$x$sm, backtransf = FALSE)
}
