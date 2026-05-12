#' Initialize Meta-Regression Models
#'
#' Adds one group per block to the `metaRegModels` Array and sets
#' dynamic Group titles. When required variables are missing or a
#' block is empty, a title-only placeholder is set on the text
#' element (same pattern as `initSubgroupText()`).
#'
#' @param modelsArray The `metaRegModels` Array result element.
#' @param options The `self$options` object.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @noRd
initMetaRegModels <- function(modelsArray, options, requiredVars) {
  blocks <- options$metaRegBlocks
  hasVars <- hasRequiredVars(options, requiredVars)

  for (i in seq_along(blocks)) {
    modelsArray$addItem(key = i)
    group <- modelsArray$get(key = i)
    terms <- blocks[[i]]

    # Build suffix: " (Model 1)" or " (Model 1: ~ age + sex)"
    if (length(terms) == 0) {
      suffix <- paste0(" (Model ", i, ")")
    } else {
      termStrs <- vapply(
        terms,
        function(t) jmvcore::stringifyTerm(t, raise = TRUE),
        character(1)
      )
      suffix <- paste0(
        " (Model ",
        i,
        ": ~ ",
        paste(termStrs, collapse = " + "),
        ")"
      )
    }

    group$setTitle(paste0("Meta-Regression", suffix))

    # Set text placeholder with title when vars missing or block empty
    textResult <- group$metaRegText
    if (textResult$visible && (!hasVars || length(terms) == 0)) {
      textResult$setContent(asHtml(title = "Model Summary"))
    }
  }
}


#' Compute Meta-Regression Models for All Blocks
#'
#' Iterates over `options$metaRegBlocks`, building a formula for each
#' block and calling `meta::metareg()`. Returns a list of models
#' (NULL entries for empty blocks).
#'
#' @param data The raw data from jamovi (`self$data`).
#' @param model A `meta` object (must have been created with `data=`).
#' @param options The jamovi options object.
#' @return A list of `metareg` objects (NULL entries for empty blocks).
#' @noRd
computeMetaRegModels <- function(data, model, options) {
  if (is.null(model)) {
    return(list())
  }

  # Ensure meta regression covariates are numeric before appending
  data[options$metaRegCovs] <- lapply(
    data[options$metaRegCovs],
    jmvcore::toNumeric
  )

  # Safely append missing columns from Jamovi data into model data
  # We give priority to meta's internal columns (.exclude, .subgroup, etc.)
  # TODO: We may use base64 encoding for user column names in the future to
  # completely avoid any collisions with `meta`'s internal dot-prefixed columns.
  missing_cols <- setdiff(names(data), names(model$data))
  if (length(missing_cols) > 0) {
    model$data[missing_cols] <- data[missing_cols]
  }

  blocks <- options$metaRegBlocks
  models <- vector("list", length(blocks))

  for (i in seq_along(blocks)) {
    terms <- blocks[[i]]
    if (length(terms) == 0) {
      next
    }

    composed <- jmvcore::composeTerms(terms)
    formula <- as.formula(paste("~", paste(composed, collapse = " + ")))
    models[[i]] <- meta::metareg(
      model,
      formula,
      intercept = options$metaRegIntercept
    )
  }

  models
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


#' Populate Meta-Regression Text for All Models
#'
#' Called from `.run()` when meta-regression models are available.
#'
#' @param modelsArray The `metaRegModels` Array result element.
#' @param metaRegModels A list of `metareg` objects.
#' @param options The `self$options` object.
#' @noRd
populateMetaRegTexts <- function(modelsArray, metaRegModels, options) {
  for (i in seq_along(metaRegModels)) {
    metaRegModel <- metaRegModels[[i]]
    if (is.null(metaRegModel)) {
      next
    }

    group <- modelsArray$get(key = i)
    textResult <- group$metaRegText
    if (!textResult$visible) {
      next
    }

    scaleLabel <- getMetaRegScaleLabel(metaRegModel)
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
}


#' Render the Bubble Plot via meta::bubble()
#'
#' Draws a bubble plot for a meta-regression model using meta's own bubble
#' function, which handles transformations, reference lines, labels, and
#' categorical covariates internally.
#'
#' Note:  In case of multiple groups in the factor variable, only the last one
#' shows as it show one for every group against the reference. Try to see what
#' we gonna do with this the same for multivariables.
#'
#' @param metaRegModel A `metareg` object from image$state.
#' @param options The `self$options` object.
#' @noRd
renderBubblePlot <- function(metaRegModel, options) {
  meta::bubble(
    metaRegModel,
    regline = options$bubbleRegline,
    studlab = options$bubbleStudyLabel
  )
}