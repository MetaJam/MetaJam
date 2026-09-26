#' Compute a Single Incidence Rates Meta-Analysis Model
#'
#' Builds the shared argument list via `buildRateArgs()` and calls
#' `meta::metarate()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metarate` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeRateModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildRateArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metarate, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Compute Single Incidence Rates Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metarate()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metarate` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeRateSubgroupModels <- function(self) {
  vars <- self$options$subgroupVariables
  if (length(vars) == 0) {
    return()
  }

  modelsArray <- self$results$subgroupModels
  models <- vector("list", length(vars))
  missing <- integer()

  # We must restore cached subgroup models BEFORE entering the calculation path.
  # If a model is missing from the cache during later lifecycle phases (e.g.,
  # image rendering or save/export), it means its calculation failed during the
  # .run() phase and threw an error. In these later phases, jamovi clears
  # self$data and it becomes NULL. If we attempted to recalculate the missing
  # model with NULL data via buildRateArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildRateArgs() checks if data is NULL and safely aborts, ensuring the true
  # error is shown.
  for (i in seq_along(vars)) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

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

  args <- buildRateArgs(self)
  if (is.null(args)) {
    return(models)
  }

  args$tau.common <- self$options$tauCommon
  args$prediction.subgroup <- self$options$predictionSubgroup &&
    self$options$model %in% c("both", "random")
  # Subgroup models are only printed/plotted, so avoid caching their data.
  args$keepdata <- FALSE

  for (i in missing) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

    args$subgroup <- self$data[[vars[[i]]]]
    args$subgroup.name <- vars[[i]]

    models[[i]] <- do.call(meta::metarate, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metarate-Specific Forest Plot
#'
#' Delegates to `renderForest()`. Single-rate models display one
#' Events / Person-Time group per study and require no two-group header
#' attachments.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderRateForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  args <- list(
    model = model,
    options = options,
    sortKey = sortKey
  )

  # Omitting digits.time preserves meta::forest()'s native automatic rule:
  # whole-number person-times use zero decimal places, while fractional
  # person-times inherit the forest effect-size rounding value, which defaults
  # to two decimal places.
  if (options$digitsTime != "auto") {
    args$digits.time <- as.integer(options$digitsTime)
  }

  do.call(renderForest, args)

  TRUE
}


#' Render a Metarate Subgroup Forest Plot
#'
#' Delegates to `renderSubgroupForest()`. Single-rate models display one
#' Events / Person-Time group per study and require no two-group header
#' attachments.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderRateSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  args <- list(
    model = model,
    options = options,
    sortKey = sortKey
  )

  # As in the main forest plot, Auto is implemented by omitting digits.time so
  # meta::forest() can choose zero decimals for whole-number person-times and
  # inherit the forest effect-size rounding for fractional person-times, which
  # defaults to two decimal places.
  if (options$subgroupDigitsTime != "auto") {
    args$digits.time <- as.integer(options$subgroupDigitsTime)
  }

  do.call(renderSubgroupForest, args)

  TRUE
}


#' Build Common metarate() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metarate()`. Shared by
#' `computeRateModel()` and `computeRateSubgroupModels()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metarate()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildRateArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("event", "time")

  # jamovi lifecycle guard: A user in jamovi cannot pass NULL data; during a
  # normal .run() cycle, jamovi always provides a data.frame (with at least one
  # row). The ONLY time self$data is NULL is during later internal phases like
  # image rendering or save/export, when jamovi actively clears it. In these
  # later stages, we rely purely on cached models. If a model is missing from
  # the cache, it means an error occurred during the .run() phase. We do not
  # need to calculate it again. Furthermore, we cannot calculate it anyway
  # because using NULL data would crash with a new, confusing error. Returning
  # NULL here safely aborts the attempt and preserves the original .run() error.
  if (is.null(data)) {
    return()
  }

  if (!hasRequiredVars(options, required)) {
    return()
  }

  if (options$method == "GLMM") {
    # jamovi applies jmvcore::validateSafeFormula(), available from our minimum
    # supported version (2.7.27) onward, to formulas used by modules. This
    # affects MetaJam because metafor's GLMM formula uses rep():
    # https://github.com/jamovi/jamovi/issues/1857
    # Remove this check when minApp is raised to 28.3.0 after that release
    # becomes Solid.
    supportsGLMM <- tryCatch(
      {
        jmvcore::validateSafeFormula(rep(0, k) ~ 1)
        TRUE
      },
      error = function(e) FALSE
    )

    if (!supportsGLMM) {
      jmvcore::reject(
        "\"Generalised linear mixed model (GLMM)\" requires jamovi version 28.3.0 or later. Update jamovi or change \"Method\" to \"Inverse variance\"." # nolint
      )
    }
  }

  if (options$method == "GLMM" && options$sm != "IRLN") {
    jmvcore::reject(
      "\"Generalised linear mixed model (GLMM)\" can only be used when \"Transformation\" is \"Log\". Change \"Transformation\" or \"Method\"." # nolint
    )
  }

  if (options$method == "GLMM" && !(options$methodTau %in% c("auto", "ML"))) {
    jmvcore::reject(
      "\"Generalised linear mixed model (GLMM)\" can only be used when \"Heterogeneity estimator\" is \"Auto (based on Method)\" or \"Maximum-likelihood\". Change \"Heterogeneity estimator\" or \"Method\"." # nolint
    )
  }

  # Curate numeric columns: core vars
  numericVars <- c(options$event, options$time)
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  level <- options$confidenceLevel / 100
  incr <- options$incr
  method.incr <- options$correctionMethod

  # IRS and IRFT do not use continuity corrections. Explicitly zeroing incr
  # aligns with the disabled UI and prevents meta from falsely reporting
  # in the "Details" section of the output that a continuity correction
  # was applied.
  if (options$sm %in% c("IRS", "IRFT") || method.incr == "none") {
    incr <- 0
    method.incr <- "only0"
  }

  args <- list(
    event = data[[options$event]],
    time = data[[options$time]],
    sm = options$sm,
    method = options$method,
    incr = incr,
    method.incr = method.incr,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    method.random.ci = options$methodRandomCi,
    prediction = options$prediction && options$model %in% c("both", "random"),
    # When 'Test against null value' is checked, pass the null value; otherwise
    # pass NA to skip the test in metarate()
    null.effect = if (options$nullEffectEnabled) {
      options$nullEffect
    } else {
      NA_real_
    },
    level = level,
    level.ma = level,
    level.predict = level,
    level.hetstat = level,
    irscale = if (options$irscaleEnabled) options$irscale else 1,
    irunit = options$irunit
  )

  # When methodTau is "auto", omit method.tau so meta defaults to ML for GLMM
  # and the package default (REML) for inverse-variance models.
  if (options$methodTau != "auto") {
    args$method.tau <- options$methodTau
  }

  if (!is.null(options$studyLabel)) {
    args$studlab <- data[[options$studyLabel]]
  }

  args
}
