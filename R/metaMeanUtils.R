#' Compute a Single Means Meta-Analysis Model
#'
#' Builds the shared argument list via `buildMeanArgs()` and calls
#' `meta::metamean()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metamean` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeMeanModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildMeanArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metamean, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Compute Single Means Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metamean()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metamean` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeMeanSubgroupModels <- function(self) {
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
  # model with NULL data via buildMeanArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildMeanArgs() checks if data is NULL and safely aborts, ensuring the true
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

  args <- buildMeanArgs(self)
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

    models[[i]] <- do.call(meta::metamean, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metamean-Specific Forest Plot
#'
#' Adds metamean-specific rounding arguments for the Mean and SD columns and
#' delegates to `renderForest()`. Unlike a two-group metacont forest, a
#' single-group mean forest has no Experimental / Control column headers to
#' attach: the Mean / SD / Total columns describe the one observed group.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderMeanForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  renderForest(
    model,
    options,
    sortKey = sortKey,
    digits.mean = as.integer(options$digitsMean),
    digits.sd = as.integer(options$digitsSd)
  )

  TRUE
}


#' Render a Metamean Subgroup Forest Plot
#'
#' Adds metamean-specific rounding arguments for the Mean and SD columns and
#' delegates to `renderSubgroupForest()`. Unlike a two-group metacont forest,
#' no Experimental / Control column headers are supplied because every study
#' contributes one Mean / SD / Total set.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderMeanSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  renderSubgroupForest(
    model,
    options,
    sortKey = sortKey,
    digits.mean = as.integer(options$subgroupDigitsMean),
    digits.sd = as.integer(options$subgroupDigitsSd)
  )

  TRUE
}


#' Build Common metamean() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metamean()`. Shared by
#' `computeMeanModel()` and `computeMeanSubgroupModels()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metamean()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildMeanArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("mean", "sd", "n")

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

  # Curate numeric columns: core vars
  numericVars <- c(options$mean, options$sd, options$n)
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  args <- list(
    n = data[[options$n]],
    mean = data[[options$mean]],
    sd = data[[options$sd]],
    sm = options$sm,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    method.tau = options$methodTau,
    method.random.ci = options$methodRandomCi,
    prediction = options$prediction && options$model %in% c("both", "random"),
    # When 'Test against null value' is checked, pass the null value; otherwise
    # pass NA to skip the test in metamean()
    null.effect = if (options$nullEffectEnabled) {
      options$nullEffect
    } else {
      NA_real_
    },
    level = level,
    level.ma = level,
    level.predict = level,
    level.hetstat = level
  )

  if (!is.null(options$studyLabel)) {
    args$studlab <- data[[options$studyLabel]]
  }

  args
}
