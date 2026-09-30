#' Compute a Correlations Meta-Analysis Model
#'
#' Builds the shared argument list via `buildCorArgs()` and calls
#' `meta::metacor()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metacor` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeCorModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildCorArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metacor, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Compute Correlations Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metacor()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metacor` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeCorSubgroupModels <- function(self) {
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
  # model with NULL data via buildCorArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildCorArgs() checks if data is NULL and safely aborts, ensuring the true
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

  args <- buildCorArgs(self)
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

    models[[i]] <- do.call(meta::metacor, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metacor-Specific Forest Plot
#'
#' Delegates to `renderForest()`. Correlation models display one
#' Correlation / Total group per study and require no two-group header
#' attachments.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderCorForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  renderForest(
    model,
    options,
    sortKey = sortKey
  )

  TRUE
}


#' Render a Metacor Subgroup Forest Plot
#'
#' Delegates to `renderSubgroupForest()`. Correlation models display one
#' Correlation / Total group per study and require no two-group header
#' attachments.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderCorSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  renderSubgroupForest(
    model,
    options,
    sortKey = sortKey
  )

  TRUE
}


#' Build Common metacor() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metacor()`. Shared by
#' `computeCorModel()` and `computeCorSubgroupModels()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metacor()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildCorArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("cor", "n")

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
  numericVars <- c(options$cor, options$n)
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  args <- list(
    cor = data[[options$cor]],
    n = data[[options$n]],
    sm = options$sm,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    method.tau = options$methodTau,
    method.random.ci = options$methodRandomCi,
    prediction = options$prediction && options$model %in% c("both", "random"),
    # When 'Test against null value' is checked, pass the null value; otherwise
    # pass NA to skip the test in metacor()
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
