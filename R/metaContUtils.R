#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Builds the shared argument list via `buildContArgs()` (with moderator
#' covariates included) and calls `meta::metacont()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildContArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metacont, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Compute Continuous Outcome Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metacont()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metacont` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeContSubgroupModels <- function(self) {
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
  # model with NULL data via buildContArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildContArgs() checks if data is NULL and safely aborts, ensuring the true
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

  args <- buildContArgs(self)
  if (is.null(args)) {
    return(models)
  }

  args$tau.common <- self$options$tauCommon
  args$prediction.subgroup <- self$options$predictionSubgroup
  # Subgroup models are only printed/plotted, so avoid caching their data.
  args$keepdata <- FALSE

  for (i in missing) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

    args$subgroup <- self$data[[vars[[i]]]]
    args$subgroup.name <- vars[[i]]

    models[[i]] <- do.call(meta::metacont, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metacont-Specific Forest Plot
#'
#' Adds metacont-specific column label attachments (so the group header
#' spans the Mean / SD / N columns) and delegates to `renderForest()`.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderContForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  if (options$forestLayout %in% c("meta", "RevMan5")) {
    renderForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$labelE,
      label.c = options$labelC,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center",
      digits.mean = as.integer(options$digitsMean),
      digits.sd = as.integer(options$digitsSd)
    )
  } else {
    renderForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$labelE,
      label.c = options$labelC,
      digits.mean = as.integer(options$digitsMean),
      digits.sd = as.integer(options$digitsSd)
    )
  }

  TRUE
}


#' Render a Metacont Subgroup Forest Plot
#'
#' Adds metacont-specific column label attachments (so the group header
#' spans the Mean / SD / N columns) and delegates to
#' `renderSubgroupForest()`.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderContSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  if (options$subgroupForestLayout %in% c("meta", "RevMan5")) {
    renderSubgroupForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$subgroupLabelE,
      label.c = options$subgroupLabelC,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center",
      digits.mean = as.integer(options$subgroupDigitsMean),
      digits.sd = as.integer(options$subgroupDigitsSd)
    )
  } else {
    renderSubgroupForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$subgroupLabelE,
      label.c = options$subgroupLabelC,
      digits.mean = as.integer(options$subgroupDigitsMean),
      digits.sd = as.integer(options$subgroupDigitsSd)
    )
  }

  TRUE
}


#' Build Common metacont() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metacont()`. Shared by
#' `computeContModel()` and `computeContSubgroupModel()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metacont()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildContArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")

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
  numericVars <- c(
    options$meanE,
    options$sdE,
    options$nE,
    options$meanC,
    options$sdC,
    options$nC
  )
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  args <- list(
    n.e = data[[options$nE]],
    mean.e = data[[options$meanE]],
    sd.e = data[[options$sdE]],
    n.c = data[[options$nC]],
    mean.c = data[[options$meanC]],
    sd.c = data[[options$sdC]],
    sm = options$sm,
    method.tau = options$methodTau,
    method.smd = options$methodSmd,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    prediction = options$prediction,
    level = level,
    level.ma = level,
    level.predict = level,
    method.random.ci = options$methodRandomCi
  )

  if (!is.null(options$studyLabel)) {
    args$studlab <- data[[options$studyLabel]]
  }

  args
}
