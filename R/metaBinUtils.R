#' Compute a Binary Outcome Meta-Analysis Model
#'
#' Builds the shared argument list via `buildBinArgs()` and calls
#' `meta::metabin()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metabin` object, or `NULL` if required columns are missing.
#' @noRd
computeBinModel <- function(self) {
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildBinArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metabin, args)
  model <- stripModel(model)

  self$results$text$setState(model)
  model
}


#' Compute Binary Outcome Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metabin()` with `subgroup=`.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metabin` objects with subgroup results, or `NULL`
#'   if no subgroup variables are assigned.
#' @noRd
computeBinSubgroupModels <- function(self) {
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
  # model with NULL data via buildBinArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildBinArgs() checks if data is NULL and safely aborts, ensuring the true
  # error is shown.
  for (i in seq_along(vars)) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

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

  args <- buildBinArgs(self)
  if (is.null(args)) {
    return(models)
  }

  args$tau.common <- self$options$tauCommon
  args$prediction.subgroup <- self$options$predictionSubgroup

  for (i in missing) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

    args$subgroup <- as.name(vars[[i]])

    models[[i]] <- do.call(meta::metabin, args)
    models[[i]] <- stripModel(models[[i]])

    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metabin-Specific Forest Plot
#'
#' Adds binary-outcome column label attachments so the group header spans the
#' Events / Total columns and delegates to `renderForest()`.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderBinForest <- function(self, sortKey) {
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
      label.e.attach = c("event.e", "n.e"),
      label.c.attach = c("event.c", "n.c"),
      just.label.e = "center",
      just.label.c = "center"
    )
  } else {
    renderForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$labelE,
      label.c = options$labelC
    )
  }

  TRUE
}


#' Render a Metabin Subgroup Forest Plot
#'
#' Adds binary-outcome column label attachments and delegates to
#' `renderSubgroupForest()`.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g. `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderBinSubgroupForest <- function(self, key, sortKey) {
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
      label.e.attach = c("event.e", "n.e"),
      label.c.attach = c("event.c", "n.c"),
      just.label.e = "center",
      just.label.c = "center"
    )
  } else {
    renderSubgroupForest(
      model,
      options,
      sortKey = sortKey,
      label.e = options$subgroupLabelE,
      label.c = options$subgroupLabelC
    )
  }

  TRUE
}


#' Build Common metabin() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and returns the
#' argument list ready for `meta::metabin()`.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metabin()`, or `NULL` if
#'   required columns are missing.
#' @noRd
buildBinArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("eventE", "nE", "eventC", "nC")

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

  numericVars <- c(
    options$eventE,
    options$nE,
    options$eventC,
    options$nC
  )
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  level <- options$confidenceLevel / 100
  incr <- options$incr
  method.incr <- options$correctionMethod

  if (method.incr == "none") {
    incr <- 0
    method.incr <- "only0"
  }

  allstudies <- options$allstudies

  if (options$method == "Peto" || incr == 0) {
    # meta still reads allstudies for Peto. If TRUE, non-informative
    # rows can be carried further internally (e.g. undefined SEs become
    # Inf), but they still get zero useful weight and the pooled Peto
    # result is unchanged. For Peto this option should be FALSE. Since
    # we disable it in the UI but meta does not fully disable it
    # internally, force it off here; otherwise meta gives a warning
    # when the stored value is TRUE.
    #
    # The same truthfulness issue happens when incr is zero. In meta::metabin(),
    # allstudies=TRUE first marks double-zero / all-event studies as included,
    # but with no continuity correction their effect size or standard error is
    # still undefined. They then receive zero weight and do not increase k,
    # while the returned object still stores allstudies=TRUE. Force FALSE so
    # the stored model matches what actually contributed to the estimate.
    allstudies <- FALSE
  }

  args <- list(
    event.e = as.name(options$eventE),
    n.e = as.name(options$nE),
    event.c = as.name(options$eventC),
    n.c = as.name(options$nC),
    data = data,
    sm = options$sm,
    method = options$method,
    incr = incr,
    method.incr = method.incr,
    allstudies = allstudies,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    prediction = options$prediction,
    level = level,
    level.ma = level,
    level.predict = level,
    method.tau = options$methodTau,
    method.random.ci = options$methodRandomCi
  )

  if (!is.null(options$studyLabel)) {
    args$studlab <- as.name(options$studyLabel)
  }

  args
}
