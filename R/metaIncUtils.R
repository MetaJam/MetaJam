#' Compute an Incidence Rate Outcome Meta-Analysis Model
#'
#' Builds the shared argument list via `buildIncArgs()` and calls
#' `meta::metainc()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metainc` object, or `NULL` if required columns are missing.
#' @noRd
computeIncModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildIncArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metainc, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Compute Incidence Rate Outcome Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metainc()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metainc` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeIncSubgroupModels <- function(self) {
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
  # model with NULL data via buildIncArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildIncArgs() checks if data is NULL and safely aborts, ensuring the true
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

  args <- buildIncArgs(self)
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

    models[[i]] <- do.call(meta::metainc, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metainc-Specific Forest Plot
#'
#' Adds metainc-specific column label attachments (so the group header
#' spans the Events / Person-Time columns) and delegates to `renderForest()`.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderIncForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  args <- list(
    model = model,
    options = options,
    sortKey = sortKey,
    label.e = options$labelE,
    label.c = options$labelC
  )

  # Omitting digits.time preserves meta::forest()'s native automatic rule:
  # whole-number person-times use zero decimal places, while fractional
  # person-times inherit the forest effect-size rounding value, which defaults
  # to two decimal places.
  if (options$digitsTime != "auto") {
    args$digits.time <- as.integer(options$digitsTime)
  }

  if (options$forestLayout %in% c("meta", "RevMan5")) {
    args <- c(
      args,
      list(
        label.e.attach = c("event.e", "time.e"),
        label.c.attach = c("event.c", "time.c"),
        just.label.e = "center",
        just.label.c = "center"
      )
    )
  }

  do.call(renderForest, args)

  TRUE
}


#' Render a Metainc Subgroup Forest Plot
#'
#' Adds metainc-specific column label attachments (so the group header
#' spans the Events / Person-Time columns) and delegates to
#' `renderSubgroupForest()`.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderIncSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  args <- list(
    model = model,
    options = options,
    sortKey = sortKey,
    label.e = options$subgroupLabelE,
    label.c = options$subgroupLabelC
  )

  # As in the main forest plot, Auto is implemented by omitting digits.time so
  # meta::forest() can choose zero decimals for whole-number person-times and
  # inherit the forest effect-size rounding for fractional person-times, which
  # defaults to two decimal places.
  if (options$subgroupDigitsTime != "auto") {
    args$digits.time <- as.integer(options$subgroupDigitsTime)
  }

  if (options$subgroupForestLayout %in% c("meta", "RevMan5")) {
    args <- c(
      args,
      list(
        label.e.attach = c("event.e", "time.e"),
        label.c.attach = c("event.c", "time.c"),
        just.label.e = "center",
        just.label.c = "center"
      )
    )
  }

  do.call(renderSubgroupForest, args)

  TRUE
}


#' Build Common metainc() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metainc()`. Shared by
#' `computeIncModel()` and `computeIncSubgroupModels()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metainc()`, or `NULL` if
#'   required columns are missing.
#' @noRd
buildIncArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("eventE", "timeE", "eventC", "timeC")

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
    options$eventE,
    options$timeE,
    options$eventC,
    options$timeC
  )
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100
  incr <- options$incr
  method.incr <- options$correctionMethod

  if (method.incr == "none") {
    incr <- 0
    method.incr <- "only0"
  }

  args <- list(
    event.e = data[[options$eventE]],
    time.e = data[[options$timeE]],
    event.c = data[[options$eventC]],
    time.c = data[[options$timeC]],
    sm = options$sm,
    method = options$method,
    incr = incr,
    method.incr = method.incr,
    common = options$model %in% c("both", "common"),
    random = options$model %in% c("both", "random"),
    method.tau = options$methodTau,
    method.random.ci = options$methodRandomCi,
    prediction = options$prediction && options$model %in% c("both", "random"),
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


#' Warn About Random-Effects Method Choices
#'
#' Emits a runtime warning when the Mantel-Haenszel method is combined with a
#' random effects model, explaining how the random effects estimate was
#' actually computed.
#'
#' @param options The `self$options` object from a jamovi analysis.
#' @return `NULL` invisibly. Called for warning side effects only.
#' @noRd
warnIncMethodForRandom <- function(options) {
  if (!(options$model %in% c("both", "random"))) {
    return(invisible(NULL))
  }

  if (options$method == "MH") {
    warning(
      "The Mantel-Haenszel method is available only for the common effect ",
      "model; the inverse variance method was therefore used for the random ",
      "effects model.",
      call. = FALSE
    )
  }

  invisible(NULL)
}
