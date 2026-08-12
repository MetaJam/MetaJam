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


#' Build Common metainc() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and returns the
#' argument list ready for `meta::metainc()`.
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
