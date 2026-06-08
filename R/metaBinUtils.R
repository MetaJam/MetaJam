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
  allstudies <- options$allstudies

  if (identical(options$method, "Peto")) {
    # meta still reads allstudies for Peto. If TRUE, non-informative
    # rows can be carried further internally (e.g. undefined SEs become
    # Inf), but they still get zero useful weight and the pooled Peto
    # result is unchanged. For Peto this option should be FALSE. Since
    # we disable it in the UI but meta does not fully disable it
    # internally, force it off here; otherwise meta gives a warning
    # when the stored value is TRUE.
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
    incr = options$incr,
    method.incr = options$correctionMethod,
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
