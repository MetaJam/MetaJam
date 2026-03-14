#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Standalone helper that creates a `meta::metacont` object from Jamovi
#' options. Designed to be reused across multiple analysis classes.
#'
#' @param data A data frame (typically `self$data`).
#' @param options A Jamovi options object with the required fields.
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(data, options) {
  # Guard: all six required columns must be assigned

  # Extract and convert columns
  mean.e <- jmvcore::toNumeric(data[[options$meanE]])
  sd.e <- jmvcore::toNumeric(data[[options$sdE]])
  n.e <- jmvcore::toNumeric(data[[options$nE]])
  mean.c <- jmvcore::toNumeric(data[[options$meanC]])
  sd.c <- jmvcore::toNumeric(data[[options$sdC]])
  n.c <- jmvcore::toNumeric(data[[options$nC]])

  # Optional study labels
  studlab <- NULL
  if (!is.null(options$studyLabel)) {
    studlab <- data[[options$studyLabel]]
  }

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  # Fit model
  meta::metacont(
    n.e = n.e,
    mean.e = mean.e,
    sd.e = sd.e,
    n.c = n.c,
    mean.c = mean.c,
    sd.c = sd.c,
    studlab = studlab,
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
}


#' Render a Metacont-Specific Forest Plot
#'
#' Adds metacont-specific column label attachments (so the group header
#' spans the Mean / SD / N columns) and delegates to `renderForest()`.
#'
#' @param model A `metacont` object.
#' @param options A Jamovi options object.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderContForest <- function(model, options) {
  if (options$forestLayout %in% c("meta", "RevMan5")) {
    renderForest(
      model,
      options,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center"
    )
  } else {
    renderForest(model, options)
  }
}