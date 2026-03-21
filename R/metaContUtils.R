#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Self-contained helper that guards for required options, loads data,
#' and creates a `meta::metacont` object. Designed to be called directly
#' from the active binding in the `.b.R` class.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(analysis) {
  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
  options <- analysis$options
  data <- analysis$data

  if (!hasRequiredVars(options, required)) {
    return()
  }

  if (is.null(data) || nrow(data) == 0) {
    data <- analysis$readDataset()
  }

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
    data = data,
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


#' Build Subgroup Forest Options with Metacont Labels
#'
#' Wraps `buildSubgroupForestOptions()` and injects the Experimental/Control
#' group labels specific to continuous outcome analyses.
#'
#' @param options A Jamovi options object.
#' @return A named list ready for `renderContForest()`.
#' @noRd
buildContSubgroupForestOptions <- function(options) {
  opts <- buildSubgroupForestOptions(options)
  opts$labelE <- options$subgroupLabelE
  opts$labelC <- options$subgroupLabelC
  opts
}


#' Render a Metacont-Specific Forest Plot
#'
#' Adds metacont-specific column label attachments (so the group header
#' spans the Mean / SD / N columns) and delegates to `renderForest()`.
#'
#' @param model A `metacont` object.
#' @param options A Jamovi options object.
#' @param ... Additional arguments forwarded to `renderForest()` and
#'   ultimately `meta::forest()`. Used by subgroup plots to pass
#'   `test.effect.subgroup` and `print.subgroup.name`.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderContForest <- function(model, options, ...) {
  if (options$forestLayout %in% c("meta", "RevMan5")) {
    renderForest(
      model,
      options,
      label.e = options$labelE,
      label.c = options$labelC,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center",
      ...
    )
  } else {
    renderForest(
      model,
      options,
      label.e = options$labelE,
      label.c = options$labelC,
      ...
    )
  }
}