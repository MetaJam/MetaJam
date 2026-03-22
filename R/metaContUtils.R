#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Self-contained helper that loads data from the analysis object,
#' curates all numeric columns (core + moderator covariates), and
#' creates a `meta::metacont` object.
#'
#' Passing `data=` ensures that `model$data` retains all original
#' columns — downstream consumers (`computeSubgroupModel`,
#' `computeMetaRegModel`) can read moderator and subgroup columns
#' directly from `model$data`.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(analysis) {
  options <- analysis$options
  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")

  if (!hasRequiredVars(options, required)) {
    return()
  }

  # Load data (inlined from former processData)
  data <- analysis$data
  if (is.null(data) || nrow(data) == 0) {
    data <- analysis$readDataset()
  }

  # Curate numeric columns: core vars + moderator covariates
  numericVars <- c(
    options$meanE,
    options$sdE,
    options$nE,
    options$meanC,
    options$sdC,
    options$nC,
    options$metaRegCovs
  )
  for (var in numericVars) {
    data[[var]] <- jmvcore::toNumeric(data[[var]])
  }

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  # Build metacont() call with column names as symbols via do.call()
  args <- list(
    n.e = as.name(options$nE),
    mean.e = as.name(options$meanE),
    sd.e = as.name(options$sdE),
    n.c = as.name(options$nC),
    mean.c = as.name(options$meanC),
    sd.c = as.name(options$sdC),
    data = data,
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
    args$studlab <- as.name(options$studyLabel)
  }

  do.call(meta::metacont, args)
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