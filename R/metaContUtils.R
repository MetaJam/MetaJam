#' Build Common metacont() Arguments
#'
#' Loads data from the analysis object, curates numeric columns, and
#' returns the argument list ready for `meta::metacont()`. Shared by
#' `computeContModel()` and `computeContSubgroupModel()`.
#'
#' Passing `data=` ensures that `model$data` retains all original
#' columns — downstream consumers (`computeMetaRegModel`) can read
#' moderator columns directly from `model$data`.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @return A named list of arguments for `meta::metacont()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildContArgs <- function(analysis) {
  options <- analysis$options
  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")

  if (!hasRequiredVars(options, required)) {
    return()
  }

  # Data
  data <- analysis$data

  # Curate numeric columns: core vars + moderator covariates
  # (options$metaRegCovs is NULL when unset — c() drops NULL)
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

  args
}


#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Builds the shared argument list via `buildContArgs()` (with moderator
#' covariates included) and calls `meta::metacont()`.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(analysis) {
  args <- buildContArgs(analysis)
  if (is.null(args)) {
    return()
  }

  do.call(meta::metacont, args)
}


#' Compute a Continuous Outcome Subgroup Meta-Analysis Model
#'
#' Builds the shared argument list via `buildContArgs()` and adds
#' subgroup-specific arguments. Calls `meta::metacont()` directly
#' with `subgroup=` — completely independent of the main model,
#' avoiding the cost of `update.meta()`.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @return A `meta::metacont` object with subgroup results, or `NULL`.
#' @noRd
computeContSubgroupModel <- function(analysis) {
  if (is.null(analysis$options$subgroupVariable)) {
    return()
  }

  args <- buildContArgs(analysis)
  if (is.null(args)) {
    return()
  }

  args$subgroup <- as.name(analysis$options$subgroupVariable)
  args$tau.common <- analysis$options$tauCommon
  args$prediction.subgroup <- analysis$options$predictionSubgroup

  do.call(meta::metacont, args)
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
      label.e = options$labelE,
      label.c = options$labelC,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center"
    )
  } else {
    renderForest(
      model,
      options,
      label.e = options$labelE,
      label.c = options$labelC
    )
  }
}


#' Render a Metacont Subgroup Forest Plot
#'
#' Adds metacont-specific column label attachments (so the group header
#' spans the Mean / SD / N columns) and delegates to
#' `renderSubgroupForest()`.
#'
#' @param model A `metacont` object with subgroup results.
#' @param options A Jamovi options object with `subgroup*` fields.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderContSubgroupForest <- function(model, options) {
  if (options$subgroupForestLayout %in% c("meta", "RevMan5")) {
    renderSubgroupForest(
      model,
      options,
      label.e = options$subgroupLabelE,
      label.c = options$subgroupLabelC,
      label.e.attach = c("n.e", "mean.e", "sd.e"),
      label.c.attach = c("n.c", "mean.c", "sd.c"),
      just.label.e = "center",
      just.label.c = "center"
    )
  } else {
    renderSubgroupForest(
      model,
      options,
      label.e = options$subgroupLabelE,
      label.c = options$subgroupLabelC
    )
  }
}