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

  args <- buildContArgs(self)
  if (is.null(args)) {
    return()
  }

  modelsArray <- self$results$subgroupModels
  args$tau.common <- self$options$tauCommon
  args$prediction.subgroup <- self$options$predictionSubgroup

  models <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    cacheElement <- modelsArray$get(key = i)$subgroupText

    # Cross-cycle cache (restored via clearWith)
    cached <- cacheElement$state
    if (!is.null(cached)) {
      models[[i]] <- cached
      next
    }

    args$subgroup <- as.name(vars[[i]])

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
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderContForest <- function(self) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

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
      digits.mean = as.integer(options$digitsMean),
      digits.sd = as.integer(options$digitsSd)
    )
  } else {
    renderForest(
      model,
      options,
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
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderContSubgroupForest <- function(self, key) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  if (options$subgroupForestLayout %in% c("meta", "RevMan5")) {
    renderSubgroupForest(
      model,
      options,
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
#' Passing `data=` ensures that `model$data` retains all original
#' columns — downstream consumers (`computeMetaRegModel`) can read
#' moderator columns directly from `model$data`.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metacont()`, or `NULL`
#'   if required columns are missing.
#' @noRd
buildContArgs <- function(self) {
  data <- self$data
  options <- self$options
  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")

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
