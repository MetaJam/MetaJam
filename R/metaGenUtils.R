#' Compute a Precomputed Effect Sizes Meta-Analysis Model
#'
#' Builds the argument list with `buildGenArgs()` and calls `meta::metagen()`.
#'
#' @param self The jamovi `self` object.
#' @return A `meta::metagen` object, or `NULL` if required columns are missing.
#' @noRd
computeGenModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$text$state
  if (!is.null(cached)) {
    return(cached)
  }

  args <- buildGenArgs(self)
  if (is.null(args)) {
    return()
  }

  model <- do.call(meta::metagen, args)
  model <- stripModel(model)

  # Cache for next cycle
  self$results$text$setState(model)
  model
}


#' Build Common metagen() Arguments
#'
#' Loads the selected columns, converts numeric inputs safely, and returns an
#' argument list for `meta::metagen()`. In SE mode, effects and standard errors
#' are supplied on the scale used for pooling. In CI mode, effects and limits
#' are supplied on the reported scale and transformed by `meta::metagen()`.
#'
#' Core study data are passed as vectors rather than via `data=` so cached
#' meta objects do not retain the full Jamovi data frame. Meta-regression
#' appends only its selected moderator columns later.
#'
#' @param self The jamovi `self` object.
#' @return A named list of arguments for `meta::metagen()`, or `NULL` if
#'   required columns or data are unavailable.
#' @noRd
buildGenArgs <- function(self) {
  data <- self$data
  options <- self$options
  ciMode <- options$inputMode == "ci"

  # jamovi lifecycle guard: A user in jamovi cannot pass NULL data; during a
  # normal .run() cycle, jamovi always provides a data.frame (with at least one
  # row). The ONLY time self$data is NULL is during later internal phases like
  # image rendering or save/export, when jamovi actively clears it. In these
  # later phases, model calculations must use cached state rather than attempt
  # to rebuild from unavailable data.
  if (is.null(data)) {
    return()
  }

  requiredVars <- if (ciMode) {
    c("ciEffectSize", "ciLower", "ciUpper")
  } else {
    c("effectSize", "standardError")
  }

  if (!hasRequiredVars(options, requiredVars)) {
    return()
  }

  # Curate numeric columns: core vars
  numericVars <- if (ciMode) {
    c(
      options$ciEffectSize,
      options$ciLower,
      options$ciUpper,
      options$ciTotal
    )
  } else {
    c(options$effectSize, options$standardError, options$total)
  }
  data[numericVars] <- lapply(data[numericVars], jmvcore::toNumeric)

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel / 100

  sm <- if (options$sm == "GEN") "Effect Size" else options$sm

  inputArgs <- if (ciMode) {
    list(
      TE = data[[options$ciEffectSize]],
      lower = data[[options$ciLower]],
      upper = data[[options$ciUpper]],
      level.ci = options$ciLevel / 100,
      # Do not derive missing effect estimates from CI limits
      approx.TE = "",
      transf = FALSE
    )
  } else {
    list(
      TE = data[[options$effectSize]],
      seTE = data[[options$standardError]],
      transf = TRUE
    )
  }

  args <- c(
    inputArgs,
    list(
      sm = sm,
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
  )

  studyLabel <- if (ciMode) options$ciStudyLabel else options$studyLabel
  if (!is.null(studyLabel)) {
    args$studlab <- data[[studyLabel]]
  }

  total <- if (ciMode) options$ciTotal else options$total
  if (!is.null(total)) {
    args$n.e <- data[[total]]
  }

  args
}
