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


#' Compute Precomputed Effect Size Subgroup Models for All Variables
#'
#' Iterates over `options$subgroupVariables`, building a model for each
#' variable by calling `meta::metagen()` with `subgroup=`. Returns a
#' list of models. Cross-cycle caching is performed per-variable via the
#' corresponding `subgroupText` result element in the array.
#'
#' @param self The jamovi `self` object.
#' @return A list of `meta::metagen` objects with subgroup results,
#'   or `NULL` if no subgroup variables are assigned.
#' @noRd
computeGenSubgroupModels <- function(self) {
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
  # model with NULL data via buildGenArgs(), it would crash with a new,
  # confusing error that masks the original .run() failure. To prevent this,
  # buildGenArgs() checks if data is NULL and safely aborts, ensuring the true
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

  args <- buildGenArgs(self)
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

    models[[i]] <- do.call(meta::metagen, args)
    models[[i]] <- stripModel(models[[i]])

    # Cache for next cycle
    cacheElement$setState(models[[i]])
  }

  models
}


#' Render a Metagen-Specific Forest Plot
#'
#' Builds custom labels for the analysis-scale effect and standard-error
#' columns while preserving `meta::forest()`'s native column selection and
#' ordering for every layout. JAMA is left unchanged because it omits the raw
#' study TE and seTE columns. The full `leftlabs` vector uses `NA` for labels
#' which should retain their upstream defaults; `leftcols` is deliberately not
#' supplied so optional Total, weights, and the effect-with-CI column stay in
#' their theme-specific positions.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderGenForest <- function(self, sortKey) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  leftlabs <- buildGenForestLeftLabs(
    model,
    sm = options$sm,
    layout = options$forestLayout
  )

  renderForest(
    model,
    options,
    sortKey = sortKey,
    leftlabs = leftlabs,
    digits.TE = as.integer(options$digitsInputEffect),
    digits.se = as.integer(options$digitsSe)
  )

  TRUE
}


#' Render a Metagen Subgroup Forest Plot
#'
#' Applies the metagen-specific analysis-scale effect and standard-error labels
#' to ordinary subgroup forest layouts, while leaving JAMA and the subgroup-only
#' layout unchanged because they omit the raw study TE and seTE columns.
#' `leftcols` is not supplied, so `meta::forest()` keeps its default column
#' selection and ordering.
#'
#' @param self The jamovi `self` object.
#' @param key The jamovi array item key (e.g., `image$parent$key`).
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderGenSubgroupForest <- function(self, key, sortKey) {
  model <- self$subgroupModels[[key]]
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  leftlabs <- buildGenForestLeftLabs(
    model,
    sm = options$sm,
    layout = options$subgroupForestLayout,
    overall = options$subgroupForestOverall
  )

  renderSubgroupForest(
    model,
    options,
    sortKey = sortKey,
    leftlabs = leftlabs,
    digits.TE = as.integer(options$subgroupDigitsInputEffect),
    digits.se = as.integer(options$subgroupDigitsSe)
  )

  TRUE
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
  # later stages, we rely purely on cached models. If a model is missing from
  # the cache, it means an error occurred during the .run() phase. We do not
  # need to calculate it again. Furthermore, we cannot calculate it anyway
  # because using NULL data would crash with a new, confusing error. Returning
  # NULL here safely aborts the attempt and preserves the original .run() error.
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


#' Build Metagen Forest Plot Column Labels
#'
#' Creates the complete `leftlabs` vector for metagen forest plots while
#' preserving `meta::forest()`'s native column selection and ordering. Only the
#' raw effect-size and standard-error headings are replaced; `NA` placeholders
#' retain the upstream labels for Study, optional Total, weights, and the
#' combined effect-with-CI column.
#'
#' @param model A `meta::metagen` object.
#' @param sm Effect measure selected in Jamovi.
#' @param layout Forest plot layout.
#' @param overall Whether overall results are displayed. This determines
#'   whether RevMan 5 includes weight columns.
#' @return A character vector of labels, or `NULL` when the layout does not
#'   display the raw TE and seTE columns.
#' @noRd
buildGenForestLeftLabs <- function(model, sm, layout, overall = TRUE) {
  if (!layout %in% c("meta", "BMJ", "RevMan5")) {
    return()
  }

  # Mirrors the measures classified by meta as relative effects.
  relativeMeasures <- c("HR", "ROM", "RR", "OR", "IRR", "DOR")
  effectLabel <- if (sm == "GEN") {
    "Effect Size"
  } else if (sm == "VE") {
    "log(VR)"
  } else if (sm %in% relativeMeasures) {
    paste0("log(", sm, ")")
  } else {
    sm
  }

  # TODO: Extend the effectLabel mapping when additional metagen effect
  # measures, such as correlation or single-group measures, are added in the
  # future.

  # Default metagen study columns begin with Study, TE, and seTE. Preserve
  # Study's upstream label and replace only the two technical headings.
  leftlabs <- c(NA_character_, effectLabel, "SE")

  # Passing Total as n.e makes meta add it after seTE. Keep its native label
  # and position.
  if (!is.null(model$n.e)) {
    leftlabs <- c(leftlabs, NA_character_)
  }

  if (layout == "RevMan5") {
    # RevMan 5 moves weights and the combined effect-with-CI result to the
    # left. leftlabs must match that complete default set exactly or meta will
    # ignore the custom labels, so add NA placeholders without changing the
    # native columns or their order. Subgroup plots omit weight columns when
    # overall results are not displayed.
    #
    # Source checkpoint: meta/R/forest.R lines 4592-4596. RevMan 5 adds each
    # weight column only when the corresponding model has non-missing weights.
    if (
      overall &&
        isTRUE(model$common) &&
        !all(is.na(model$w.common))
    ) {
      leftlabs <- c(leftlabs, NA_character_)
    }
    if (
      overall &&
        isTRUE(model$random) &&
        !all(is.na(model$w.random))
    ) {
      leftlabs <- c(leftlabs, NA_character_)
    }

    # RevMan always places the combined effect-with-CI column on the left.
    leftlabs <- c(leftlabs, NA_character_)
  }

  leftlabs
}
