#' Compute a Leave-One-Out Model
#'
#' Analysis-agnostic: works with any `meta` object (metacont, metabin, etc.).
#'
#' @param self The jamovi `self` object.
#' @return A `metainf` object, or `NULL` if model is NULL.
#' @noRd
computeLeaveOneOutModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$leaveOneOutText$state
  if (!is.null(cached)) {
    return(cached)
  }

  if (is.null(self$model)) {
    return()
  }

  result <- meta::metainf(
    self$model,
    prediction = self$options$leaveOneOutPrediction
  )
  # Cache for next cycle
  self$results$leaveOneOutText$setState(result)
  result
}


#' Populate the Leave-One-Out Text
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or leave-one-out model is NULL. We use the NULL check of the model here
#' across our module mainly as a proxy that required variables are available,
#' which we already verified in `.run()` before reaching this line. Although
#' redundant, we keep it for clarity.
#'
#' @param self The jamovi `self` object.
#' @noRd
populateLeaveOneOutText <- function(self) {
  textResult <- self$results$leaveOneOutText
  if (
    !textResult$visible ||
      textResult$isFilled() ||
      is.null(self$leaveOneOutModel)
  ) {
    return()
  }

  textResult$setContent(
    asHtml(
      summary(self$leaveOneOutModel),
      title = "Leave-One-Out Summary",
      modifier = function(out) {
        if (length(out) > 0 && out[1] == "Leave-one-out meta-analysis") {
          out[-c(1, 2)]
        } else {
          out
        }
      }
    )
  )
}


#' Render a Leave-One-Out Forest Plot
#'
#' Handles grid canvas setup, leave-one-out-specific sort options
#' (I², τ²), and delegates to `meta::forest()` which dispatches
#' to `forest.metainf` → `forest.metacum` → `forest.meta`.
#'
#' @param self The jamovi `self` object.
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderLeaveOneOutForest <- function(self, sortKey) {
  leaveOneOutModel <- self$leaveOneOutModel
  options <- self$options

  if (is.null(leaveOneOutModel)) {
    return(FALSE)
  }

  colgap <- paste0(options$leaveOneOutColgap, options$leaveOneOutColgapUnit)
  colgap.forest <- paste0(
    options$leaveOneOutColgapForest,
    options$leaveOneOutColgapForestUnit
  )

  args <- list(
    x = leaveOneOutModel,
    layout = options$leaveOneOutForestLayout,
    label.left = options$leaveOneOutLabelLeft,
    label.right = options$leaveOneOutLabelRight,
    colgap = colgap,
    colgap.forest = colgap.forest,
    details = options$leaveOneOutForestDetails,
    # Use superscript column headers for I2 and Tau2 in leave-one-out plots
    label.tau2 = "Tau\u00b2",
    label.I2 = "I\u00b2",
    digits = as.integer(options$leaveOneOutDigitsEffect),
    digits.pval = as.integer(options$leaveOneOutDigitsPval),
    digits.I2 = as.integer(options$leaveOneOutDigitsI2),
    digits.tau2 = as.integer(options$leaveOneOutDigitsTau2),
    digits.tau = as.integer(options$leaveOneOutDigitsTau2)
  )

  if (!is.null(sortKey)) {
    args$sortvar <- sortKey
  }

  if (options$leaveOneOutXlimCustom) {
    args$xlim <- c(options$leaveOneOutXlimLower, options$leaveOneOutXlimUpper)
  }

  if (options$leaveOneOutAddrowsCustom) {
    args$addrows.below.overall <- options$leaveOneOutAddrowsBelowOverall
  }

  do.call(meta::forest, args)

  TRUE
}
