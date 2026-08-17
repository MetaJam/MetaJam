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

  # metainf() follows the same pooled-model rule as metacum(): MetaJam's
  # explicit prediction gating selects common for "both" and random for
  # "random". If prediction is later enabled for "both", meta selects random.
  prediction <- self$options$leaveOneOutPrediction &&
    self$options$model == "random"

  result <- meta::metainf(self$model, prediction = prediction)

  # metainf keeps the source meta object in $x for package bookkeeping, but
  # MetaJam only prints/plots the leave-one-out result, so do not cache it.
  result$x <- NULL

  # Match stripModel(): calls are not used by MetaJam and can become large if
  # this calculation is ever changed to a call-capturing path such as do.call().
  result$call <- NULL

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
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateLeaveOneOutText <- function(self) {
  textResult <- self$results$leaveOneOutText
  if (
    !textResult$visible ||
      textResult$isFilled() ||
      is.null(self$leaveOneOutModel)
  ) {
    return(invisible(NULL))
  }

  textResult$setContent(
    asHtml(
      summary(self$leaveOneOutModel),
      title = "Leave-One-Out Analysis Summary",
      modifier = function(out) {
        if (length(out) > 0 && out[1] == "Leave-one-out meta-analysis") {
          out[-c(1, 2)]
        } else {
          out
        }
      }
    )
  )

  invisible(NULL)
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

  colgap.left <- paste0(
    options$leaveOneOutColgapLeft,
    options$leaveOneOutColgapLeftUnit
  )
  colgap.right <- paste0(
    options$leaveOneOutColgapRight,
    options$leaveOneOutColgapRightUnit
  )
  colgap.forest.left <- paste0(
    options$leaveOneOutColgapForestLeft,
    options$leaveOneOutColgapForestLeftUnit
  )
  colgap.forest.right <- paste0(
    options$leaveOneOutColgapForestRight,
    options$leaveOneOutColgapForestRightUnit
  )

  args <- list(
    x = leaveOneOutModel,
    layout = options$leaveOneOutForestLayout,
    label.left = options$leaveOneOutLabelLeft,
    label.right = options$leaveOneOutLabelRight,
    colgap.left = colgap.left,
    colgap.right = colgap.right,
    colgap.forest.left = colgap.forest.left,
    colgap.forest.right = colgap.forest.right,
    # Follow the compact-width policy documented in renderForest().
    calcwidth.hetstat = FALSE,
    calcwidth.tests = FALSE,
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
