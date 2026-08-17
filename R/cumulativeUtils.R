#' Compute a Cumulative Meta-Analysis Model
#'
#' Analysis-agnostic: works with any supported `meta` object. Studies can be
#' sorted by their original order, standard error, or a selected data variable.
#'
#' @param self The jamovi `self` object.
#' @return A `metacum` object, or `NULL` if model is NULL.
#' @noRd
computeCumulativeModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$cumulativeText$state
  if (!is.null(cached)) {
    return(cached)
  }

  if (is.null(self$model)) {
    return()
  }

  options <- self$options

  # metacum() uses one pooled model. MetaJam always passes `prediction`
  # explicitly and enables it only for a random-only analysis. Thus, leaving
  # `pooled` unspecified selects common for "both" and random for "random".
  # If prediction is later enabled for "both", meta will select random.
  prediction <- options$cumulativePrediction && options$model == "random"

  result <- if (
    options$cumulativeSortBy == "none" &&
      options$cumulativeSortDirection == "asc"
  ) {
    meta::metacum(self$model, prediction = prediction)
  } else {
    # Prepare the cumulative sort key
    sortValue <- switch(
      options$cumulativeSortBy,
      none = seq_along(self$model$TE),
      se = self$model$seTE
    )

    if (startsWith(options$cumulativeSortBy, "varid::")) {
      data <- self$data
      # jamovi lifecycle guard: A user in jamovi cannot pass NULL data;
      # during a normal .run() cycle, jamovi always provides a data.frame
      # (with at least one row). The ONLY time self$data is NULL is during
      # later internal phases like image rendering or save/export, when
      # jamovi actively clears it. In these later stages, we rely purely on
      # cached models. If the cumulative model is missing from the cache, it
      # means an error occurred during the .run() phase. We do not need to
      # calculate it again. Furthermore, calculating a cumulative model sorted
      # by a selected variable requires the original data, which are unavailable
      # in this phase. Attempting this with NULL data would crash with a new,
      # confusing error. Returning NULL safely aborts the attempt and preserves
      # the original .run() error.
      if (is.null(data)) {
        return()
      }

      sortValue <- data[[options$cumulativeSortVariable]]
    }

    sortKey <- xtfrm(sortValue)
    if (options$cumulativeSortDirection == "desc") {
      sortKey <- -sortKey
    }

    meta::metacum(
      self$model,
      sortvar = sortKey,
      prediction = prediction
    )
  }

  # metacum keeps the source meta object in $x for package bookkeeping, but
  # MetaJam only uses the cumulative result, so do not cache the source model.
  result$x <- NULL

  # Match stripModel(): calls are not used by MetaJam and can become large if
  # this calculation is ever changed to a call-capturing path such as do.call().
  result$call <- NULL

  # Cache for next cycle
  self$results$cumulativeText$setState(result)
  result
}


#' Populate the Cumulative Meta-Analysis Text
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or the cumulative model is NULL. We use the NULL check of the model here
#' across our module mainly as a proxy that required variables are available,
#' which we already verified in `.run()` before reaching this line. Although
#' redundant, we keep it for clarity.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateCumulativeText <- function(self) {
  textResult <- self$results$cumulativeText
  if (
    !textResult$visible ||
      textResult$isFilled() ||
      is.null(self$cumulativeModel)
  ) {
    return(invisible(NULL))
  }

  textResult$setContent(
    asHtml(
      summary(self$cumulativeModel),
      title = "Cumulative Meta-Analysis Summary",
      modifier = function(out) {
        if (length(out) > 0 && out[1] == "Cumulative meta-analysis") {
          out[-c(1, 2)]
        } else {
          out
        }
      }
    )
  )

  invisible(NULL)
}


#' Render a Cumulative Forest Plot
#'
#' Renders a forest plot for the cumulative meta-analysis.
#'
#' @param self The jamovi `self` object.
#'
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#'
#' @noRd
renderCumulativeForest <- function(self) {
  cumulativeModel <- self$cumulativeModel
  options <- self$options

  if (is.null(cumulativeModel)) {
    return(FALSE)
  }

  colgap.left <- paste0(
    options$cumulativeColgapLeft,
    options$cumulativeColgapLeftUnit
  )
  colgap.right <- paste0(
    options$cumulativeColgapRight,
    options$cumulativeColgapRightUnit
  )
  colgap.forest.left <- paste0(
    options$cumulativeColgapForestLeft,
    options$cumulativeColgapForestLeftUnit
  )
  colgap.forest.right <- paste0(
    options$cumulativeColgapForestRight,
    options$cumulativeColgapForestRightUnit
  )

  args <- list(
    x = cumulativeModel,
    layout = options$cumulativeForestLayout,
    label.left = options$cumulativeLabelLeft,
    label.right = options$cumulativeLabelRight,
    colgap.left = colgap.left,
    colgap.right = colgap.right,
    colgap.forest.left = colgap.forest.left,
    colgap.forest.right = colgap.forest.right,
    # Follow the compact-width policy documented in renderForest().
    calcwidth.hetstat = FALSE,
    calcwidth.tests = FALSE,
    details = options$cumulativeForestDetails,
    # Use superscript column headers for I2 and Tau2
    label.tau2 = "Tau\u00b2",
    label.I2 = "I\u00b2",
    digits = as.integer(options$cumulativeDigitsEffect),
    digits.pval = as.integer(options$cumulativeDigitsPval),
    digits.I2 = as.integer(options$cumulativeDigitsI2),
    digits.tau2 = as.integer(options$cumulativeDigitsTau2),
    digits.tau = as.integer(options$cumulativeDigitsTau2)
  )

  if (options$cumulativeXlimCustom) {
    args$xlim <- c(options$cumulativeXlimLower, options$cumulativeXlimUpper)
  }

  if (options$cumulativeAddrowsCustom) {
    args$addrows.below.overall <- options$cumulativeAddrowsBelowOverall
  }

  do.call(meta::forest, args)

  TRUE
}
