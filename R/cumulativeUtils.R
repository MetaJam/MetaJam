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

  # metacum() uses a single pooled model. By leaving `pooled` unspecified,
  # MetaJam follows meta's common-first rule when both models are available.
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
      sortValue <- self$data[[options$cumulativeSortVariable]]
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

  colgap <- paste0(options$cumulativeColgap, options$cumulativeColgapUnit)
  colgap.forest <- paste0(
    options$cumulativeColgapForest,
    options$cumulativeColgapForestUnit
  )

  args <- list(
    x = cumulativeModel,
    layout = options$cumulativeForestLayout,
    label.left = options$cumulativeLabelLeft,
    label.right = options$cumulativeLabelRight,
    colgap = colgap,
    colgap.forest = colgap.forest,
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
