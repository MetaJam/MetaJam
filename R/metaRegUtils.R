#' Update Meta-Regression Result Visibility
#'
#' Sets visibility of meta-regression text based on whether the user has
#' assigned at least one covariate or factor and the summary checkbox is on.
#' Called from `.init()` to avoid flashing.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updateMetaRegVisibility <- function(options, results) {
  hasMetaRegVars <-
    length(options$metaRegCovs) > 0 || length(options$metaRegFactors) > 0
  results$metaRegText$setVisible(hasMetaRegVars && options$showMetaRegSummary)
}


#' Update Bubble Plot Visibility
#'
#' Sets visibility of the bubble plot based on whether the user has assigned at
#' least one covariate or factor, has model terms, and the bubble plot checkbox
#' is on. Called from `.init()`.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updateBubblePlotVisibility <- function(options, results) {
  hasMetaRegVars <-
    length(options$metaRegCovs) > 0 || length(options$metaRegFactors) > 0
  results$bubblePlot$setVisible(hasMetaRegVars && options$bubblePlot)
}


#' Compute a Meta-Regression Model
#'
#' Analysis-agnostic: works with any `meta` object (metacont, metabin, etc.).
#' Builds a formula from the terms list and calls `meta::metareg()`. Moderator
#' columns are expected to already exist in `model$data` (ensured by passing
#' `data=` when creating the meta object).
#'
#' @param model A `meta` object (must have been created with `data=`).
#' @param options The jamovi options object (needs `metaRegTerms`,
#'   `metaRegIntercept`).
#' @return A `metareg` object, or `NULL` if model or terms are empty.
#' @noRd
computeMetaRegModel <- function(model, options) {
  if (is.null(model)) {
    return()
  }

  terms <- options$metaRegTerms
  if (length(terms) == 0) {
    return()
  }

  composed <- jmvcore::composeTerms(terms)
  formula <- as.formula(paste("~", paste(composed, collapse = " + ")))
  meta::metareg(model, formula, intercept = options$metaRegIntercept)
}


#' Initialize the Meta-Regression Text Skeleton
#'
#' Called from `.run()` to show a titled HTML placeholder before the model is
#' available. Same pattern as `initLeaveOneOutText()`.
#'
#' @param textResult Html result element.
#' @param options The `self$options` object.
#' @param requiredVars Character vector of option names that must be assigned.
#' @noRd
initMetaRegText <- function(textResult, options, requiredVars) {
  if (!textResult$visible || textResult$isFilled()) {
    return()
  }
  if (
    !hasRequiredVars(options, requiredVars) || length(options$metaRegTerms) == 0
  ) {
    textResult$setContent(asHtml(title = "Meta-Regression Summary"))
  }
}


#' Get Scale Label for Meta-Regression Output
#'
#' Returns a human-readable label describing the scale of the regression
#' estimates (e.g., "Log Odds Ratio", "Standardised Mean Difference"). Works
#' with any standard meta analysis type (metacont, metabin, metaprop, metarate,
#' metainc, etc.).
#'
#' Since backtransf is always FALSE, xlab_meta never returns "".
#'
#' NOTE: For future metagen() support with custom transforms, pass func.transf /
#' func.backtransf to xlab_meta. See meta:::xlab_meta in meta/R/meta-xlab.R
#' L150-161 and bubble.R L349-362 for the empty-label fallback.
#'
#' @param metaRegModel A `metareg` object.
#' @return A character string label.
#' @noRd
getMetaRegScaleLabel <- function(metaRegModel) {
  meta:::xlab_meta(metaRegModel$.meta$x$sm, backtransf = FALSE)
}


#' Populate the Meta-Regression Text
#'
#' Called from `.run()` when the meta-regression model is available.
#'
#' @param textResult Html result element.
#' @param metaRegModel A `metareg` object.
#' @noRd
populateMetaRegText <- function(textResult, metaRegModel) {
  if (!textResult$visible || textResult$isFilled()) {
    return()
  }

  scaleLabel <- getMetaRegScaleLabel(metaRegModel)
  textResult$setContent(
    asHtml(
      summary(metaRegModel),
      cat(
        "\nNote: Estimates and confidence intervals are on the",
        scaleLabel,
        "scale."
      ),
      title = "Meta-Regression Summary"
    )
  )
}


#' Render the Bubble Plot via meta::bubble()
#'
#' Draws a bubble plot for a meta-regression model using meta's own bubble
#' function, which handles transformations, reference lines, labels, and
#' categorical covariates internally.
#'
#' Note:  In case of multiple groups in the factor variable, only the last one
#' shows as it show one for every group against the reference. Try to see what
#' we gonna do with this the same for multivariables.
#'
#' @param options The `self$options` object.
#' @noRd
renderBubblePlot <- function(metaRegModel, options) {
  meta::bubble(
    metaRegModel,
    regline = options$bubbleRegline,
    studlab = options$bubbleStudyLabel
  )
}