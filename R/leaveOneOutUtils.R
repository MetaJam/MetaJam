#' Compute a Leave-One-Out Model
#'
#' Analysis-agnostic: works with any `meta` object (metacont, metabin, etc.).
#'
#' @param model A `meta` object.
#' @param options A Jamovi options object with `leaveOneOutPrediction`.
#' @return A `metainf` object, or `NULL` if model is NULL.
#' @noRd
computeLeaveOneOutModel <- function(model, options) {
  if (is.null(model)) {
    return()
  }
  meta::metainf(model, prediction = options$leaveOneOutPrediction)
}


#' Update Leave-One-Out Result Visibility
#'
#' Sets visibility of leave-one-out text and plot results based on the
#' master toggle and per-output checkboxes. Called from `.init()` to
#' avoid flashing.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updateLeaveOneOutVisibility <- function(options, results) {
  active <- options$leaveOneOut
  results$leaveOneOutText$setVisible(active && options$showLeaveOneOutSummary)
  results$leaveOneOutPlot$setVisible(active && options$leaveOneOutForestPlot)
}


#' Render a Leave-One-Out Forest Plot
#'
#' Handles grid canvas setup, leave-one-out-specific sort options
#' (I², τ²), and delegates to `meta::forest()` which dispatches
#' to `forest.metainf` → `forest.metacum` → `forest.meta`.
#'
#' @param leaveOneOutModel A `metainf` object.
#' @param options A Jamovi options object with `leaveOneOut*` fields.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderLeaveOneOutForest <- function(leaveOneOutModel, options) {
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

  colgap <- paste0(options$leaveOneOutColgap, options$leaveOneOutColgapUnit)
  colgap.forest <- paste0(
    options$leaveOneOutColgapForest,
    options$leaveOneOutColgapForestUnit
  )

  args <- list(
    x = leaveOneOutModel,
    new = FALSE,
    layout = options$leaveOneOutForestLayout,
    label.left = options$leaveOneOutLabelLeft,
    label.right = options$leaveOneOutLabelRight,
    colgap = colgap,
    colgap.forest = colgap.forest,
    details = options$leaveOneOutForestDetails
  )

  # Sort by leave-one-out result columns (I², τ²)
  if (options$leaveOneOutSortBy != "none") {
    args$sortvar <- switch(
      options$leaveOneOutSortBy,
      effectAsc = leaveOneOutModel$TE,
      effectDesc = -leaveOneOutModel$TE,
      i2Asc = leaveOneOutModel$I2,
      i2Desc = -leaveOneOutModel$I2,
      tau2Asc = leaveOneOutModel$tau2,
      tau2Desc = -leaveOneOutModel$tau2
    )
  }

  if (options$leaveOneOutXlimMode == "custom") {
    args$xlim <- c(options$leaveOneOutXlimLower, options$leaveOneOutXlimUpper)
  }

  if (options$leaveOneOutAddrowsMode == "custom") {
    args$addrows.below.overall <- options$leaveOneOutAddrowsBelowOverall
  }

  do.call(meta::forest, args)
}


#' Initialize the Leave-One-Out Text Skeleton
#'
#' Called from `.run()` to show a titled HTML placeholder before the
#' model is available. Same pattern as `initSubgroupText()`.
#'
#' @param textResult Html result element.
#' @param options The `self$options` object.
#' @param requiredVars Character vector of option names that must be assigned.
#' @noRd
initLeaveOneOutText <- function(textResult, options, requiredVars) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  if (!hasRequiredVars(options, requiredVars)) {
    textResult$setContent(asHtml(title = "Leave-One-Out Summary"))
  }
}


#' Populate the Leave-One-Out Text
#'
#' Called from `.run()` when the leave-one-out model is available.
#'
#' @param textResult Html result element.
#' @param leaveOneOutModel A `metainf` object.
#' @noRd
populateLeaveOneOutText <- function(textResult, leaveOneOutModel) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  textResult$setContent(
    asHtml(summary(leaveOneOutModel), title = "Leave-One-Out Summary")
  )
}