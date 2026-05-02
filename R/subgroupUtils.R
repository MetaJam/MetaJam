#' Update Subgroup Result Visibility
#'
#' Sets visibility of subgroup text and plot results based on whether
#' the subgroup variable is supplied and the corresponding checkboxes
#' are on. Called from `.init()` to avoid flashing.
#'
#' @param options The `self$options` object from a jamovi analysis.
#' @param results The `self$results` object from a jamovi analysis.
#' @noRd
updateSubgroupVisibility <- function(options, results) {
  hasSubgroup <- !is.null(options$subgroupVariable)

  results$subgroupText$setVisible(hasSubgroup && options$showSubgroupSummary)
  results$subgroupPlot$setVisible(hasSubgroup && options$subgroupForestPlot)
}


#' Initialize the Subgroup Text Skeleton
#'
#' Called from `.run()` to show a titled HTML placeholder before the
#' subgroup model is available. Uses `hasRequiredVars()` instead of
#' checking the model directly to avoid forcing the subgroupModel
#' (and thus the main model) active binding. The `isFilled()` guard
#' preserves the clearWith optimization.
#'
#' @param textResult Html result element
#'   (e.g., `self$results$subgroupText`).
#' @param options The `self$options` object from a jamovi analysis.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @noRd
initSubgroupText <- function(textResult, options, requiredVars) {
  if (!textResult$visible || textResult$isFilled()) {
    return()
  }
  if (!hasRequiredVars(options, requiredVars)) {
    textResult$setContent(asHtml(title = "Subgroup Analysis Summary"))
  }
}


#' Populate the Subgroup Text
#'
#' Called from `.run()` when the subgroup model is available.
#' Pairs with `updateSubgroupVisibility()` which handles show/hide
#' in `.init()`, and `initSubgroupText()` which sets the skeleton.
#' The `isFilled()` guard skips recomputation when a non-model option
#' changed and the previous content is still valid.
#'
#' No is.null(subgroupModel) guard is needed here because by the time
#' this function is called: hasRequiredVars already ensured core vars
#' are assigned, and !visible already exited when subgroupVariable is
#' NULL — so the subgroup model is guaranteed to exist.
#'
#' @param textResult Html result element
#'   (e.g., `self$results$subgroupText`).
#' @param subgroupModel A `meta` object with subgroup results.
#' @noRd
populateSubgroupText <- function(textResult, subgroupModel) {
  if (!textResult$visible || textResult$isFilled()) {
    return()
  }
  textResult$setContent(
    asHtml(summary(subgroupModel), title = "Subgroup Analysis Summary")
  )
}


#' Render a Subgroup Forest Plot
#'
#' Generic helper that draws a subgroup `meta::forest()` plot. Maps
#' subgroup-prefixed Jamovi options to the standard option names that
#' `renderForest()` expects, then delegates. Subgroup-specific
#' `forest.meta` arguments (test.effect.subgroup, calcwidth.*, etc.)
#' are injected alongside any analysis-specific `...` arguments.
#'
#' Analysis-specific wrappers (e.g. `renderContSubgroupForest`) should
#' call this after injecting any type-specific arguments into `...`
#' (e.g. `label.e`, `label.c`, `label.e.attach`).
#'
#' @param model A `meta` object with subgroup results.
#' @param options A Jamovi options object with `subgroup*` fields.
#' @param ... Extra arguments forwarded to `renderForest()` and then
#'   `meta::forest()`. Used by analysis-specific wrappers for
#'   type-specific args.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderSubgroupForest <- function(model, options, ...) {
  # Map subgroup-prefixed options → standard option names for renderForest()
  mapped <- list(
    forestLayout = options$subgroupForestLayout,
    sortBy = options$subgroupSortBy,
    labelLeft = options$subgroupLabelLeft,
    labelRight = options$subgroupLabelRight,
    colgap = options$subgroupColgap,
    colgapUnit = options$subgroupColgapUnit,
    colgapForest = options$subgroupColgapForest,
    colgapForestUnit = options$subgroupColgapForestUnit,
    forestTestOverall = options$subgroupForestTestOverall,
    forestDetails = options$subgroupForestDetails,
    forestPrintI2Ci = options$subgroupForestPrintI2Ci,
    forestPrintTau2Ci = options$subgroupForestPrintTau2Ci,
    xlimCustom = options$subgroupXlimCustom,
    xlimLower = options$subgroupXlimLower,
    xlimUpper = options$subgroupXlimUpper,
    addrowsCustom = options$subgroupAddrowsCustom,
    addrowsBelowOverall = options$subgroupAddrowsBelowOverall
  )

  renderForest(
    model,
    mapped,
    test.effect.subgroup = options$subgroupForestTestEffect,
    test.subgroup = options$subgroupForestTestSubgroup,
    subgroup.name = options$subgroupVariable,
    print.subgroup.name = options$printSubgroupName,
    calcwidth.hetstat = options$subgroupForestLayout == "subgroup",
    calcwidth.tests = options$subgroupForestLayout == "subgroup",
    ...
  )
}