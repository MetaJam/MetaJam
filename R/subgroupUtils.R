#' Populate the Subgroup Text
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or subgroup model is NULL. We use the NULL check of the model here across
#' our module mainly as a proxy that required variables are available, which
#' we already verified in `.run()` before reaching this line. Although
#' redundant, we keep it for clarity.
#'
#' @param self The jamovi `self` object.
#' @noRd
populateSubgroupText <- function(self) {
  textResult <- self$results$subgroupText
  if (
    !textResult$visible || textResult$isFilled() || is.null(self$subgroupModel)
  ) {
    return()
  }
  textResult$setContent(
    asHtml(summary(self$subgroupModel), title = "Subgroup Analysis Summary")
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