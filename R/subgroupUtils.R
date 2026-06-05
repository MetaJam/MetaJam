#' Initialize Subgroup Models
#'
#' Adds one group per subgroup variable to the `subgroupModels` Array
#' and sets dynamic Group titles. Unlike meta-regression where empty
#' blocks are possible and need title-only placeholders, subgroup items
#' are only created when variables are assigned and visibility is
#' controlled by `length(subgroupVariables) > 0` in r.yaml. When required
#' variables are missing, a title-only placeholder is set on the text
#' element (same pattern as `initText()`).
#'
#' @param self The jamovi `self` object.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @noRd
initSubgroupModels <- function(self, requiredVars) {
  modelsArray <- self$results$subgroupModels
  options <- self$options
  vars <- options$subgroupVariables
  hasVars <- hasRequiredVars(options, requiredVars)

  for (i in seq_along(vars)) {
    modelsArray$addItem(key = i)
    group <- modelsArray$get(key = i)
    group$setTitle(
      paste0("Subgroup Analysis: ", vars[[i]])
    )

    if (group$subgroupText$visible && !hasVars) {
      group$subgroupText$setContent(asHtml(title = "Subgroup Summary"))
    }
  }
}


#' Populate Subgroup Text for All Variables
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or subgroup model is NULL. We use the NULL check of the model here
#' across our module mainly as a proxy that required variables are
#' available, which we already verified in `.run()` before reaching
#' this line. Although redundant, we keep it for clarity.
#'
#' @param self The jamovi `self` object.
#' @noRd
populateSubgroupTexts <- function(self) {
  modelsArray <- self$results$subgroupModels

  for (i in seq_along(self$options$subgroupVariables)) {
    textResult <- modelsArray$get(key = i)$subgroupText

    if (!textResult$visible || textResult$isFilled()) {
      next
    }

    model <- self$subgroupModels[[i]]
    if (is.null(model)) {
      next
    }

    textResult$setContent(
      asHtml(summary(model), title = "Subgroup Summary")
    )
  }
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
    addrowsBelowOverall = options$subgroupAddrowsBelowOverall,
    digitsEffect = options$subgroupDigitsEffect,
    digitsWeight = options$subgroupDigitsWeight,
    digitsI2 = options$subgroupDigitsI2,
    digitsTau2 = options$subgroupDigitsTau2,
    digitsPval = options$subgroupDigitsPval
  )

  renderForest(
    model,
    mapped,
    overall = options$subgroupForestOverall,
    overall.hetstat = options$subgroupForestOverall,
    test.effect.subgroup = options$subgroupForestTestEffect,
    test.subgroup = options$subgroupForestTestSubgroup,
    print.subgroup.name = options$printSubgroupName,
    calcwidth.hetstat = options$subgroupForestLayout == "subgroup",
    calcwidth.tests = options$subgroupForestLayout == "subgroup",
    ...
  )
}