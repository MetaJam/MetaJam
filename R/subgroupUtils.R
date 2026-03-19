#' Resolve the Subgroup Model (If Ready)
#'
#' Encapsulates the shared subgroupModel active-binding workflow:
#' guard that the primary model and subgroup variable exist, ensure
#' data is loaded, extract the subgroup column, and delegate to
#' `computeSubgroupModel()`.
#'
#' @param analysis The jamovi analysis object (`self`), from which `model`, `options` and `data` are extracted.
#' @return An updated `meta` object with subgroup results, or `NULL`.
#' @noRd
resolveSubgroupModel <- function(analysis) {
  model <- analysis$model
  if (is.null(model)) {
    return()
  }
  if (is.null(analysis$options$subgroupVariable)) {
    return()
  }
  data <- analysis$data
  if (is.null(data) || nrow(data) == 0) {
    data <- analysis$readDataset()
  }
  subgroup <- data[[analysis$options$subgroupVariable]]
  computeSubgroupModel(model, subgroup, analysis$options)
}


#' Compute a Subgroup Meta-Analysis Model
#'
#' Reusable helper. Takes an existing `meta` object and updates it with
#' subgroup information via `update.meta()`.
#'
#' @param model A `meta` object (e.g., from `meta::metacont`).
#' @param subgroup A factor or character vector defining subgroups.
#' @param options A Jamovi options object with subgroup-related fields.
#' @return An updated `meta` object with subgroup results.
#' @noRd
computeSubgroupModel <- function(model, subgroup, options) {
  update(
    model,
    subgroup = subgroup,
    tau.common = options$tauCommon,
    prediction.subgroup = options$predictionSubgroup
  )
}


#' Build a Virtual-Options Object for Subgroup Forest Rendering
#'
#' Maps subgroup-prefixed option names to the standard names that
#' `renderForest()` expects. This avoids duplicating `renderForest` logic.
#'
#' Analysis-type-specific labels (e.g., `labelE`, `labelC` for two-group
#' analyses) are NOT included here — add them in your `.b.R` before
#' calling the type-specific render wrapper.
#'
#' @param options A Jamovi options object.
#' @return A named list mirroring the standard forest option names.
#' @noRd
buildSubgroupForestOptions <- function(options) {
  list(
    forestLayout = options$subgroupForestLayout,
    sortBy = options$subgroupSortBy,
    labelLeft = options$subgroupLabelLeft,
    labelRight = options$subgroupLabelRight,
    colgap = options$subgroupColgap,
    colgapUnit = options$subgroupColgapUnit,
    colgapForest = options$subgroupColgapForest,
    colgapForestUnit = options$subgroupColgapForestUnit,
    xlimMode = options$subgroupXlimMode,
    xlimLower = options$subgroupXlimLower,
    xlimUpper = options$subgroupXlimUpper,
    addrowsMode = options$subgroupAddrowsMode,
    addrowsBelowOverall = options$subgroupAddrowsBelowOverall,
    forestWidthAdjust = options$subgroupForestWidthAdjust,
    forestWidthUnit = options$subgroupForestWidthUnit,
    forestHeightAdjust = options$subgroupForestHeightAdjust,
    forestHeightUnit = options$subgroupForestHeightUnit,
    forestTestOverall = options$subgroupForestTestOverall,
    forestDetails = options$subgroupForestDetails,
    forestPrintI2Ci = options$subgroupForestPrintI2Ci,
    forestPrintTau2Ci = options$subgroupForestPrintTau2Ci
  )
}


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
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
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
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  textResult$setContent(
    asHtml(summary(subgroupModel), title = "Subgroup Analysis Summary")
  )
}