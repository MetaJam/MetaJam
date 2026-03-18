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
    forestHeightUnit = options$subgroupForestHeightUnit
  )
}


#' Update Subgroup Result Visibility and Initialize Blank Content
#'
#' Sets visibility of subgroup text and plot results based on whether
#' the subgroup variable is supplied and the corresponding checkboxes
#' are on. When visible but model is not yet available, initializes
#' the text result with a blank titled HTML so the user sees a skeleton
#' rather than nothing.
#'
#' @param options The `self$options` object from a jamovi analysis.
#' @param results The `self$results` object from a jamovi analysis.
#' @param model The subgroup model object, or `NULL` if not yet computed.
#' @noRd
updateSubgroupVisibility <- function(options, results, model) {
  hasSubgroup <- !is.null(options$subgroupVariable)

  showText <- hasSubgroup && options$showSubgroupSummary
  showPlot <- hasSubgroup && options$subgroupForestPlot

  results$subgroupText$setVisible(showText)
  results$subgroupPlot$setVisible(showPlot)

  # Initialize blank HTML when visible but model not ready
  if (showText && is.null(model)) {
    results$subgroupText$setContent(
      asHtml(title = "Subgroup Analysis Summary")
    )
  }
}


#' Populate the Subgroup Text
#'
#' Called from `.run()` when the subgroup model is available.
#' Pairs with `updateSubgroupVisibility()` which handles show/hide
#' and skeleton initialization in `.init()`.
#'
#' @param textResult Html result element
#'   (e.g., `self$results$subgroupText`).
#' @param subgroupModel A `meta` object with subgroup results, or `NULL`.
#' @noRd
populateSubgroupText <- function(textResult, subgroupModel) {
  if (!textResult$visible) {
    return()
  }
  if (is.null(subgroupModel)) {
    return()
  }
  textResult$setContent(
    asHtml(summary(subgroupModel), title = "Subgroup Analysis Summary")
  )
}