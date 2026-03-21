#' Render a Forest Plot
#'
#' Generic helper that draws a `meta::forest()` plot. Handles the grid
#' canvas setup (newpage + white background) and passes shared Jamovi
#' options through.  Reusable across all meta-analysis classes.
#'
#' Analysis-specific wrappers (e.g. `renderContForest`) should call this
#' after injecting any type-specific arguments into `...`.
#'
#' @param model A `meta` object (e.g., from `meta::metacont`).
#' @param options A Jamovi options object with forest-related fields.
#' @param ... Extra arguments forwarded to `meta::forest()`.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderForest <- function(model, options, ...) {
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

  # Format numeric gaps into strings with units (e.g. "2mm")
  # Values are always present — validated Number inputs in .a.yaml
  colgap <- paste0(options$colgap, options$colgapUnit)
  colgap.forest <- paste0(options$colgapForest, options$colgapForestUnit)

  args <- list(
    x = model,
    new = FALSE,
    layout = options$forestLayout,
    label.left = options$labelLeft,
    label.right = options$labelRight,
    colgap = colgap,
    colgap.forest = colgap.forest,
    test.overall = options$forestTestOverall,
    details = options$forestDetails,
    print.I2.ci = options$forestPrintI2Ci,
    print.tau2.ci = options$forestPrintTau2Ci,
    ...
  )

  # Sort variable
  if (options$sortBy != "none") {
    args$sortvar <- switch(
      options$sortBy,
      effectAsc = model$TE,
      effectDesc = -model$TE,
      # Match meta's default: common weights when common is active
      weightAsc = if (isTRUE(model$common)) model$w.common else model$w.random,
      weightDesc = -(if (isTRUE(model$common)) {
        model$w.common
      } else {
        model$w.random
      })
    )
  }

  # When custom, pass xlim; when auto, let meta use its own default
  if (options$xlimMode == "custom") {
    args$xlim <- c(options$xlimLower, options$xlimUpper)
  }

  # When custom, pass addrows.below.overall; when auto, let meta's smart
  # auto-calculation kick in
  if (options$addrowsMode == "custom") {
    args$addrows.below.overall <- options$addrowsBelowOverall
  }

  do.call(meta::forest, args)
}

#' Calculate Height for Forest Plot
#'
#' Runs forest plot rendering in a null PDF device to measure the
#' required height without actually drawing anything visible.
#'
#' @param model A `meta` object.
#' @param options A Jamovi options object with forest-related fields.
#' @param ... Extra arguments forwarded to `renderForest()`.
#' @return The calculated height in points.
#' @noRd
calcForestHeight <- function(model, options, renderFn = renderForest, ...) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  res <- renderFn(model, options, ...)
  res$figheight$total_height * 72
}

#' Convert Plot Adjustment Unit to Pixels (jamovi uses 72 dpi)
#' @noRd
convertToPx <- function(x, unit) {
  x / c(inch = 1, cm = 2.54, mm = 25.4)[unit] * 72
}

#' Initialize a Forest Plot Image (Shared .init() Helper)
#'
#' Handles the full sizing workflow for any forest-type image:
#' checks guards, calculates height, and calls `setSize()`.
#' Designed to be called from `.init()` across all meta-analysis types.
#'
#' @param image A jamovi Image result element (e.g., `self$results$plot`).
#' @param model A `meta` object. If `NULL`, the function returns early.
#' @param options A Jamovi options object with forest-related fields.
#' @param width Plot width in pixels (default 800).
#' @param ... Extra arguments forwarded to `calcForestHeight()`.
#' @return `NULL` invisibly. Called for side effects (`setSize()`).
#' @noRd
initForestPlot <- function(
  image,
  model,
  options,
  renderFn = renderForest,
  width = 800,
  ...
) {
  if (!image$visible) {
    return()
  }
  if (is.null(model)) {
    return()
  }

  height <- calcForestHeight(model, options, renderFn = renderFn, ...)

  w_adj <- convertToPx(options$forestWidthAdjust, options$forestWidthUnit)
  h_adj <- convertToPx(options$forestHeightAdjust, options$forestHeightUnit)

  final_width <- width + w_adj
  final_height <- height + h_adj

  image$setSize(width = final_width, height = final_height)
}