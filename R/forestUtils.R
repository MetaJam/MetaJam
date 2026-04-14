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
  # Format numeric gaps into strings with units (e.g. "2mm")
  # Values are always present — validated Number inputs in .a.yaml
  colgap <- paste0(options$colgap, options$colgapUnit)
  colgap.forest <- paste0(options$colgapForest, options$colgapForestUnit)

  args <- list(
    x = model,
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

#' Calculate Forest Plot Dimensions
#'
#' Renders the forest plot in a null PDF device and extracts the true
#' width and height from `meta`'s internal grid layout.
#'
#' `meta::forest()` constructs a [grid::grid.layout()] with exact
#' column widths (measured from text grobs) and uniform row heights.
#' The `figheight` value returned by `meta::forest()` is only a
#' heuristic row-count estimate (via the internal `gh()` function)
#' used to size file devices before the layout exists; the grid
#' layout captured here is the authoritative source of dimensions.
#'
#' A small padding is added to account for elements that extend
#' beyond the grid layout (x-axis tick labels, floating labels such
#' as `label.left` / `label.right`).
#'
#' @param model A `meta` object.
#' @param options A Jamovi options object with forest-related fields.
#' @param renderFn Render function (default `renderForest`).
#' @param ... Extra arguments forwarded to `renderFn()`.
#' @return A list with `width` and `height` in inches.
#' @noRd
calcForestDims <- function(model, options, renderFn = renderForest, ...) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  gtree <- grid::grid.grabExpr(renderFn(model, options, ...))

  # The main viewport's layout sits at the vpTree parent
  layout <- gtree$childrenvp[[1]]$parent$layout

  width <- grid::convertWidth(
    sum(layout$widths),
    "inches",
    valueOnly = TRUE
  )
  height <- grid::convertHeight(
    sum(rep(layout$heights, layout$nrow)),
    "inches",
    valueOnly = TRUE
  )

  list(width = width + 0.3, height = height + 0.8)
}

#' Initialize a Forest Plot Image (Shared .init() Helper)
#'
#' Handles the full sizing workflow for any forest-type image:
#' checks guards, extracts accurate dimensions from the grid layout,
#' and calls `setSize()`.
#' Designed to be called from `.init()` across all meta-analysis types.
#'
#' @param image A jamovi Image result element (e.g., `self$results$plot`).
#' @param model A `meta` object. If `NULL`, the function returns early.
#' @param options A Jamovi options object with forest-related fields.
#' @param renderFn Render function (default `renderForest`).
#' @param ... Extra arguments forwarded to `calcForestDims()`.
#' @return `NULL` invisibly. Called for side effects (`setSize()`).
#' @noRd
initForestPlot <- function(
  image,
  model,
  options,
  renderFn = renderForest,
  ...
) {
  if (!image$visible || is.null(model)) {
    return()
  }

  dims <- calcForestDims(model, options, renderFn = renderFn, ...)

  # Jamovi renders images at 72 DPI; convert inches → pixels
  image$setSize(
    width = dims$width * 72,
    height = dims$height * 72
  )
}