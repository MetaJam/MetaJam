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
#' @param sortKey Precomputed sort key from `calcForestSortKey()`.
#' @param ... Extra arguments forwarded to `meta::forest()`.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderForest <- function(model, options, sortKey, ...) {
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
    digits = as.integer(options$digitsEffect),
    digits.pval = as.integer(options$digitsPval),
    digits.pval.Q = as.integer(options$digitsPval),
    digits.weight = as.integer(options$digitsWeight),
    digits.I2 = as.integer(options$digitsI2),
    digits.tau2 = as.integer(options$digitsTau2),
    ...
  )

  if (!is.null(sortKey)) {
    args$sortvar <- sortKey
  }

  # When custom, pass xlim; when auto, let meta use its own default
  if (options$xlimCustom) {
    args$xlim <- c(options$xlimLower, options$xlimUpper)
  }

  # When custom, pass addrows.below.overall; when auto, let meta's smart
  # auto-calculation kick in
  if (options$addrowsCustom) {
    args$addrows.below.overall <- options$addrowsBelowOverall
  }

  do.call(meta::forest, args)
}


#' Calculate Forest Sort Key
#'
#' Resolves the active forest sort option to the numeric key passed to
#' `meta::forest(sortvar=)`. Returns `NULL` for original ascending order so
#' `meta::forest()` can use its default study order.
#'
#' @param model A `meta` object (e.g., from `meta::metacont`).
#' @param sortBy Sort option name from the UI.
#' @param sortDirection Sort direction (`"asc"` or `"desc"`).
#' @param sortVariable External data variable selected for sorting, if any.
#' @param data Analysis data frame, used when sorting by a data column.
#' @return A numeric sort key, or `NULL` for original ascending order.
#' @noRd
calcForestSortKey <- function(model, sortBy, sortDirection, sortVariable, data) {
  if (is.null(model)) {
    return()
  }

  if (sortBy == "none" && sortDirection == "asc") {
    return()
  }

  sortValue <- switch(
    sortBy,
    none = seq_along(model$TE),
    effect = model$TE,
    weight = if (isTRUE(model$common)) model$w.common else model$w.random,
    i2 = model$I2,
    tau2 = model$tau2
  )

  if (startsWith(sortBy, "varid::")) {
    sortValue <- data[[sortVariable]]
  }

  sortKey <- xtfrm(sortValue)
  if (sortDirection == "desc") {
    sortKey <- -sortKey
  }

  sortKey
}


#' Update Forest Plot Size and Cache Dimensions
#'
#' Calculates accurate dimensions for a forest-type image, applies them
#' via `setSize()`, and persists them in a hidden cache element so that
#' `.postInit()` can restore the size without recomputing.
#'
#' Designed to be called from `.run()` — NOT from `.init()`.
#'
#' @param image A jamovi Image result element (e.g., `self$results$plot`).
#' @param model A `meta` object. If `NULL`, the function returns early.
#' @param sizeCache A hidden Group result element with `clearWith: []`
#'   used to persist the dimensions across engine requests.
#' @param renderCall A zero-argument closure that renders the forest plot.
#' @return `NULL` invisibly. Called for side effects (`setSize`,
#'   `setState`).
#' @noRd
updateForestSize <- function(
  image,
  model,
  sizeCache,
  renderCall
) {
  if (!image$visible || image$isFilled() || is.null(model)) {
    return()
  }

  dims <- calcForestDims(renderCall = renderCall)
  w <- dims$width * 72
  h <- dims$height * 72
  image$setSize(width = w, height = h)
  sizeCache$setState(list(w = w, h = h))
}


#' Apply Cached Plot Dimensions
#'
#' Shared `.postInit()` helper. Restores plot dimensions from a hidden
#' `clearWith: []` cache element. Skips when the cache is empty (first
#' run), the plot is hidden, or `clearWith` cleared it (`isFilled()`
#' is FALSE). Essential for correct save/export sizing since
#' `fromProtoBuf()` does not restore `widthM`/`heightM`.
#'
#' @param image An Image result element (e.g., `self$results$plot`).
#' @param sizeCache A Group result element with `clearWith: []`
#'   (e.g., `self$results$plotSizeCache`).
#' @noRd
applyCachedSize <- function(image, sizeCache) {
  size <- sizeCache$state
  if (!is.null(size) && image$visible && image$isFilled()) {
    image$setSize(size$w, size$h)
  }
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
#' @param renderCall A zero-argument closure that renders the forest plot.
#' @return A list with `width` and `height` in inches.
#' @noRd
calcForestDims <- function(renderCall) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  gtree <- grid::grid.grabExpr(renderCall())

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
