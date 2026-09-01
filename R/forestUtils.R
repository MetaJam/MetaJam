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
#' @param sortKey Precomputed sort key from `prepareForestSortKey()`.
#' @param ... Extra arguments forwarded to `meta::forest()`.
#' @return The (invisible) return value of `meta::forest()`.
#' @noRd
renderForest <- function(model, options, sortKey, ...) {
  extraArgs <- list(...)
  hasReference <-
    !inherits(model, c("metamean", "metaprop", "metarate")) ||
    !is.na(model$null.effect)

  # Format numeric gaps into strings with units (e.g. "2mm")
  # Values are always present — validated Number inputs in .a.yaml
  colgap.left <- paste0(options$colgapLeft, options$colgapLeftUnit)
  colgap.right <- paste0(options$colgapRight, options$colgapRightUnit)
  colgap.forest.left <- paste0(
    options$colgapForestLeft,
    options$colgapForestLeftUnit
  )
  colgap.forest.right <- paste0(
    options$colgapForestRight,
    options$colgapForestRightUnit
  )

  args <- list(
    x = model,
    layout = options$forestLayout,
    label.left = if (hasReference) options$labelLeft else "",
    label.right = if (hasReference) options$labelRight else "",
    colgap.left = colgap.left,
    colgap.right = colgap.right,
    colgap.forest.left = colgap.forest.left,
    colgap.forest.right = colgap.forest.right,
    # meta now defaults calcwidth.hetstat to TRUE. In standard layouts, when
    # other left columns follow Study, the calculated width is added after the
    # study-label column. This separates Study from the numeric columns and
    # makes rows harder to trace. MetaJam leaves it disabled there: users move
    # footer text down (preferred) or increase colgap.forest.left, which moves
    # the complete left table away from the plot without widening the gap after
    # Study.
    #
    # The subgroup-only layout has one column left of the plot. Subgroup
    # heterogeneity is printed in that column, overlaps the plot, and cannot be
    # moved to the footer. In this layout, calcwidth.hetstat has the same effect
    # as manually increasing colgap.forest.left, so MetaJam enables it for
    # convenience and a non-overlapping default.
    calcwidth.hetstat = options$forestLayout == "subgroup",

    # meta currently defaults calcwidth.tests to FALSE. MetaJam keeps it FALSE
    # except in the subgroup-only layout when both models are shown and the
    # Subgroup effect test is used. In that case, the two model-specific test
    # lines are repeated inside every subgroup and overlap the plot, and cannot
    # be moved to the footer. In this layout, calcwidth.tests follows the same
    # logic as calcwidth.hetstat: enabling it has the same effect as manually
    # increasing colgap.forest.left.
    #
    # calcwidth.tests measures all displayed tests, including footer tests.
    # Therefore, a longer subgroup-difference or overall test can make the plot
    # slightly wider than the subgroup-effect lines alone require. MetaJam
    # accepts that extra space here to keep the repeated subgroup lines from
    # colliding with the plot.
    #
    # With one model, the heterogeneity line normally already provides enough
    # width for the subgroup-effect test. The rare exceptions are when all
    # subgroups contain one study or when extreme rounding settings make the
    # test line slightly longer. MetaJam does not add automatic spacing for
    # those impractical edge cases because doing so would reduce flexibility in
    # common one-model layouts. Users can still adjust colgap.forest.left
    # manually if one occurs.
    calcwidth.tests = options$forestLayout == "subgroup" &&
      isTRUE(model$common) &&
      isTRUE(model$random) &&
      isTRUE(extraArgs$test.effect.subgroup),
    # Single-arm models only test overall effect when a null value is specified
    test.overall = options$forestTestOverall && hasReference,
    details = options$forestDetails,
    print.I2.ci = options$forestPrintI2Ci,
    print.tau2.ci = options$forestPrintTau2Ci,
    digits = as.integer(options$digitsEffect),
    digits.pval = as.integer(options$digitsPval),
    digits.pval.Q = as.integer(options$digitsPval),
    digits.weight = as.integer(options$digitsWeight),
    digits.I2 = as.integer(options$digitsI2),
    digits.tau2 = as.integer(options$digitsTau2)
  )

  args <- c(args, extraArgs)

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


#' Prepare Forest Sort Key
#'
#' Resolves the active forest sort option to the numeric key passed to
#' `meta::forest(sortvar=)`, caches it in image state, and returns it for the
#' current run. If the image is hidden or already filled, returns the cached
#' `sortKey` from image state and does not recompute sorting.
#'
#' @param image A jamovi Image result element.
#' @param model A `meta` object (e.g., from `meta::metacont`).
#' @param sortBy Sort option name from the UI.
#' @param sortDirection Sort direction (`"asc"` or `"desc"`).
#' @param sortVariable External data variable selected for sorting, if any.
#' @param data Analysis data frame, used when sorting by a data column.
#' @return A numeric sort key, or `NULL` for original ascending order.
#' @noRd
prepareForestSortKey <- function(
  image,
  model,
  sortBy,
  sortDirection,
  sortVariable,
  data
) {
  if (!image$visible || image$isFilled()) {
    return(image$state$sortKey)
  }

  if ((sortBy == "none" && sortDirection == "asc") || is.null(model)) {
    image$setState(list(sortKey = NULL))
    return()
  }

  sortValue <- switch(
    sortBy,
    none = seq_along(model$TE),
    effect = if (
      identical(model$sm, "VE") &&
        isTRUE(model$backtransf)
    ) {
      # meta displays VE as 100 * (1 - exp(TE)), which decreases as TE
      # increases. Negating TE therefore gives the same displayed-scale
      # ordering without calculating the complete back-transformation.
      -model$TE
    } else {
      model$TE
    },
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

  image$setState(list(sortKey = sortKey))
  sortKey
}


#' Update and Cache Forest Plot Dimensions
#'
#' Measures layout dimensions for a dynamic forest plot during `.run()`, applies
#' them to the image via `setSize()`, and stores them in a hidden size cache for
#' `.postInit()` restoration.
#'
#' For standalone plots (main, leave-one-out, cumulative), `sizeCacheKey` is
#' `NULL` and dimensions are stored directly as `list(w, h)`.
#'
#' For subgroup analyses, multiple plots share a single top-level cache. Passing
#' the subgroup variable name as `sizeCacheKey` stores dimensions in a shared
#' named list (e.g. `list(Country = list(w, h), Age = list(w, h))`). This
#' preserves the cached dimensions of other subgroup variables while updating
#' the entry for the current variable.
#'
#' @param image A jamovi Image result element (e.g., `self$results$plot`).
#' @param model A `meta` object. Sizing is skipped if `NULL`.
#' @param sizeCache A hidden Group result element with `clearWith: []` used to
#'   persist dimensions across engine requests.
#' @param renderCall A zero-argument closure that renders the forest plot.
#' @param sizeCacheKey Optional string specifying the subgroup variable name.
#'   When `NULL`, dimensions are stored directly in `sizeCache$state`. When
#'   provided, dimensions are stored under this key in a shared dictionary.
#' @return `NULL` invisibly. Called for side effects (`setSize`, `setState`).
#' @noRd
updateForestSize <- function(
  image,
  model,
  sizeCache,
  renderCall,
  sizeCacheKey = NULL
) {
  if (!image$visible || image$isFilled() || is.null(model)) {
    return(invisible(NULL))
  }

  dims <- calcForestDims(renderCall = renderCall)
  w <- dims$width * 72
  h <- dims$height * 72
  image$setSize(width = w, height = h)

  size <- list(w = w, h = h)

  # Standalone plots store a single size; subgroup analyses share one cache
  # keyed by variable
  if (is.null(sizeCacheKey)) {
    sizeCache$setState(size)
  } else {
    # Subgroup plots share a single top-level cache holding a named list of
    # dimensions keyed by variable name. Retrieve the existing cache, update the
    # entry for the current variable, and save it back so other subgroup plots
    # are preserved.
    cachedSizes <- sizeCache$state
    cachedSizes[[sizeCacheKey]] <- size
    sizeCache$setState(cachedSizes)
  }

  invisible(NULL)
}


#' Apply Cached Plot Dimensions
#'
#' Shared `.postInit()` helper. Restores dynamic plot dimensions from a hidden
#' `clearWith: []` cache element whenever the image is visible. See
#' `.postInit()` in `rob.b.R` for details on the lifecycle rationale.
#'
#' For subgroup analyses, passing `sizeCacheKey` retrieves variable-specific
#' dimensions from a shared dictionary.
#'
#' @param image An Image result element (e.g., `self$results$plot`).
#' @param sizeCache A Group result element with `clearWith: []` containing
#'   cached dimensions.
#' @param sizeCacheKey Optional string specifying the subgroup variable name
#'   used to look up dimensions from a shared dictionary.
#' @return `NULL` invisibly. Called for side effects (`setSize`).
#' @noRd
applyCachedSize <- function(image, sizeCache, sizeCacheKey = NULL) {
  size <- sizeCache$state

  # Retrieve variable-specific dimensions from the shared subgroup cache.
  # Subgroup sizes are keyed by variable name so reordering or removing Array
  # items cannot associate a cached size with a different subgroup variable.
  # Entries for removed or renamed variables are intentionally retained: they
  # contain only dimensions, and pruning them would add complexity for
  # negligible benefit. If a name is reused, the old size is temporary and used
  # initially instead of the default size, then replaced in .run().
  if (!is.null(sizeCacheKey)) {
    size <- size[[sizeCacheKey]]
  }

  if (!is.null(size) && image$visible) {
    image$setSize(size$w, size$h)
  }

  invisible(NULL)
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
