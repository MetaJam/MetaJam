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
    # GLMM does not compute a Tau² confidence interval. Explicitly force it
    # FALSE here to align with the disabled UI.
    print.tau2.ci = options$forestPrintTau2Ci &&
      model$method != "GLMM",
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
    effect = if (!isTRUE(model$backtransf)) {
      model$TE
    } else if (inherits(model, "metaprop")) {
      # forest.meta() displays the observed event / n for every metaprop study
      # instead of back-transforming TE. This is necessary for PFT, whose
      # transformation depends on each study's n, and for PLN/PLOGIT when a
      # continuity correction makes backtransf(TE) differ from event / n.
      # Applying the rule to every metaprop measure exactly follows upstream
      # and is also valid for PRAW and PAS, whose ordering already agrees with
      # TE. Source: meta/R/forest.R lines 4743-4745 and 6816-6817.
      model$event / model$n
    } else if (inherits(model, "metarate")) {
      # forest.meta() likewise displays the observed event / time for every
      # metarate study instead of back-transforming TE. This is necessary for
      # IRFT, whose transformation depends on exposure time, and for IR/IRLN
      # when a continuity correction makes TE represent a corrected rate.
      # Applying the rule to every metarate measure exactly follows upstream
      # and is also valid when transformed and observed rate orderings agree.
      # Source: meta/R/forest.R lines 4766-4769 and 6818-6819.
      model$event / model$time
    } else if (inherits(model, "metainf") && identical(model$sm, "PFT")) {
      # metainf rows are pooled leave-one-out estimates, not observed studies,
      # so no single event / n is available. For PFT, forest.meta()
      # back-transforms each row using its own harmonic n; this can change the
      # ordering of TE. PRAW, PAS, PLN, and PLOGIT use fixed monotonic
      # back-transformations for leave-one-out estimates, so their displayed
      # ordering agrees with TE. Source: meta/R/forest.R lines 6793-6801 and
      # 6820-6821; meta/R/meta-transf.R lines 219-223.
      meta::asin2p(model$TE, model$n.harmonic.mean)
    } else if (inherits(model, "metainf") && identical(model$sm, "IRFT")) {
      # metainf rows are pooled leave-one-out estimates, not observed studies,
      # so no single event / time is available. For IRFT, forest.meta()
      # back-transforms each row using its own harmonic exposure time; this can
      # change the ordering of TE. IR, IRS, and IRLN preserve TE ordering for
      # leave-one-out estimates because their inverses are fixed and monotonic.
      # Source: meta/R/forest.R lines 6793-6801 and 6820-6821;
      # meta/R/meta-transf.R lines 229-230.
      meta::asin2ir(model$TE, model$t.harmonic.mean)
    } else if (identical(model$sm, "VE")) {
      # meta displays VE as 100 * (1 - exp(TE)), which decreases as TE
      # increases. Negating TE therefore gives the same displayed-scale
      # ordering without calculating the full back-transformation.
      # Source: meta/R/meta-transf.R lines 382-383.
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
#' Measures the forest plot on a non-rendering ragg device and extracts
#' width and height from `meta`'s internal grid layout.
#'
#' `meta::forest()` constructs a [grid::grid.layout()] with exact
#' column widths (measured from text grobs) and uniform row heights.
#' The `figheight` value returned by `meta::forest()` is only a
#' heuristic row-count estimate (via the internal `gh()` function)
#' used to size file devices before the layout exists; the grid
#' layout captured here supplies the dimensions for the measuring device.
#'
#' ## Why `ragg` is the Single Source of Truth
#' In jamovi, dynamic per-device sizing is impossible: `Image$setSize()`
#' accepts only a single logical size (in 72 units per inch) that jamovi
#' reuses for both live rendering and all export formats (`saveAs`):
#' - Live display: `ragg::agg_png` at screen `ppi`
#' - PNG export: `ragg::agg_png` at 144 PPI
#' - PDF export: `grDevices::cairo_pdf`
#' - SVG export: `grDevices::svg`
#' - EPS export: `grDevices::cairo_ps`
#' - PPTX export: `export::graph2ppt` (DrawingML via `rvg`)
#'
#' Base R's `pdf(file = NULL)` measures text with built-in PostScript Helvetica
#' AFM metrics rather than resolving the system's generic `sans` font. This
#' happened to work on Windows and macOS because Helvetica AFM is close in width
#' to the Arial and Helvetica fonts selected by jamovi's rendering devices on
#' those systems. It failed in Linux jamovi because the live ragg renderer
#' resolves generic `sans` through systemfonts/fontconfig to DejaVu Sans, which
#' measured about 13% wider than Helvetica AFM in the device experiment. The old
#' PDF measurement therefore underestimated the width required by the live plot.
#'
#' Each jamovi output format uses a graphics device, and those devices do not
#' always select the exact same default font. The experiment found:
#' - On Windows, ragg (display and PNG) uses Arial, Cairo (PDF, SVG, and EPS)
#'   uses ArialMT, and PowerPoint uses Arial.
#' - On macOS, ragg (display and PNG) and Cairo (PDF, SVG, and EPS) use
#'   Helvetica, while PowerPoint uses Arial.
#' - On the Ubuntu GitHub runner, ragg (display and PNG) and PowerPoint use
#'   DejaVu Sans, while Cairo (PDF, SVG, and EPS) uses the narrower Nimbus Sans.
#'   In the official jamovi Docker image, Cairo (PDF, SVG, and EPS) instead uses
#'   DejaVu Sans because that image has a different set of installed fonts.
#'
#' MetaJam measures with ragg because jamovi uses it for the displayed plot and
#' PNG export, which are the main output path. Across the tested platforms, most
#' other export devices used the same default sans font as ragg or a metrically
#' similar counterpart (such as Arial, ArialMT, and Helvetica). The remaining
#' devices used narrower fonts, so the ragg measurement plus the existing +0.3
#' inch width padding also contained those outputs. The experiment found no
#' new-size containment failures on Windows, Ubuntu, or macOS.
#'
#' ## Layout Spacing Note (RevMan Layout on Linux)
#' In continuous outcomes meta-analysis with `layout = "RevMan5"`, the default
#' 2 mm `colgap.forest.left` can cause the '95% CI' column header to touch the
#' 'IV, Fixed...' forest header in Linux jamovi because DejaVu Sans is wider
#' than the Arial and Helvetica defaults used on Windows and macOS. We keep the
#' 2 mm default to preserve the compact layout on those systems. Linux users can
#' adjust spacing in the Dimensions tab, where the UI includes an explicit note:
#' "If text overlaps in the forest plot, adjust spacing in the Dimensions
#' tab".
#'
#' ## Risk of Bias Traffic Light Plot Footnote Overlap on Linux
#' `robvis:::get_width()` estimates width from character counts rather than font
#' metrics. In Linux jamovi, the wider DejaVu Sans can therefore make
#' long domain footnotes collide with the legend. This occurred for ROB2,
#' ROB2-Cluster, ROBINS-I, and ROBINS-E in testing, but not for the shorter
#' QUADAS-2 and QUIPS text. Dynamic font measurement is not justified for this
#' secondary plot; users can widen it when the overlap occurs.
#'
#' ## Future `forestploter` Integration Note
#' `forestploter::forest()` eagerly evaluates unit conversions (e.g. axis
#' height, arrow width, title height) during plot construction, and
#' `forestploter::get_wh()` queries the ambient device. When adding
#' `forestploter`, the entire sequence:
#' 1. Open `ragg::agg_record()`,
#' 2. Construct `forestploter::forest(...)` inside that device,
#' 3. Call `forestploter::get_wh()` on that object,
#' 4. Close the device,
#' must execute within the single `ragg` session to guarantee consistent
#' metrics.
#'
#' @param renderCall A zero-argument closure that renders the forest plot.
#' @return A list with `width` and `height` in inches.
#' @noRd
calcForestDims <- function(renderCall) {
  oldDev <- grDevices::dev.cur()

  # Open a non-rendering ragg device to query systemfonts/textshaping metrics
  # without allocating an in-memory pixel buffer or creating temporary files.
  ragg::agg_record()
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  # Print devices normally initialize the graphics-engine display list with
  # recording OFF. ragg::agg_record() is different: it deliberately initializes
  # that display list with recording ON so graphics can be captured later with
  # recordPlot(). This function does not use recordPlot(). grid::grid.grab()
  # reads grid's separate display list, which remains available when the
  # graphics-engine list is inhibited. Turning off the unused engine list here
  # avoids recording a second copy of the drawing operations in memory.
  grDevices::dev.control(displaylist = "inhibit")

  renderCall()
  gtree <- grid::grid.grab()

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
