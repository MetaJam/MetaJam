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

  # Sort variable
  sortvar <- NULL
  if (options$sortBy != "none") {
    sortvar <- switch(
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

  meta::forest(
    model,
    new = FALSE,
    layout = options$forestLayout,
    label.e = options$labelE,
    label.c = options$labelC,
    label.left = options$labelLeft,
    label.right = options$labelRight,
    sortvar = sortvar,
    ...
  )
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
calcForestHeight <- function(model, options, ...) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  res <- renderForest(model, options, ...)
  res$figheight$total_height * 72
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
initForestPlot <- function(image, model, options, width = 800, ...) {
  if (image$isFilled()) {
    return()
  }
  if (!image$visible) {
    return()
  }
  if (is.null(model)) {
    return()
  }

  height <- calcForestHeight(model, options, ...)
  image$setSize(width = width, height = height)
}