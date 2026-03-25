#' Update Publication Bias Result Visibility
#'
#' Sets visibility of publication bias results based on per-output
#' checkboxes. Called from `.init()` to avoid flashing.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updatePubBiasVisibility <- function(options, results) {
  results$funnelPlotImage$setVisible(options$funnelPlot)
}


#' Render Funnel Plot for Publication Bias
#'
#' Draws a standard or contour-enhanced funnel plot using `meta::funnel()`.
#' When contour-enhanced, a legend is added manually with metafor-style
#' p-value labels (since `meta::funnel()` does not draw its own legend).
#'
#' @param model A `meta` object.
#' @param options Jamovi options object.
#' @noRd
renderFunnelPlot <- function(model, options) {
  if (options$funnelContour) {
    fun <- meta::funnel(model,
      type     = "contour",
      studlab  = options$funnelStudlab
    )

    if (options$funnelLegend) {
      legend(options$funnelLegendPos,
        legend = fun$text.contour,
        fill   = fun$col.contour,
        bg     = "white"
      )
    }
  } else {
    meta::funnel(model,
      studlab = options$funnelStudlab
    )
  }
}
