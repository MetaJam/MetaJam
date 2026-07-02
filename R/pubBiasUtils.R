#' Compute Trim and Fill Model
#'
#' Analysis-agnostic: works with any `meta` object. The trim-and-fill object is
#' a plain list (class metagen/meta/trimfill), `trimfill()` internally sets
#' `$call <- NULL`, so there are no environment/closure leaks — `stripModel()`
#' is not needed.
#'
#' @param self The jamovi `self` object.
#' @return The trim and fill model or NULL if base model is not ready.
#' @noRd
computeTrimFillModel <- function(self) {
  # Cross-cycle cache (restored via clearWith)
  cached <- self$results$trimFillText$state
  if (!is.null(cached)) {
    return(cached)
  }

  if (is.null(self$model)) {
    return()
  }

  options <- self$options

  side <- if (options$trimFillSide == "auto") {
    NULL
  } else if (options$trimFillSide == "left") {
    TRUE
  } else if (options$trimFillSide == "right") {
    FALSE
  }

  tf_model <- meta::trimfill(
    self$model,
    left = side,
    type = options$trimFillEstimator
  )

  # Cache for next cycle
  self$results$trimFillText$setState(tf_model)
  tf_model
}


#' Populate the Asymmetry Test Summary
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or model is NULL. We use the NULL check of the model here across our
#' module mainly as a proxy that required variables are available, which
#' we already verified in `.run()` before reaching this line. Although
#' redundant, we keep it for clarity.
#' Runs `meta::metabias()` and renders the output as HTML. Handles
#' Pustejovsky/SMD and Deeks/DOR validation.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateAsymmetryTestText <- function(self) {
  textResult <- self$results$asymmetryTestText
  options <- self$options
  methodBias <- options$asymmetryMethod

  if (!textResult$visible || textResult$isFilled() || is.null(self$model)) {
    return(invisible(NULL))
  }

  if (methodBias == "Pustejovsky" && options$sm != "smd") {
    jmvcore::reject(
      "The Pustejovsky and Rodgers test is only available when the effect measure is standardised mean difference (SMD)." # nolint
    )
  }

  if (methodBias == "Deeks" && options$sm != "DOR") {
    jmvcore::reject(
      "The Deeks test is only available when the effect measure is diagnostic odds ratio (DOR)." # nolint
    )
  }

  # Run metabias — subgroup constraint is handled internally by meta,
  # which returns a warning (not an error), so we capture it cleanly
  args <- list(x = self$model, k.min = 3)
  if (methodBias != "auto") {
    args$method.bias <- methodBias
  }
  biasResult <- do.call(meta::metabias, args)

  title <- getAsymmetryTestTitle(
    if (length(biasResult$method.bias) == 1) {
      biasResult$method.bias
    } else {
      methodBias
    }
  )

  textResult$setContent(
    asHtml(
      print(biasResult),
      title = title,
      modifier = function(out) {
        if (
          length(out) > 1 &&
            (grepl("test of funnel plot asymmetry$", out[1]) ||
              grepl("diagnostic odds ratios$", out[1]))
        ) {
          return(out[-c(1, 2)])
        }

        out
      }
    )
  )

  invisible(NULL)
}


#' Populate the Trim and Fill Summary
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled, or model is NULL.
#' Renders the rich summary of the trim and fill model as HTML.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateTrimFillText <- function(self) {
  textResult <- self$results$trimFillText

  if (
    !textResult$visible || textResult$isFilled() || is.null(self$trimFillModel)
  ) {
    return(invisible(NULL))
  }

  textResult$setContent(
    asHtml(summary(self$trimFillModel), title = "Trim & Fill Summary")
  )

  invisible(NULL)
}


#' Populate the LFK Index Summary
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled, or model is NULL.
#' Runs `metasens::lfkindex()` and renders the output as HTML.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateLfkIndexText <- function(self) {
  textResult <- self$results$lfkIndexText

  if (!textResult$visible || textResult$isFilled() || is.null(self$model)) {
    return(invisible(NULL))
  }

  lfk <- metasens::lfkindex(self$model)

  textResult$setContent(
    asHtml(
      print(lfk),
      title = "LFK Index",
      modifier = function(out) {
        if (length(out) > 0 && trimws(out[1]) == "LFK index test") {
          out[-c(1, 2)]
        } else {
          out
        }
      }
    )
  )

  invisible(NULL)
}


#' Render Funnel Plot for Publication Bias
#'
#' Draws a standard or contour-enhanced funnel plot using `meta::funnel()`.
#' When contour-enhanced, a legend is added with custom p-value labels
#' using correct statistical notation (strict `<` on lower bound,
#' `\u2264` on upper bound), including the white non-significant region.
#'
#' @param self The jamovi `self` object.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderFunnelPlot <- function(self) {
  model <- self$model
  options <- self$options

  if (is.null(model)) {
    return(FALSE)
  }

  if (options$funnelContour) {
    fun <- meta::funnel(
      model,
      type = "contour",
      studlab = options$funnelStudyLabel
    )

    if (options$funnelLegend) {
      # Custom labels: strict < on lower bound, <= on upper bound,
      # matching meta's rendering (boundary -> more-significant band)
      contour_labels <- c(
        "p > 0.10",
        "0.05 < p \u2264 0.10",
        "0.01 < p \u2264 0.05",
        "p \u2264 0.01"
      )
      contour_colors <- c("white", fun$col.contour)

      # We use pch=22 (filled square symbol) instead of fill= so that
      # the legend style matches the trim-and-fill funnel plot, which
      # must use pch= to combine contour squares with study-shape
      # circles in a single legend call. This is consistent with how
      # metafor renders its funnel legend (pch=22, pt.cex=2).
      # col= sets the square border color (with fill=, the border
      # was black by default via the `border` param).
      # By default, pt.cex = cex and scales automatically. Since we explicitly
      # override pt.cex, it will not automatically scale with cex. Therefore,
      # we must manually multiply the custom pt.cex values by the scale factor
      # (cex) to keep symbols proportional and avoid box overflow.
      scale <- options$funnelLegendCex / 100
      legend(
        options$funnelLegendPos,
        legend = contour_labels,
        pch = rep(22, 4),
        col = rep("black", 4),
        pt.bg = contour_colors,
        pt.cex = rep(2, 4) * scale,
        bg = "white",
        cex = scale,
        # Add slight breathing room (default is 1.0)
        x.intersp = 1.2,
        y.intersp = 1.2
      )
    }
  } else {
    meta::funnel(model, studlab = options$funnelStudyLabel)
  }

  TRUE
}


#' Render the Asymmetry Test Plot
#'
#' Draws the radial/scatter plot produced by `meta::metabias(plotit = TRUE)`.
#' Handles the same validations as `populateAsymmetryTestText`.
#'
#' @param self The jamovi `self` object.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderAsymmetryPlot <- function(self) {
  model <- self$model
  methodBias <- self$options$asymmetryMethod

  if (is.null(model)) {
    return(FALSE)
  }

  if (
    !(methodBias %in%
      c("auto", "Egger", "Begg", "Thompson", "Harbord", "Deeks"))
  ) {
    return(FALSE)
  }

  if (methodBias == "Deeks" && self$options$sm != "DOR") {
    jmvcore::reject(
      "The Deeks test is only available when the effect measure is diagnostic odds ratio (DOR)." # nolint
    )
  }

  args <- list(x = model, plotit = TRUE, k.min = 3)
  if (methodBias != "auto") {
    args$method.bias <- methodBias
  }
  do.call(meta::metabias, args)

  TRUE
}


#' Render Trim and Fill Funnel Plot
#'
#' Draws a funnel plot for the trim-and-fill model, showing both original and
#' imputed (filled) studies. Builds a dynamic legend that always shows the
#' shape distinction (observed vs imputed), and prepends contour-band items
#' when contour-enhanced mode is active. Uses `pch` for all legend items
#' (squares for contour bands, circles for study shapes) so they align on the
#' same column.
#'
#' @param self The jamovi `self` object.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderTrimFillFunnelPlot <- function(self) {
  model <- self$trimFillModel
  if (is.null(model)) {
    return(FALSE)
  }

  opts <- self$options

  if (opts$trimFillFunnelContour) {
    fun <- meta::funnel(
      model,
      type = "contour",
      studlab = opts$trimFillFunnelStudyLabel
    )
  } else {
    meta::funnel(model, studlab = opts$trimFillFunnelStudyLabel)
  }

  if (opts$trimFillFunnelLegend) {
    # Shape items: always present for trim-and-fill plots
    shape_labels <- c("Observed Studies", "Imputed Studies")
    shape_pch <- c(21, 1)
    shape_col <- c("black", "black")
    shape_pt_bg <- c("darkgray", NA)

    if (opts$trimFillFunnelContour) {
      # Combined legend: shapes + contour bands in one legend call.
      # pch=22 (filled square symbol, matching metafor's approach with
      # pt.cex=2) for contour items so all items align on the same
      # point column alongside the study-shape circles.
      # By default, pt.cex = cex and scales automatically. Since we explicitly
      # override pt.cex, it will not automatically scale with cex. Therefore,
      # we must manually multiply the custom pt.cex values by the scale factor
      # (cex) to keep symbols proportional and avoid box overflow.
      contour_labels <- c(
        "p > 0.10",
        "0.05 < p \u2264 0.10",
        "0.01 < p \u2264 0.05",
        "p \u2264 0.01"
      )
      contour_colors <- c("white", fun$col.contour)
      scale <- opts$trimFillFunnelLegendCex / 100
      legend(
        opts$trimFillFunnelLegendPos,
        legend = c(shape_labels, contour_labels),
        pch = c(shape_pch, rep(22, 4)),
        col = c(shape_col, rep("black", 4)),
        pt.bg = c(shape_pt_bg, contour_colors),
        pt.cex = c(1, 1, rep(2, 4)) * scale,
        bg = "white",
        cex = scale,
        # Add slight breathing room (default is 1.0)
        x.intersp = 1.2,
        y.intersp = 1.2
      )
    } else {
      # Shape items only
      legend(
        opts$trimFillFunnelLegendPos,
        legend = shape_labels,
        pch = shape_pch,
        col = shape_col,
        pt.bg = shape_pt_bg,
        bg = "white",
        cex = opts$trimFillFunnelLegendCex / 100,
        # Add slight breathing room (default is 1.0)
        x.intersp = 1.2,
        y.intersp = 1.2
      )
    }
  }

  TRUE
}


#' Render DOI Plot
#'
#' Draws a DOI plot using `metasens::doiplot()` with the base meta object.
#' Suppresses the built-in legend (`lfkindex = FALSE`) and renders a custom
#' `legend()` call with user-controlled position and size, following the same
#' pattern as the funnel plot legend.
#'
#' @param self The jamovi `self` object.
#' @return TRUE if the plot was successfully rendered, FALSE otherwise.
#' @noRd
renderDoiPlot <- function(self) {
  model <- self$model
  if (is.null(model)) {
    return(FALSE)
  }

  opts <- self$options

  # Calculate LFK index once
  lfk <- metasens::lfkindex(model)

  # Draw the DOI plot without its internal legend
  metasens::doiplot(lfk, lfkindex = FALSE)

  # Add our own legend with size control
  if (opts$doiPlotLegend) {
    legend(
      opts$doiPlotLegendPos,
      legend = paste("LFK index", round(lfk$lfkindex, 2)),
      bg = "white",
      cex = opts$doiPlotLegendCex / 100
    )
  }

  TRUE
}


#' Get the Publication Bias Test Title
#'
#' Maps the selected asymmetry method to a user-facing title that names the
#' test and its broad statistical family.
#'
#' @param method The asymmetry method string.
#' @return A character string representing the test title.
#' @noRd
getAsymmetryTestTitle <- function(method) {
  switch(
    method,
    Egger = "Egger's Linear Regression Test for Funnel Plot Asymmetry",
    Begg = "Begg and Mazumdar's Rank Correlation Test for Funnel Plot Asymmetry", # nolint
    Thompson = "Thompson and Sharp's Linear Regression Test for Funnel Plot Asymmetry", # nolint
    Harbord = "Harbord's Linear Regression Test for Funnel Plot Asymmetry",
    Peters = "Peters' Linear Regression Test for Funnel Plot Asymmetry",
    Macaskill = "Macaskill's Linear Regression Test for Funnel Plot Asymmetry",
    Schwarzer = "Schwarzer's Rank Correlation Test for Funnel Plot Asymmetry",
    # Kept for future diagnostic-test-accuracy support; not exposed in metabin.
    Deeks = "Deeks' Linear Regression Test for Funnel Plot Asymmetry",
    Pustejovsky = "Pustejovsky and Rodgers' Linear Regression Test for Funnel Plot Asymmetry", # nolint
    "Test for Funnel Plot Asymmetry"
  )
}
