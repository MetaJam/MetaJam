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
      contour_fills <- c("white", fun$col.contour)

      legend(
        options$funnelLegendPos,
        legend = contour_labels,
        fill = contour_fills,
        bg = "white",
        cex = options$funnelLegendCex / 100
      )
    }
  } else {
    meta::funnel(model, studlab = options$funnelStudyLabel)
  }

  TRUE
}


#' Get the Publication Bias Test Title
#'
#' Maps the selected asymmetry method to the exact title used by the meta package.
#'
#' @param method The asymmetry method string.
#' @return A character string representing the test title.
#' @noRd
getAsymmetryTestTitle <- function(method) {
  if (method == "Begg") {
    return("Rank correlation test of funnel plot asymmetry")
  }
  return("Linear regression test of funnel plot asymmetry")
}


#' Initialize the Asymmetry Test Text Skeleton
#'
#' Called from `.init()` to show a titled HTML placeholder before the
#' model is available. Follows the same pattern as other init functions.
#'
#' @param textResult Html result element.
#' @param options The `self$options` object.
#' @param requiredVars Character vector of required option names.
#' @noRd
initAsymmetryTestText <- function(textResult, options, requiredVars) {
  if (textResult$visible && !hasRequiredVars(options, requiredVars)) {
    title <- getAsymmetryTestTitle(options$asymmetryMethod)
    textResult$setContent(
      asHtml(title = title)
    )
  }
}


#' Populate the Asymmetry Test Summary
#'
#' Called from `.run()` when the model is available. Runs
#' `meta::metabias()` and renders the output as HTML. Handles Pustejovsky/SMD
#' validation.
#'
#' @param textResult Html result element.
#' @param model A `meta` object.
#' @param options Jamovi options object.
#' @noRd
populateAsymmetryTestText <- function(textResult, model, options) {
  if (!textResult$visible || textResult$isFilled()) {
    return()
  }

  methodBias <- options$asymmetryMethod

  # Pustejovsky is designed for SMD only
  if (methodBias == "Pustejovsky" && options$sm != "smd") {
    jmvcore::reject(
      "The Pustejovsky and Rodgers test is designed exclusively for the standardised mean difference (SMD)." # nolint
    )
  }

  # Run metabias — subgroup constraint is handled internally by meta,
  # which returns a warning (not an error), so we capture it cleanly
  biasResult <- meta::metabias(
    model,
    method.bias = methodBias,
    k.min = 3
  )

  title <- getAsymmetryTestTitle(methodBias)

  textResult$setContent(
    asHtml(
      print(biasResult),
      title = title,
      modifier = function(out) {
        if (
          length(out) > 0 && grepl("test of funnel plot asymmetry$", out[1])
        ) {
          return(out[-c(1, 2)])
        }
        return(out)
      }
    )
  )
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

  # Pustejovsky does not support plotit
  if (methodBias == "Pustejovsky") {
    return(FALSE)
  }

  meta::metabias(
    model,
    method.bias = methodBias,
    plotit = TRUE,
    k.min = 3
  )

  TRUE
}