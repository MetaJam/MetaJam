robClass <- R6::R6Class(
  "robClass",
  inherit = robBase,

  private = list(
    .setCombinedPlotSize = function(image, trafficWidth, trafficHeight) {
      # Under plot.tag.location = "margin", enabling panel tags (18 pt bold)
      # expands the patchwork layout by inserting a left column and two rows
      # (one per subplot) for the tag labels. We add tagWidth and tagHeight
      # so the calculated plot dimensions remain unaffected by the tags:
      # - tagHeight adds the combined height of the two tag rows (fixed at
      #   0.4722... in, as all supported styles measured the same).
      # - tagWidth adds the tag column width (dynamic across styles, as
      #   measured widths vary by tag style).
      # Values are the raw physical inches measured from the gtable layout;
      # multiplying by 72 converts them into jamovi's 72-units-per-inch image
      # scale.
      tagWidth <- switch(
        self$options$combinedTags,
        A = 0.1805555555555556 * 72,
        "1" = 0.1391059027777778 * 72,
        I = 0.1388888888888889 * 72,
        none = 0
      )
      tagHeight <- if (self$options$combinedTags == "none") {
        0
      } else {
        0.4722222222222222 * 72
      }

      image$setSize(
        max(8 * 72, trafficWidth) + tagWidth,
        2.41 * 72 + trafficHeight + tagHeight
      )
    },

    .postInit = function() {
      trafficImage <- self$results$trafficPlot
      combinedImage <- self$results$combinedPlot
      trafficSize <- self$results$trafficPlotSizeCache$state

      # Each request creates a new image at the YAML/default size; jamovi
      # restores result state but not the dimensions. Reapply the last
      # calculated size on every request. If it is still correct, it remains in
      # use. If clearWith cleared the plot, the old size is kept until .run()
      # calculates, applies, and caches the current size. This avoids changing
      # first from the old size to the YAML/default size and then changing again
      # to the current size; the plot changes only once, directly from the old
      # size to the current size.
      if (!is.null(trafficSize)) {
        if (trafficImage$visible) {
          trafficImage$setSize(trafficSize$width, trafficSize$height)
        }

        if (combinedImage$visible) {
          private$.setCombinedPlotSize(
            combinedImage,
            trafficSize$width,
            trafficSize$height
          )
        }
      }
    },

    .run = function() {
      summaryImage <- self$results$summaryPlot
      trafficImage <- self$results$trafficPlot
      combinedImage <- self$results$combinedPlot

      # Use state as a proxy for clearWith to decide whether plot preparation
      # must be recalculated. This analysis always stores non-NULL state after
      # preparing either image. clearWith removes it when plot inputs change;
      # therefore NULL state means preparation must run again. isFilled()
      # becomes FALSE for those same changes, but also when the user resizes the
      # image or changes the global theme/palette. State remains non-NULL in
      # those two rerender-only cases, so using it instead of isFilled() avoids
      # redundant data preparation and size calculation.
      needsSummary <- summaryImage$visible && is.null(summaryImage$state)
      needsTraffic <- trafficImage$visible && is.null(trafficImage$state)
      needsCombined <- combinedImage$visible && is.null(combinedImage$state)

      if (!needsSummary && !needsTraffic && !needsCombined) {
        return(invisible(NULL))
      }

      # Tool-specific judgements, validation checks, and error handling are
      # based on the robvis Shiny app. Revisit if robvis updates its tool
      # definitions.
      toolSpec <- switch(
        self$options$tool,
        "ROB2" = list(
          study = "rob2Study",
          domains = c(
            rob2D1 = "D1",
            rob2D2 = "D2",
            rob2D3 = "D3",
            rob2D4 = "D4",
            rob2D5 = "D5"
          ),
          overall = "rob2Overall",
          weight = "rob2Weight",
          judgements = c(
            "Low",
            "Some concerns",
            "High",
            "No information"
          )
        ),
        "ROB2-Cluster" = list(
          study = "rob2ClusterStudy",
          domains = c(
            rob2ClusterD1 = "D1",
            rob2ClusterD1b = "D1b",
            rob2ClusterD2 = "D2",
            rob2ClusterD3 = "D3",
            rob2ClusterD4 = "D4",
            rob2ClusterD5 = "D5"
          ),
          overall = "rob2ClusterOverall",
          judgements = c(
            "Low",
            "Some concerns",
            "High",
            "No information",
            "Not applicable"
          )
        ),
        "ROBINS-I" = list(
          study = "robinsIStudy",
          domains = c(
            robinsID1 = "D1",
            robinsID2 = "D2",
            robinsID3 = "D3",
            robinsID4 = "D4",
            robinsID5 = "D5",
            robinsID6 = "D6",
            robinsID7 = "D7"
          ),
          overall = "robinsIOverall",
          weight = "robinsIWeight",
          judgements = c(
            "Low",
            "Moderate",
            "Serious",
            "Critical",
            "No information"
          )
        ),
        "ROBINS-E" = list(
          study = "robinsEStudy",
          domains = c(
            robinsED1 = "D1",
            robinsED2 = "D2",
            robinsED3 = "D3",
            robinsED4 = "D4",
            robinsED5 = "D5",
            robinsED6 = "D6",
            robinsED7 = "D7"
          ),
          overall = "robinsEOverall",
          weight = "robinsEWeight",
          judgements = c(
            "Low",
            "Some concerns",
            "High",
            "Very high",
            "No information"
          )
        ),
        "QUADAS-2" = list(
          study = "quadas2Study",
          domains = c(
            quadas2D1 = "D1",
            quadas2D2 = "D2",
            quadas2D3 = "D3",
            quadas2D4 = "D4"
          ),
          overall = "quadas2Overall",
          weight = "quadas2Weight",
          judgements = c(
            "Low",
            "Some concerns",
            "High",
            "No information"
          )
        ),
        "QUIPS" = list(
          study = "quipsStudy",
          domains = c(
            quipsD1 = "D1",
            quipsD2 = "D2",
            quipsD3 = "D3",
            quipsD4 = "D4",
            quipsD5 = "D5",
            quipsD6 = "D6"
          ),
          overall = "quipsOverall",
          weight = "quipsWeight",
          judgements = c(
            "Low",
            "Moderate",
            "High",
            "No information"
          )
        )
      )

      if (
        !hasRequiredVars(
          self$options,
          c(toolSpec$study, names(toolSpec$domains))
        )
      ) {
        return(invisible(NULL))
      }

      data <- data.frame(
        Study = self$data[[self$options[[toolSpec$study]]]]
      )

      for (option in names(toolSpec$domains)) {
        data[[toolSpec$domains[[option]]]] <-
          self$data[[self$options[[option]]]]
      }

      overall <- !is.null(self$options[[toolSpec$overall]])
      if (overall) {
        data$Overall <-
          self$data[[self$options[[toolSpec$overall]]]]
      }

      if (anyDuplicated(data$Study) > 0) {
        jmvcore::reject("Study labels must be unique.")
      }

      judgementData <- data[setdiff(names(data), "Study")]
      judgements <- unlist(
        lapply(judgementData, as.character),
        use.names = FALSE
      )
      judgements <- trimws(tolower(judgements))

      if (any(!(judgements %in% tolower(toolSpec$judgements)))) {
        jmvcore::reject(
          paste0(
            "Judgements must be one of: ",
            paste(toolSpec$judgements, collapse = ", "),
            "."
          )
        )
      }

      if (needsSummary || needsCombined) {
        summaryData <- data
        weighted <- !is.null(self$options[[toolSpec$weight]])

        if (weighted) {
          weights <- jmvcore::toNumeric(
            self$data[[self$options[[toolSpec$weight]]]]
          )

          if (any(!is.finite(weights) | weights < 0)) {
            jmvcore::reject(
              "Weights must not be missing, infinite, or negative."
            )
          }

          if (!any(weights > 0)) {
            jmvcore::reject("At least one weight must be greater than zero.")
          }

          summaryData$Weight <- weights
        }

        summaryState <- list(
          data = summaryData,
          tool = self$options$tool,
          overall = overall,
          weighted = weighted,
          colour = self$options$colour
        )
      }

      if (needsTraffic || needsCombined) {
        # get_width() returns NA when any Study label is missing because max()
        # receives an NA character count. Use a separate sizing copy to avoid
        # that NA result without changing the original data passed to the plot.
        sizeData <- data
        sizeData$Study <- as.character(sizeData$Study)
        sizeData$Study[is.na(sizeData$Study)] <- ""

        width <- robvis:::get_width(
          data = sizeData,
          psize = self$options$pointSize,
          type = "tf"
        ) *
          72
        height <- robvis:::get_height(
          data = data,
          tool = self$options$tool,
          psize = self$options$pointSize,
          type = "tf"
        ) *
          72

        trafficState <- list(
          data = data,
          tool = self$options$tool,
          overall = overall,
          colour = self$options$colour,
          pointSize = self$options$pointSize
        )

        self$results$trafficPlotSizeCache$setState(list(
          width = width,
          height = height
        ))
      }

      if (needsSummary) {
        summaryImage$setState(summaryState)
      }

      if (needsTraffic) {
        trafficImage$setSize(width, height)
        trafficImage$setState(trafficState)
      }

      if (needsCombined) {
        private$.setCombinedPlotSize(
          combinedImage,
          width,
          height
        )

        combinedImage$setState(list(
          summary = summaryState,
          traffic = trafficState,
          order = self$options$combinedOrder,
          tags = self$options$combinedTags
        ))
      }
    },

    .summaryPlot = function(image, ...) {
      if (is.null(image$state)) {
        return(FALSE)
      }

      state <- image$state
      print(robvis::rob_summary(
        data = state$data,
        tool = state$tool,
        overall = state$overall,
        weighted = state$weighted,
        colour = state$colour
      ))
      TRUE
    },

    .trafficPlot = function(image, ...) {
      if (is.null(image$state)) {
        return(FALSE)
      }

      state <- image$state
      print(robvis::rob_traffic_light(
        data = state$data,
        tool = state$tool,
        colour = state$colour,
        psize = state$pointSize,
        overall = state$overall
      ))
      TRUE
    },

    .combinedPlot = function(image, ...) {
      if (is.null(image$state)) {
        return(FALSE)
      }

      state <- image$state
      summaryState <- state$summary
      trafficState <- state$traffic
      trafficSize <- self$results$trafficPlotSizeCache$state

      summaryPlot <- robvis::rob_summary(
        data = summaryState$data,
        tool = summaryState$tool,
        overall = summaryState$overall,
        weighted = summaryState$weighted,
        colour = summaryState$colour
      )
      trafficPlot <- robvis::rob_traffic_light(
        data = trafficState$data,
        tool = trafficState$tool,
        colour = trafficState$colour,
        psize = trafficState$pointSize,
        overall = trafficState$overall
      )

      # patchwork normally aligns grid tracks across plots in the same column.
      # The most visible issue is on the left side: the summary plot's long
      # domain labels force a shared axis alignment that creates unwanted
      # white space on the left of the traffic light plot.
      #
      # In addition, un-freed plots retain fixed outer tracks (axes, legends,
      # captions), meaning row 'heights' apply only to the relative panel area
      # and distort the final plot dimensions. Freeing both plots on all four
      # sides isolates them completely, eliminating the white space gap and
      # allowing each plot to resolve its standalone layout independently.
      summaryPlot <- patchwork::free(
        summaryPlot,
        type = "panel",
        side = "trbl"
      )
      trafficPlot <- patchwork::free(
        trafficPlot,
        type = "panel",
        side = "trbl"
      )

      # patchwork's 'heights' scale only the flexible panel area, but each freed
      # child plot still retains its default 5.5 pt top and bottom theme margins
      # as fixed tracks in the master layout.
      #
      # Because the final rendered height of each row is the sum of its fixed
      # margins plus its allocated panel height, passing raw standalone heights
      # would prevent the rows from matching their target heights and distort
      # their proportions.
      #
      # We calculate the fixed margin height in jamovi's 72-units-per-inch scale
      # (grid defines 72.27 pt per inch) and subtract it from each target height.
      # This ensures that once grid adds the fixed margins back during rendering,
      # the total height of each child row matches its exact standalone target:
      plotMarginHeight <- 2 * 5.5 * 72 / 72.27

      if (state$order == "summaryFirst") {
        plots <- list(summaryPlot, trafficPlot)
        heights <- c(
          2.41 * 72 - plotMarginHeight,
          trafficSize$height - plotMarginHeight
        )
      } else {
        plots <- list(trafficPlot, summaryPlot)
        heights <- c(
          trafficSize$height - plotMarginHeight,
          2.41 * 72 - plotMarginHeight
        )
      }

      combinedPlot <- patchwork::wrap_plots(
        plots,
        ncol = 1,
        heights = heights
      ) +
        patchwork::plot_annotation(
          tag_levels = if (state$tags == "none") {
            character()
          } else {
            state$tags
          },
          theme = ggplot2::theme(
            # Remove only the top-level patchwork margin. The original robvis
            # child margins remain intact and are accounted for in the
            # panel-row weights above.
            plot.margin = ggplot2::margin(0, 0, 0, 0)
          )
        )

      if (state$tags != "none") {
        combinedPlot <- combinedPlot &
          ggplot2::theme(
            plot.tag = ggplot2::element_text(
              # Keep panel tags clearly above robvis's 6 to 10 pt text hierarchy.
              size = 18,
              face = "bold",
              # Default is 0 margin (no extra padding around tag text)
              margin = ggplot2::margin(0, 0, 0, 0)
            ),
            # Default is "topleft"
            plot.tag.position = "topleft",
            # Default is "margin" for named tag positions
            plot.tag.location = "margin"
          )
      }

      print(combinedPlot)
      TRUE
    }
  )
)
