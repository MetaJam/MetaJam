robClass <- R6::R6Class(
  "robClass",
  inherit = robBase,

  private = list(
    .postInit = function() {
      image <- self$results$trafficPlot
      size <- self$results$trafficPlotSizeCache$state

      # Each request creates a new image at the YAML/default size; jamovi
      # restores result state but not the dimensions. Reapply the last
      # calculated size on every request. If it is still correct, it remains in
      # use. If clearWith cleared the plot, the old size is kept until .run()
      # calculates, applies, and caches the current size. This avoids changing
      # first from the old size to the YAML/default size and then changing again
      # to the current size; the plot changes only once, directly from the old
      # size to the current size.
      if (image$visible && !is.null(size)) {
        image$setSize(size$width, size$height)
      }
    },

    .run = function() {
      summaryImage <- self$results$summaryPlot
      trafficImage <- self$results$trafficPlot

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

      if (!needsSummary && !needsTraffic) {
        return(invisible(NULL))
      }

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
          weight = "rob2Weight"
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
          overall = "rob2ClusterOverall"
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
          weight = "robinsIWeight"
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
          weight = "robinsEWeight"
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
          weight = "quadas2Weight"
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
          weight = "quipsWeight"
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

      if (needsSummary) {
        summaryData <- data
        weighted <- !is.null(self$options[[toolSpec$weight]])

        if (weighted) {
          summaryData$Weight <- jmvcore::toNumeric(
            self$data[[self$options[[toolSpec$weight]]]]
          )
        }

        summaryImage$setState(list(
          data = summaryData,
          tool = self$options$tool,
          overall = overall,
          weighted = weighted,
          colour = self$options$colour
        ))
      }

      if (needsTraffic) {
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

        trafficImage$setSize(width, height)
        self$results$trafficPlotSizeCache$setState(list(
          width = width,
          height = height
        ))
        trafficImage$setState(list(
          data = data,
          tool = self$options$tool,
          overall = overall,
          colour = self$options$colour,
          pointSize = self$options$pointSize
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
    }
  )
)
