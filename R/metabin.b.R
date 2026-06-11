metaBinClass <- R6::R6Class(
  "metaBinClass",
  inherit = metaBinBase,

  active = list(
    model = function() {
      if (isFALSE(private$.model)) {
        private$.model <- computeBinModel(self)
      }
      private$.model
    },

    subgroupModels = function() {
      if (isFALSE(private$.subgroupModels)) {
        private$.subgroupModels <- computeBinSubgroupModels(self)
      }
      private$.subgroupModels
    },

    metaRegModels = function() {
      if (isFALSE(private$.metaRegModels)) {
        private$.metaRegModels <- computeMetaRegModels(self)
      }
      private$.metaRegModels
    },

    leaveOneOutModel = function() {
      if (isFALSE(private$.leaveOneOutModel)) {
        private$.leaveOneOutModel <- computeLeaveOneOutModel(self)
      }
      private$.leaveOneOutModel
    },

    trimFillModel = function() {
      if (isFALSE(private$.trimFillModel)) {
        private$.trimFillModel <- computeTrimFillModel(self)
      }
      private$.trimFillModel
    }
  ),

  private = list(
    .model = FALSE,
    .subgroupModels = FALSE,
    .metaRegModels = FALSE,
    .leaveOneOutModel = FALSE,
    .trimFillModel = FALSE,
    .requiredVars = c("eventE", "nE", "eventC", "nC"),

    .init = function() {
      initSubgroupModels(self, private$.requiredVars)
      initMetaRegModels(self, private$.requiredVars)

      initText(
        self$results$text,
        self$options,
        private$.requiredVars,
        "Overall Summary"
      )
      initText(
        self$results$leaveOneOutText,
        self$options,
        private$.requiredVars,
        "Leave-One-Out Summary"
      )
      initText(
        self$results$asymmetryTestText,
        self$options,
        private$.requiredVars,
        getAsymmetryTestTitle(self$options$asymmetryMethod)
      )
      initText(
        self$results$trimFillText,
        self$options,
        private$.requiredVars,
        "Trim & Fill Summary"
      )
      initText(
        self$results$lfkIndexText,
        self$options,
        private$.requiredVars,
        "LFK Index"
      )
    },

    .postInit = function() {
      applyCachedSize(self$results$plot, self$results$plotSizeCache)

      for (i in seq_along(self$options$subgroupVariables)) {
        group <- self$results$subgroupModels$get(key = i)
        applyCachedSize(group$subgroupPlot, group$subgroupPlotSizeCache)
      }

      applyCachedSize(
        self$results$leaveOneOutPlot,
        self$results$leaveOneOutPlotSizeCache
      )
    },

    .run = function() {
      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return()
      }

      collector <- newCollector()
      runSafe(
        {
          updateForestSize(
            image = self$results$plot,
            model = self$model,
            sizeCache = self$results$plotSizeCache,
            renderCall = function() renderBinForest(self)
          )
          for (i in seq_along(self$options$subgroupVariables)) {
            group <- self$results$subgroupModels$get(key = i)
            updateForestSize(
              image = group$subgroupPlot,
              model = self$subgroupModels[[i]],
              sizeCache = group$subgroupPlotSizeCache,
              renderCall = function() renderBinSubgroupForest(self, key = i)
            )
          }
          updateForestSize(
            image = self$results$leaveOneOutPlot,
            model = self$leaveOneOutModel,
            sizeCache = self$results$leaveOneOutPlotSizeCache,
            renderCall = function() renderLeaveOneOutForest(self)
          )

          populateMainText(self)
          populateSubgroupTexts(self)
          populateMetaRegTexts(self)
          populateLeaveOneOutText(self)
          populateAsymmetryTestText(self)
          populateTrimFillText(self)
          populateLfkIndexText(self)
        },
        collector
      )
      displayNotices(self, collector)
    },

    .forestPlot = function(image, ...) {
      renderBinForest(self)
    },

    .subgroupForestPlot = function(image, ...) {
      renderBinSubgroupForest(self, key = image$parent$key)
    },

    .bubblePlot = function(image, ...) {
      renderBubblePlot(self, key = image$parent$key)
    },

    .leaveOneOutForestPlot = function(image, ...) {
      renderLeaveOneOutForest(self)
    },

    .funnelPlot = function(image, ...) {
      renderFunnelPlot(self)
    },

    .asymmetryPlot = function(image, ...) {
      renderAsymmetryPlot(self)
    },

    .trimFillFunnelPlot = function(image, ...) {
      renderTrimFillFunnelPlot(self)
    },

    .doiPlot = function(image, ...) {
      renderDoiPlot(self)
    }
  )
)
