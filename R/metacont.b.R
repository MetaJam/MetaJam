metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,

  active = list(
    model = function() {
      if (isFALSE(private$.model)) {
        private$.model <- computeContModel(self)
      }
      private$.model
    },

    subgroupModel = function() {
      if (isFALSE(private$.subgroupModel)) {
        private$.subgroupModel <- computeContSubgroupModel(self)
      }
      private$.subgroupModel
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
    }
  ),

  private = list(
    .model = FALSE,
    .subgroupModel = FALSE,
    .metaRegModels = FALSE,
    .leaveOneOutModel = FALSE,
    .requiredVars = c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),

    .init = function() {
      initMetaRegModels(
        self$results$metaRegModels,
        self$options,
        private$.requiredVars
      )

      # Initialize text skeletons
      initMainText(
        self$results$text,
        self$options,
        private$.requiredVars
      )
      initSubgroupText(
        self$results$subgroupText,
        self$options,
        private$.requiredVars
      )
      initLeaveOneOutText(
        self$results$leaveOneOutText,
        self$options,
        private$.requiredVars
      )
      initAsymmetryTestText(
        self$results$asymmetryTestText,
        self$options,
        private$.requiredVars
      )
    },

    .postInit = function() {
      # Apply cached dimensions for plots preserved by clearWith.
      applyCachedSize(self$results$plot, self$results$plotSizeCache)
      applyCachedSize(
        self$results$subgroupPlot,
        self$results$subgroupPlotSizeCache
      )
      applyCachedSize(
        self$results$leaveOneOutPlot,
        self$results$leaveOneOutPlotSizeCache
      )
    },

    .run = function() {
      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return()
      }

      # Compute forest plot dimensions and cache for .postInit()
      updateForestSize(
        self = self,
        image = self$results$plot,
        model = self$model,
        sizeCache = self$results$plotSizeCache,
        renderFn = renderContForest
      )

      updateForestSize(
        self = self,
        image = self$results$subgroupPlot,
        model = self$subgroupModel,
        sizeCache = self$results$subgroupPlotSizeCache,
        renderFn = renderContSubgroupForest
      )

      updateForestSize(
        self = self,
        image = self$results$leaveOneOutPlot,
        model = self$leaveOneOutModel,
        sizeCache = self$results$leaveOneOutPlotSizeCache,
        renderFn = renderLeaveOneOutForest
      )

      populateMainText(self$results$text, self$model)
      populateSubgroupText(
        self$results$subgroupText,
        self$subgroupModel
      )
      populateMetaRegTexts(
        self$results$metaRegModels,
        self$metaRegModels,
        self$options
      )

      populateLeaveOneOutText(
        self$results$leaveOneOutText,
        self$leaveOneOutModel
      )
      populateAsymmetryTestText(
        self$results$asymmetryTestText,
        self$model,
        self$options
      )
    },

    .forestPlot = function(image, ...) {
      renderContForest(self)
    },

    .subgroupForestPlot = function(image, ...) {
      renderContSubgroupForest(self)
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
    }
  )
)