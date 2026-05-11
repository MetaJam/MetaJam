metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,

  active = list(
    model = function() {
      if (is.null(private$.model)) {
        private$.model <- getCachedModel(
          self$results$text,
          computeContModel(self)
        )
      }
      private$.model
    },

    subgroupModel = function() {
      if (is.null(private$.subgroupModel)) {
        private$.subgroupModel <- getCachedModel(
          self$results$subgroupText,
          computeContSubgroupModel(self)
        )
      }
      private$.subgroupModel
    },

    metaRegModels = function() {
      if (is.null(private$.metaRegModels)) {
        private$.metaRegModels <- computeMetaRegModels(
          self$data,
          self$model,
          self$options
        )
      }
      private$.metaRegModels
    },

    leaveOneOutModel = function() {
      if (is.null(private$.leaveOneOutModel)) {
        private$.leaveOneOutModel <- getCachedModel(
          self$results$leaveOneOutText,
          computeLeaveOneOutModel(self$model, self$options)
        )
      }
      private$.leaveOneOutModel
    }
  ),

  private = list(
    .model = NULL,
    .subgroupModel = NULL,
    .metaRegModels = NULL,
    .leaveOneOutModel = NULL,
    .requiredVars = c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),

    .init = function() {
      initMetaRegModelItems(self$options, self$results)
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
      # Initialize text skeletons (before hasRequiredVars so placeholders
      # show even when variables aren't assigned yet). isFilled() guards
      # inside these functions skip when clearWith didn't invalidate.
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

      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return()
      }

      # Compute forest plot dimensions and cache for .postInit()
      updateForestSize(
        image = self$results$plot,
        model = self$model,
        options = self$options,
        sizeCache = self$results$plotSizeCache
      )

      updateForestSize(
        image = self$results$subgroupPlot,
        model = self$subgroupModel,
        options = self$options,
        sizeCache = self$results$subgroupPlotSizeCache,
        renderFn = renderContSubgroupForest
      )

      updateForestSize(
        image = self$results$leaveOneOutPlot,
        model = self$leaveOneOutModel,
        options = self$options,
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
      if (is.null(self$model)) {
        return(FALSE)
      }
      renderContForest(self$model, self$options)
      TRUE
    },

    .subgroupForestPlot = function(image, ...) {
      if (is.null(self$subgroupModel)) {
        return(FALSE)
      }
      renderContSubgroupForest(self$subgroupModel, self$options)
      TRUE
    },

    .bubblePlot = function(image, ...) {
      i <- image$parent$key
      if (is.null(i) || i > length(self$metaRegModels)) {
        return(FALSE)
      }
      metaRegModel <- self$metaRegModels[[i]]
      if (is.null(metaRegModel)) {
        return(FALSE)
      }
      renderBubblePlot(metaRegModel, self$options)
      TRUE
    },

    .leaveOneOutForestPlot = function(image, ...) {
      if (is.null(self$leaveOneOutModel)) {
        return(FALSE)
      }
      renderLeaveOneOutForest(self$leaveOneOutModel, self$options)
      TRUE
    },

    .funnelPlot = function(image, ...) {
      if (is.null(self$model)) {
        return(FALSE)
      }
      renderFunnelPlot(self$model, self$options)
      TRUE
    },

    .asymmetryPlot = function(image, ...) {
      if (is.null(self$model)) {
        return(FALSE)
      }
      renderAsymmetryPlot(self$model, self$options)
      TRUE
    }
  )
)