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

    metaRegModel = function() {
      if (is.null(private$.metaRegModel)) {
        private$.metaRegModel <- getCachedModel(
          self$results$metaRegText,
          computeMetaRegModel(self$model, self$options)
        )
      }
      private$.metaRegModel
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
    .metaRegModel = NULL,
    .leaveOneOutModel = NULL,
    .requiredVars = c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),

    .init = function() {
      # Visibility only — no model computation, no sizing
      updateSubgroupVisibility(self$options, self$results)
      updateMetaRegVisibility(self$options, self$results)
      updateBubblePlotVisibility(self$options, self$results)
      updateLeaveOneOutVisibility(self$options, self$results)
      updatePubBiasVisibility(self$options, self$results)
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
      initMetaRegText(
        self$results$metaRegText,
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
      populateMetaRegText(
        self$results$metaRegText,
        self$metaRegModel
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
      if (is.null(self$metaRegModel)) {
        return(FALSE)
      }
      renderBubblePlot(self$metaRegModel, self$options)
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