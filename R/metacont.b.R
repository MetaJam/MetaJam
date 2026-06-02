metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,

  # Active bindings for lazy-loaded models. Models are computed only once per
  # request when first accessed, and then cached in private fields to avoid
  # redundant computation.
  active = list(
    model = function() {
      if (isFALSE(private$.model)) {
        private$.model <- computeContModel(self)
      }
      private$.model
    },

    subgroupModels = function() {
      if (isFALSE(private$.subgroupModels)) {
        private$.subgroupModels <- computeContSubgroupModels(self)
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
    # State tracking for lazy models and required core variables
    .model = FALSE,
    .subgroupModels = FALSE,
    .metaRegModels = FALSE,
    .leaveOneOutModel = FALSE,
    .trimFillModel = FALSE,
    .requiredVars = c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),

    # Initialization: runs before the model is computed. Sets up dynamic arrays
    # (subgroup, meta-regression) and displays placeholder titles.
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
    },

    # Post-initialization: runs after .init() but before .run() or render
    # functions.
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

    # Main execution: Calculate plot dimensions for caching and populate textual
    # results.
    .run = function() {
      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return()
      }

      updateForestSize(
        image = self$results$plot,
        model = self$model,
        sizeCache = self$results$plotSizeCache,
        renderCall = function() renderContForest(self)
      )
      for (i in seq_along(self$options$subgroupVariables)) {
        group <- self$results$subgroupModels$get(key = i)
        updateForestSize(
          image = group$subgroupPlot,
          model = self$subgroupModels[[i]],
          sizeCache = group$subgroupPlotSizeCache,
          renderCall = function() renderContSubgroupForest(self, key = i)
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
    },

    # Render functions: called by jmvcore when the corresponding plot needs to
    # be rendered or exported. They delegate to shared rendering utilities.
    .forestPlot = function(image, ...) {
      renderContForest(self)
    },

    .subgroupForestPlot = function(image, ...) {
      renderContSubgroupForest(self, key = image$parent$key)
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
    }
  )
)