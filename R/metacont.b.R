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
    # State tracking for lazy models and required core variables
    .model = FALSE,
    .subgroupModel = FALSE,
    .metaRegModels = FALSE,
    .leaveOneOutModel = FALSE,
    .requiredVars = c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),

    # Initialization: runs before the model is computed. Sets up dynamic arrays
    # (meta-regression) and displays placeholder titles.
    .init = function() {
      initMetaRegModels(
        self$results$metaRegModels,
        self$options,
        private$.requiredVars
      )

      initText(
        self$results$text,
        self$options,
        private$.requiredVars,
        "Meta-Analysis Summary"
      )
      initText(
        self$results$subgroupText,
        self$options,
        private$.requiredVars,
        "Subgroup Analysis Summary"
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
    },

    # Post-initialization: runs after .init() but before .run() or render
    # functions.
    .postInit = function() {
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
      updateForestSize(
        image = self$results$subgroupPlot,
        model = self$subgroupModel,
        sizeCache = self$results$subgroupPlotSizeCache,
        renderCall = function() renderContSubgroupForest(self)
      )
      updateForestSize(
        image = self$results$leaveOneOutPlot,
        model = self$leaveOneOutModel,
        sizeCache = self$results$leaveOneOutPlotSizeCache,
        renderCall = function() renderLeaveOneOutForest(self)
      )

      populateMainText(self)
      populateSubgroupText(self)
      populateMetaRegTexts(self)
      populateLeaveOneOutText(self)
      populateAsymmetryTestText(self)
    },

    # Render functions: called by jmvcore when the corresponding plot needs to
    # be rendered or exported. They delegate to shared rendering utilities.
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