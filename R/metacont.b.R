metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,

  # Active bindings for lazy-loaded models. Models are computed only once per
  # request when first accessed, and then cached in private fields to avoid
  # redundant computation. FALSE indicates the computation hasn't been attempted
  # yet. We assign NULL before computing so that if the computation fails, the
  # state remains NULL rather than FALSE. This cleanly prevents the active
  # binding from pointlessly retrying a failed calculation when accessed later
  # in another lifecycle phase (e.g., image rendering).
  active = list(
    model = function() {
      if (isFALSE(private$.model)) {
        private$.model <- NULL
        private$.model <- computeContModel(self)
      }
      private$.model
    },

    subgroupModels = function() {
      if (isFALSE(private$.subgroupModels)) {
        private$.subgroupModels <- NULL
        private$.subgroupModels <- computeContSubgroupModels(self)
      }
      private$.subgroupModels
    },

    metaRegModels = function() {
      if (isFALSE(private$.metaRegModels)) {
        private$.metaRegModels <- NULL
        private$.metaRegModels <- computeMetaRegModels(self)
      }
      private$.metaRegModels
    },

    leaveOneOutModel = function() {
      if (isFALSE(private$.leaveOneOutModel)) {
        private$.leaveOneOutModel <- NULL
        private$.leaveOneOutModel <- computeLeaveOneOutModel(self)
      }
      private$.leaveOneOutModel
    },

    cumulativeModel = function() {
      if (isFALSE(private$.cumulativeModel)) {
        private$.cumulativeModel <- NULL
        private$.cumulativeModel <- computeCumulativeModel(self)
      }
      private$.cumulativeModel
    },

    trimFillModel = function() {
      if (isFALSE(private$.trimFillModel)) {
        private$.trimFillModel <- NULL
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
    .cumulativeModel = FALSE,
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
        "Meta-Analysis Summary"
      )
      initText(
        self$results$leaveOneOutText,
        self$options,
        private$.requiredVars,
        "Leave-One-Out Analysis Summary"
      )
      initText(
        self$results$cumulativeText,
        self$options,
        private$.requiredVars,
        "Cumulative Meta-Analysis Summary"
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
        "Trim-and-Fill Analysis Summary"
      )
      initText(
        self$results$lfkIndexText,
        self$options,
        private$.requiredVars,
        "LFK Index"
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

      applyCachedSize(
        self$results$cumulativePlot,
        self$results$cumulativePlotSizeCache
      )
    },

    # Main execution: Calculate plot dimensions for caching and populate textual
    # results.
    .run = function() {
      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return(invisible(NULL))
      }

      collector <- newCollector()
      runSafe(
        {
          sortKey <- prepareForestSortKey(
            image = self$results$plot,
            model = self$model,
            sortBy = self$options$sortBy,
            sortDirection = self$options$sortDirection,
            sortVariable = self$options$sortVariable,
            data = self$data
          )

          updateForestSize(
            image = self$results$plot,
            model = self$model,
            sizeCache = self$results$plotSizeCache,
            renderCall = function() renderContForest(self, sortKey = sortKey)
          )

          for (i in seq_along(self$options$subgroupVariables)) {
            group <- self$results$subgroupModels$get(key = i)
            subgroupSortKey <- prepareForestSortKey(
              image = group$subgroupPlot,
              model = self$subgroupModels[[i]],
              sortBy = self$options$subgroupSortBy,
              sortDirection = self$options$subgroupSortDirection,
              sortVariable = self$options$subgroupSortVariable,
              data = self$data
            )

            updateForestSize(
              image = group$subgroupPlot,
              model = self$subgroupModels[[i]],
              sizeCache = group$subgroupPlotSizeCache,
              renderCall = function() {
                renderContSubgroupForest(
                  self,
                  key = i,
                  sortKey = subgroupSortKey
                )
              }
            )
          }

          leaveOneOutSortKey <- prepareForestSortKey(
            image = self$results$leaveOneOutPlot,
            model = self$leaveOneOutModel,
            sortBy = self$options$leaveOneOutSortBy,
            sortDirection = self$options$leaveOneOutSortDirection,
            sortVariable = self$options$leaveOneOutSortVariable,
            data = self$data
          )

          updateForestSize(
            image = self$results$leaveOneOutPlot,
            model = self$leaveOneOutModel,
            sizeCache = self$results$leaveOneOutPlotSizeCache,
            renderCall = function() {
              renderLeaveOneOutForest(self, sortKey = leaveOneOutSortKey)
            }
          )

          updateForestSize(
            image = self$results$cumulativePlot,
            model = self$cumulativeModel,
            sizeCache = self$results$cumulativePlotSizeCache,
            renderCall = function() renderCumulativeForest(self)
          )

          prepareModelForImages(
            self$model,
            list(
              self$results$funnelPlot,
              self$results$asymmetryPlot,
              self$results$doiPlot
            )
          )
          prepareModelForImages(
            self$trimFillModel,
            self$results$trimFillFunnelPlot
          )
          bubbleImages <- lapply(
            seq_along(self$options$metaRegBlocks),
            function(i) self$results$metaRegModels$get(key = i)$bubblePlot
          )
          prepareModelForImages(self$metaRegModels, bubbleImages)

          populateMainText(self)
          populateSubgroupTexts(self)
          populateMetaRegTexts(self)
          populateLeaveOneOutText(self)
          populateCumulativeText(self)
          populateAsymmetryTestText(self)
          populateTrimFillText(self)
          populateLfkIndexText(self)
        },
        collector
      )
      displayNotices(self, collector)
    },

    # Render functions: called by jmvcore when the corresponding plot needs to
    # be rendered or exported. They delegate to shared rendering utilities.
    .forestPlot = function(image, ...) {
      renderContForest(self, sortKey = image$state$sortKey)
    },

    .subgroupForestPlot = function(image, ...) {
      renderContSubgroupForest(
        self,
        key = image$parent$key,
        sortKey = image$state$sortKey
      )
    },

    .bubblePlot = function(image, ...) {
      renderBubblePlot(self, key = image$parent$key)
    },

    .leaveOneOutForestPlot = function(image, ...) {
      renderLeaveOneOutForest(self, sortKey = image$state$sortKey)
    },

    .cumulativeForestPlot = function(image, ...) {
      renderCumulativeForest(self)
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
