metaBinClass <- R6::R6Class(
  "metaBinClass",
  inherit = metaBinBase,

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
        private$.model <- computeBinModel(self)
      }
      private$.model
    },

    subgroupModels = function() {
      if (isFALSE(private$.subgroupModels)) {
        private$.subgroupModels <- NULL
        private$.subgroupModels <- computeBinSubgroupModels(self)
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

    trimFillModel = function() {
      if (isFALSE(private$.trimFillModel)) {
        private$.trimFillModel <- NULL
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
            renderCall = function() renderBinForest(self, sortKey = sortKey)
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
                renderBinSubgroupForest(
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

          prepareModelForImages(
            self$model,
            list(
              self$results$funnelPlotImage,
              self$results$asymmetryPlotImage,
              self$results$doiPlotImage
            )
          )
          prepareModelForImages(
            self$trimFillModel,
            self$results$trimFillFunnelPlotImage
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
          populateAsymmetryTestText(self)
          populateTrimFillText(self)
          populateLfkIndexText(self)
        },
        collector
      )
      displayNotices(self, collector)
    },

    .forestPlot = function(image, ...) {
      renderBinForest(self, sortKey = image$state$sortKey)
    },

    .subgroupForestPlot = function(image, ...) {
      renderBinSubgroupForest(
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
