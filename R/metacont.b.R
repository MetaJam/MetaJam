metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,

  active = list(
    model = function() {
      if (is.null(private$.model)) {
        private$.model <- resolveModel(
          self,
          c("meanE", "sdE", "nE", "meanC", "sdC", "nC"),
          computeContModel
        )
      }
      private$.model
    },

    subgroupModel = function() {
      if (is.null(private$.subgroupModel)) {
        private$.subgroupModel <- resolveSubgroupModel(self)
      }
      private$.subgroupModel
    },

    leaveOneOutModel = function() {
      if (is.null(private$.leaveOneOutModel)) {
        private$.leaveOneOutModel <- computeLeaveOneOutModel(
          self$model, self$options
        )
      }
      private$.leaveOneOutModel
    }
  ),

  private = list(
    .model = NULL,
    .subgroupModel = NULL,
    .leaveOneOutModel = NULL,

    .init = function() {
      # Main results
      initForestPlot(self$results$plot, self$model, self$options)

      # Subgroup results
      updateSubgroupVisibility(self$options, self$results)
      initForestPlot(
        self$results$subgroupPlot,
        self$subgroupModel,
        buildContSubgroupForestOptions(self$options),
        test.effect.subgroup = self$options$subgroupForestTestEffect,
        test.subgroup = self$options$subgroupForestTestSubgroup,
        subgroup.name = self$options$subgroupVariable,
        print.subgroup.name = self$options$printSubgroupName
      )

      # Leave-one-out results
      updateLeaveOneOutVisibility(self$options, self$results)
      initForestPlot(
        self$results$leaveOneOutPlot,
        self$leaveOneOutModel,
        self$options,
        renderFn = renderLeaveOneOutForest
      )
    },

    .run = function() {
      # Initialize text skeletons (before hasRequiredVars so placeholders
      # show even when variables aren't assigned yet). isFilled() guards
      # inside these functions skip when clearWith didn't invalidate.
      initMainText(
        self$results$text,
        self$options,
        c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
      )
      initSubgroupText(
        self$results$subgroupText,
        self$options,
        c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
      )
      initLeaveOneOutText(
        self$results$leaveOneOutText,
        self$options,
        c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
      )

      if (
        !hasRequiredVars(
          self$options,
          c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
        )
      ) {
        return()
      }

      populateMainText(self$results$text, self$model)
      populateSubgroupText(
        self$results$subgroupText,
        self$subgroupModel
      )
      populateLeaveOneOutText(
        self$results$leaveOneOutText,
        self$leaveOneOutModel
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
      renderContForest(
        self$subgroupModel,
        buildContSubgroupForestOptions(self$options),
        test.effect.subgroup = self$options$subgroupForestTestEffect,
        test.subgroup = self$options$subgroupForestTestSubgroup,
        subgroup.name = self$options$subgroupVariable,
        print.subgroup.name = self$options$printSubgroupName
      )
      TRUE
    },

    .leaveOneOutForestPlot = function(image, ...) {
      if (is.null(self$leaveOneOutModel)) {
        return(FALSE)
      }
      renderLeaveOneOutForest(self$leaveOneOutModel, self$options)
      TRUE
    }
  )
)