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
    }
  ),

  private = list(
    .model = NULL,
    .subgroupModel = NULL,

    .init = function() {
      # Main results
      initMainText(self$results$text, self$model)
      initForestPlot(self$results$plot, self$model, self$options)

      # Subgroup results
      updateSubgroupVisibility(
        self$options,
        self$results,
        self$subgroupModel
      )
      initForestPlot(
        self$results$subgroupPlot,
        self$subgroupModel,
        buildContSubgroupForestOptions(self$options),
        test.effect.subgroup = TRUE,
        subgroup.name = self$options$subgroupVariable,
        print.subgroup.name = self$options$printSubgroupName
      )
    },

    .run = function() {
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
        test.effect.subgroup = TRUE,
        subgroup.name = self$options$subgroupVariable,
        print.subgroup.name = self$options$printSubgroupName
      )
      TRUE
    }
  )
)