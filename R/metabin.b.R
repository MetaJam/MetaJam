metaBinClass <- R6::R6Class(
  "metaBinClass",
  inherit = metaBinBase,

  active = list(
    model = function() {
      if (isFALSE(private$.model)) {
        private$.model <- computeBinModel(self)
      }
      private$.model
    }
  ),

  private = list(
    .model = FALSE,
    .requiredVars = c("eventE", "nE", "eventC", "nC"),

    .init = function() {
      initText(
        self$results$text,
        self$options,
        private$.requiredVars,
        "Overall Summary"
      )
    },

    .run = function() {
      if (!hasRequiredVars(self$options, private$.requiredVars)) {
        return()
      }

      collector <- newCollector()
      runSafe(
        {
          populateMainText(self)
        },
        collector
      )
      displayNotices(self, collector)
    }
  )
)
