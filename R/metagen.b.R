metaGenClass <- R6::R6Class(
  "metaGenClass",
  inherit = metaGenBase,

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
        private$.model <- computeGenModel(self)
      }
      private$.model
    }
  ),

  private = list(
    # State tracking for lazy models and required core variables
    .model = FALSE,
    .requiredVars = c("effectSize", "standardError"),

    # Initialization: runs before the model is computed. Sets up dynamic arrays
    # (subgroup, meta-regression) and displays placeholder titles.
    .init = function() {
      initText(
        self$results$text,
        self$options,
        private$.requiredVars,
        "Meta-Analysis Summary"
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
        populateMainText(self),
        collector
      )
      displayNotices(self, collector)
    }
  )
)
