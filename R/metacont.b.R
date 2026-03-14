metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,
  active = list(
    model = function() {
      if (is.null(private$.model)) {
        # Guard: all six required columns must be assigned
        required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
        for (opt in required) {
          if (is.null(self$options[[opt]])) return()
        }

        data <- self$data
        if (is.null(data) || nrow(data) == 0) {
          data <- self$readDataset()
        }
        private$.model <- computeContModel(data, self$options)
      }
      private$.model
    }
  ),
  private = list(
    .model = NULL,

    .init = function() {
      initForestPlot(self$results$plot, self$model, self$options)
      if (self$options$showSummary) {
        self$results$text$setContent(asHtml(title = "Meta-Analysis Summary"))
      }
    },

    .run = function() {
      if (is.null(self$model)) {
        return(NULL)
      }

      # Overall results
      if (self$options$showSummary) {
        self$results$text$setContent(asHtml(
          summary(self$model),
          title = "Meta-Analysis Summary"
        ))
      }
    },

    .forestPlot = function(image, ...) {
      if (is.null(self$model)) {
        return(FALSE)
      }

      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

      model <- self$model
      eval(buildForestExpr(model, self$options))

      TRUE
    }
  )
)