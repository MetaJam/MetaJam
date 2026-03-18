#' Check Whether Required Variables Are Assigned
#'
#' Lightweight check that avoids triggering model computation.
#' Used as a guard in `.run()` instead of `is.null(self$model)` so
#' the model active binding is not forced unnecessarily.
#'
#' @param options A Jamovi options object.
#' @param vars Character vector of variable option names that must be assigned.
#' @return `TRUE` if all required variables are non-NULL, `FALSE` otherwise.
#' @noRd
hasRequiredVars <- function(options, vars) {
  for (opt in vars) {
    if (is.null(options[[opt]])) return(FALSE)
  }
  TRUE
}


#' Resolve the Primary Model (If All Required Options Are Set)
#'
#' Encapsulates the shared active-binding workflow used by every analysis
#' type: guard that required options are assigned, ensure data is loaded,
#' then delegate to a type-specific compute function.
#'
#' @param analysis The jamovi analysis object (`self`).
#' @param required Character vector of option names that must be assigned.
#' @param computeFn A function `f(data, options)` that returns a `meta` object.
#' @return A `meta` object, or `NULL` if required options are missing.
#' @noRd
resolveModel <- function(analysis, required, computeFn) {
  if (!hasRequiredVars(analysis$options, required)) return()
  data <- analysis$data
  if (is.null(data) || nrow(data) == 0) {
    data <- analysis$readDataset()
  }
  computeFn(data, analysis$options)
}


#' Initialize the Main Text Skeleton
#'
#' Called from `.init()` to show a blank titled HTML immediately.
#' Visibility is handled by the YAML binding `visible: (showSummary)`,
#' so this function only builds the skeleton content.
#' The `!visible` guard preserves lazy evaluation — when showSummary
#' is off, `model` (a promise to `self$model`) is never forced.
#'
#' @param textResult Html result element (e.g., `self$results$text`).
#' @param model A `meta` object, or `NULL` if not yet computed.
#' @noRd
initMainText <- function(textResult, model) {
  if (!textResult$visible) {
    return()
  }
  if (is.null(model)) {
    textResult$setContent(asHtml(title = "Meta-Analysis Summary"))
  }
}


#' Populate the Main Summary Text
#'
#' Called from `.run()` when the model is available.
#'
#' @param textResult Html result element.
#' @param model A `meta` object (must not be `NULL`).
#' @noRd
populateMainText <- function(textResult, model) {
  if (!textResult$visible) {
    return()
  }
  textResult$setContent(asHtml(summary(model), title = "Meta-Analysis Summary"))
}