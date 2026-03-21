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


#' Initialize the Main Text Skeleton
#'
#' Called from `.run()` to show a titled HTML placeholder before the
#' model is available (e.g., when required variables are not yet assigned).
#' Uses `hasRequiredVars()` instead of checking the model directly to
#' avoid forcing the model active binding. The `isFilled()` guard
#' preserves the clearWith optimization — when a non-model option
#' changed, the previous content is still valid and this function
#' exits immediately.
#'
#' @param textResult Html result element (e.g., `self$results$text`).
#' @param options The `self$options` object from a jamovi analysis.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @noRd
initMainText <- function(textResult, options, requiredVars) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  if (!hasRequiredVars(options, requiredVars)) {
    textResult$setContent(asHtml(title = "Meta-Analysis Summary"))
  }
}


#' Populate the Main Summary Text
#'
#' Called from `.run()` when the model is available. The `isFilled()`
#' guard skips recomputation when a non-model option changed and the
#' previous content is still valid (restored from protobuf via clearWith).
#'
#' @param textResult Html result element.
#' @param model A `meta` object (must not be `NULL`).
#' @noRd
populateMainText <- function(textResult, model) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  textResult$setContent(asHtml(summary(model), title = "Meta-Analysis Summary"))
}