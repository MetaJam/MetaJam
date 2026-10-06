#' Initialize a Text Skeleton
#'
#' Called from `.init()` to show a titled HTML placeholder before the
#' model is available (e.g., when required variables are not yet assigned).
#' Uses `hasRequiredVars()` instead of checking the model directly to
#' avoid forcing the model active binding.
#'
#' @param textResult Html result element (e.g., `self$results$text`).
#' @param options The `self$options` object from a jamovi analysis.
#' @param requiredVars Character vector of option names that must be
#'   assigned for the model to compute.
#' @param title A string to use as the HTML title.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
initText <- function(textResult, options, requiredVars, title) {
  if (textResult$visible && !hasRequiredVars(options, requiredVars)) {
    textResult$setContent(asHtml(title = title))
  }

  invisible(NULL)
}


#' Populate the Main Summary Text
#'
#' Called from `.run()` after `hasRequiredVars()` has passed.
#' Guards: skips when hidden, already filled (clearWith cache hit),
#' or model is NULL. We use the NULL check of the model here across our
#' module mainly as a proxy that required variables are available, which
#' we already verified in `.run()` before reaching this line. Although
#' redundant, we keep it for clarity.
#'
#' @param self The jamovi `self` object.
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
populateMainText <- function(self) {
  textResult <- self$results$text
  if (!textResult$visible || textResult$isFilled() || is.null(self$model)) {
    return(invisible(NULL))
  }
  textResult$setContent(
    asHtml(summary(self$model), title = "Meta-Analysis Summary")
  )

  invisible(NULL)
}


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


#' Omit Rows Where All Required Variables Are Empty
#'
#' Omits rows where all required variables are missing. Labels, moderators,
#' sorting variables, and optional variables do not determine whether a row is
#' empty, and partially complete rows are retained. Emits an informational
#' message listing the omitted rows by row name, or stops with an error if all
#' rows are empty.
#'
#' This supports keeping several outcomes in one datasheet with a shared
#' study label column. A study may have data for one outcome but none for
#' another. For each analysis, rows with no data in any required variable
#' are omitted from the analysis. The original datasheet is unchanged,
#' so those studies can still be used for other outcomes.
#'
#' Temporarily assign the returned data frame to `private$.data` in `.run()` so
#' all downstream models, subgroups, and sorting use the same filtered rows.
#'
#' We omit rows at the start of `.run()` because it is currently the only
#' lifecycle stage in MetaJam with access to the dataset. Other stages do not
#' have access to the full data: `.init()` and `.postInit()` only receive column
#' metadata, while image rendering and export run with `self$data` set to `NULL`
#' because plots rely on cached models rather than requesting raw data
#' (`requiresData` is not enabled). If another stage accesses raw data in the
#' future, apply the same row omission there.
#'
#' @param self The jamovi analysis object.
#' @param requiredVars Character vector of option names for the variables
#'   required by this analysis, such as `c("eventE", "nE", "eventC", "nC")`.
#' @return The data frame with empty rows omitted.
#' @noRd
omitEmptyRows <- function(self, requiredVars) {
  data <- self$data
  options <- self$options
  emptyRows <- rep(TRUE, nrow(data))

  for (optionName in requiredVars) {
    column <- data[[options[[optionName]]]]
    emptyRows <- emptyRows & is.na(jmvcore::toNumeric(column))
  }

  if (all(emptyRows)) {
    jmvcore::reject("All required variables are empty.")
  }

  if (!any(emptyRows)) {
    return(data)
  }

  omitted <- sum(emptyRows)
  message(
    "Omitted ",
    omitted,
    " row",
    if (omitted == 1) "" else "s",
    " (",
    paste(rownames(data)[emptyRows], collapse = ", "),
    ") because all required variables are empty in ",
    if (omitted == 1) "this row." else "these rows."
  )

  filteredData <- data[!emptyRows, , drop = FALSE]

  # Like jmvcore::naOmit(), restore column attributes after row subsetting.
  # This preserves jamovi's numeric value mappings for downstream conversion.
  for (columnName in names(data)) {
    attributes(filteredData[[columnName]]) <- attributes(data[[columnName]])
  }

  filteredData
}


#' Prepare Model Objects for Fixed-Size Images
#'
#' Fixed-size images do not pass through `updateForestSize()`. Forest images
#' force their model during `.run()` when visible and stale because their
#' dynamic size must be measured before rendering. Fixed-size images have no
#' size-preparation step, so this helper prepares their model during `.run()`
#' before jamovi clears `self$data` and image rendering begins.
#'
#' @param model A model active binding, such as `self$model` or
#'   `self$metaRegModels`. It is only forced when an image needs rendering.
#' @param images An Image result element, or a list of Image result elements,
#'   that depend on the model.
#' @return `NULL` invisibly. Called for the side effect of forcing `model`.
#' @noRd
prepareModelForImages <- function(model, images) {
  if (!is.list(images)) {
    images <- list(images)
  }

  needsModel <- any(vapply(
    images,
    function(image) image$visible && !image$isFilled(),
    logical(1)
  ))

  if (needsModel) {
    force(model)
  }

  invisible(NULL)
}


#' Strip Internal Environments to Prevent Memory Leaks
#'
#' R's `do.call()` injects entire function closures into the model's `$call`
#' when we invoke the functions dynamically. Additionally, the `meta` package
#' internally creates environments and closures for certain components (like
#' GLMM fits in metaprop/metabin/metarate). This utility strips these heavy
#' closures and formula environments out before caching the model to prevent
#' massive serialization size bloat. It applies to base `meta` objects
#' (metacont, metabin, metaprop, metarate, etc.).
#'
#' @param model A `meta` object.
#' @return The cleaned model.
#' @noRd
stripModel <- function(model) {
  # 1. Base models: strip the heavy do.call closures.
  model$call <- NULL

  # metagen stores custom transformations as character names in func.transf /
  # func.backtransf, not as function closures. These fields therefore do not
  # cause the model$call serialization bloat and must not be stripped. meta
  # later executes a stored name with:
  #
  # do.call(func, c(list(x), args))
  #
  # If MetaJam adds custom transformations, define them at package level so
  # later Jamovi RUN, render, and SAVE requests can find them. Keep
  # args.transf and args.backtransf serializable. Some downstream meta helpers
  # do not fully support args.backtransf: metainf() and metacum() omit it from
  # their returned objects, while bubble() ignores it when calling
  # func.backtransf. Review these paths when custom transformations are added.
  # Current MetaJam measures use meta's built-in transformations, and these
  # custom-function fields are NULL.

  # 2. GLMM models: strip internal closures and data environments generated by
  # meta. The data objects are actually environments that capture the R6
  # execution context.
  model$.glmm.common$call <- NULL
  model$.glmm.common$data <- NULL
  if (!is.null(model$.glmm.common$formula)) {
    environment(model$.glmm.common$formula) <- baseenv()
  }

  model$.glmm.random$call <- NULL
  model$.glmm.random$data <- NULL
  if (!is.null(model$.glmm.random$formula)) {
    environment(model$.glmm.random$formula) <- baseenv()
  }

  # 3. Internal metafor::rma model objects (rma.mv for three-level, rma.uni for
  # user-specified weights). Only populated when keeprma = TRUE (default FALSE).
  # MetaJam never reads from them, and nothing in the meta package reads them
  # back either — they are write-only. Safe to NULL unconditionally.
  model$rma.three.level <- NULL
  model$rma.usw.c <- NULL
  model$rma.usw.r <- NULL

  model
}
