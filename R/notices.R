#' Create a new collector environment
#'
#' @return An environment with `warnings` and `messages` character vectors
#' @noRd
newCollector <- function() {
  collector <- new.env(parent = emptyenv())
  collector$warnings <- character()
  collector$messages <- character()
  collector
}

#' Execute an expression, capturing warnings and messages
#'
#' Warnings are collected for display as WARNING notices. Messages are collected
#' for display as INFO notices.
#'
#' Known noise patterns (e.g., Rtools path warnings) are filtered out.
#'
#' @param expr The expression to evaluate
#' @param collector An environment created by newCollector()
#' @return The evaluated result of `expr`. The `collector` environment is
#'   modified in-place with intercepted strings.
#' @noRd
runSafe <- function(expr, collector) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- w$message
      if (!grepl("C:/Rtools/home/builder", msg, fixed = TRUE)) {
        collector$warnings <- c(collector$warnings, trimws(msg))
      }
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- m$message
      collector$messages <- c(collector$messages, trimws(msg))
      invokeRestart("muffleMessage")
    }
  )
}

#' Display Notices
#'
#' Converts collected warnings and messages into jmvcore::Notice objects and
#' inserts them into results.
#'
#' @param self The jamovi `self` object.
#' @param collector Environment with `$warnings` and `$messages` vectors
#' @return `NULL` invisibly. Called for side effects.
#' @noRd
displayNotices <- function(self, collector) {
  options <- self$options
  results <- self$results

  # Messages (INFO)
  if (length(collector$messages) > 0) {
    msgContent <- paste(collector$messages, collapse = "\n\n")

    msgNotice <- jmvcore::Notice$new(
      options = options,
      name = "runMsg",
      type = jmvcore::NoticeType$INFO
    )
    msgNotice$setContent(msgContent)
    results$insert(1, msgNotice)
  }

  # Warnings (WARNING)
  if (length(collector$warnings) > 0) {
    warnContent <- paste(collector$warnings, collapse = "\n\n")

    warnNotice <- jmvcore::Notice$new(
      options = options,
      name = "runWarn",
      type = jmvcore::NoticeType$WARNING
    )
    warnNotice$setContent(warnContent)
    results$insert(1, warnNotice)
  }

  invisible(NULL)
}
