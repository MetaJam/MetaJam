# buildB64Map ---------------------------------------------------------------

#' Build a B64-to-original-name mapping
#'
#' Returns a named character vector where names are B64-encoded strings and
#' values are the original human-readable names.
#'
#' @param origNames Character vector of original variable names
#' @return Named character vector: `c(b64name = "Original Name", ...)`
#' @noRd
buildB64Map <- function(origNames) {
  stats::setNames(origNames, jmvcore::toB64(origNames))
}


# decodeB64 -----------------------------------------------------------------

#' Replace B64 tokens in text with original variable names
#'
#' Scans a character vector for B64-encoded tokens and substitutes the
#' original names. Replacements are applied **longest B64 key first** to
#' prevent partial-match corruption (e.g. B64 for "cat" is a prefix of
#' B64 for "cats").
#'
#' @param text Character vector of strings to decode
#' @param b64Map Named character vector from `buildB64Map()`
#' @return Character vector with B64 tokens replaced
#' @noRd
decodeB64 <- function(text, b64Map) {
  if (length(text) == 0) {
    return(text)
  }

  # Sort by key length descending — longest first
  ord <- order(nchar(names(b64Map)), decreasing = TRUE)

  for (i in ord) {
    text <- gsub(names(b64Map)[i], b64Map[i], text, fixed = TRUE)
  }

  text
}


# decodeB64Conditions -------------------------------------------------------

#' Decode B64 tokens in warnings, messages, and errors
#'
#' Warnings and messages are re-signaled after decoding so the existing
#' `runSafe()` collector can keep handling notices in one place. The original
#' B64 condition is muffled. Errors are decoded and rethrown so jamovi displays
#' original variable names.
#'
#' @param expr Expression to evaluate
#' @param b64Map Named character vector from `buildB64Map()`
#' @return The value of `expr`
#' @noRd
decodeB64Conditions <- function(expr, b64Map) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warning(decodeB64(conditionMessage(w), b64Map), call. = FALSE)
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        message(decodeB64(conditionMessage(m), b64Map))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      e$message <- decodeB64(conditionMessage(e), b64Map)
      stop(e)
    }
  )
}
