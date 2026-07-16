#' Capture and Style Output as HTML
#'
#' Captures the output of an expression (like `summary()`) and wraps it in a
#' styled HTML container matching jamovi's native table visual style:
#' black caption-style title with border, and a bottom border on the content.
#'
#' @param ... Expressions to be evaluated. Their output is captured via
#'   `capture.output`.
#' @param title Optional string to add a table-caption-style title above the
#'   content. Styled to match jamovi's native table titles.
#' @param modifier Optional callback function. If provided, the captured text
#'   (as a character vector) is passed to this function before being collapsed
#'   into a single string. Useful for stripping redundant headers from R output.
#' @return A string containing the styled HTML.
#' @noRd
asHtml <- function(..., title = NULL, modifier = NULL) {
  # Temporarily increase width to prevent console-like line wrapping for long
  # text
  old_opts <- options(width = 10000)
  on.exit(options(old_opts), add = TRUE)

  # Capture the printed output of the expression(s)
  text <- utils::capture.output(...)

  if (!is.null(modifier)) {
    text <- modifier(text)
  }

  text <- paste0(text, collapse = "\n")

  # Build optional title styled to match jamovi table titles visually.
  # We only take what we actually need from .jmv-results-table-title-cell:
  #   - the 1px bottom border (the line under the title)
  #   - white-space: nowrap (title stays on one line)
  #   - vertical spacing (padding)
  # Font/weight/color match our content <pre> for visual consistency.
  titleHtml <- ""
  if (!is.null(title)) {
    titleHtml <- paste0(
      "<div style=\"",
      # The line under the title
      "border-bottom: 1px solid #333333;",
      # Vertical spacing around title text
      "padding: 4px 0;",
      # Prevent long titles from wrapping to a second line
      "white-space: nowrap;",
      # Use jamovi's native monospace stack:
      # jamovi/jamovi/client/resultsview/main.css:693
      "font-family: Consolas, 'Liberation Mono', Menlo, Courier, monospace;",
      # Material data tables use 13sp table content; use 13px here as the
      # CSS-screen equivalent for dense summary output.
      # https://m1.material.io/components/data-tables.html
      "font-size: 13px;",
      # Material table content uses 87% black; on white this is about #212121.
      # https://m1.material.io/components/data-tables.html
      "color: #212121;",
      "\">",
      title,
      "</div>"
    )
  }

  # --- CSS Definitions ---

  # Container style matches jamovi table borders:
  # border-bottom: 2px is the same as the table last-row border
  # padding: 8px 0 gives 8px top and bottom spacing inside the box
  bodyCss <- "
    background-color: transparent;
    border-bottom: 2px solid #333333;
    padding: 8px 0;
  "

  # Text style (Inner Pre)
  # Use jamovi's native monospace stack:
  # jamovi/jamovi/client/resultsview/main.css:693
  # Size/color are intentionally stronger than jamovi's inherited 12px/#333:
  # Material data tables use 13sp table content and 87% black text; use the
  # CSS-screen equivalents 13px and #212121 here.
  # https://m1.material.io/components/data-tables.html
  # white-space: pre keeps the printed spacing/alignment from summary().
  preCss <- "
    font-family: Consolas, 'Liberation Mono', Menlo, Courier, monospace;
    font-size: 13px;
    color: #212121;
    line-height: 1.5;
    white-space: pre;
    margin: 0;
    overflow-x: auto;
  "

  # --- HTML Construction ---

  if (text == "") {
    return(titleHtml)
  }

  # 1. Scoped Style: Forces this result container to max-content width
  #    using :has() (default is 500px, too narrow for wide summary output)
  # 2. Structure: Title div (with bottom line) + Content div (with bottom
  #    border) containing PRE (monospace text)
  htmlContent <- paste0(
    "<style>
      .jmv-results-html:has(.metajam-output) { width: max-content !important; }
    </style>",
    titleHtml,
    "<div class='metajam-output' style=\"",
    bodyCss,
    "\">",
    "<pre style=\"",
    preCss,
    "\">",
    text,
    "</pre>",
    "</div>"
  )

  htmlContent
}
