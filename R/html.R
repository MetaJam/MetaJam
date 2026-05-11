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
#' @return A string containing the styled HTML.
#' @noRd
asHtml <- function(..., title = NULL) {
  # Temporarily increase width to prevent console-like line wrapping for long
  # text
  old_opts <- options(width = 10000)
  on.exit(options(old_opts), add = TRUE)

  # Capture the printed output of the expression(s)
  text <- capture.output(...)
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
      # Match content font/weight/color exactly
      "font-family: 'Fira Code','JetBrains Mono','Roboto Mono',",
      "'Cascadia Code','Source Code Pro',ui-monospace,SFMono-Regular,",
      "Menlo,Consolas,'DejaVu Sans Mono',monospace;",
      "font-size: 12px;",
      "font-weight: 500;",
      "color: #333333;",
      "\">",
      title,
      "</div>"
    )
  }

  # --- CSS Definitions ---

  # Container style matches jamovi table borders:
  # border-bottom: 2px is the same as the table last-row border
  # padding: 8px 0 gives 8px top and bottom spacing inside the box
  divCss <- "
    background-color: transparent;
    border-bottom: 2px solid #333333;
    padding: 8px 0;
  "

  # Text style (Inner Pre)
  # Uses a premium font stack with fallback to system monospace
  preCss <- "
    font-family: 'Fira Code', 'JetBrains Mono', 'Roboto Mono',
    'Cascadia Code', 'Source Code Pro', ui-monospace, SFMono-Regular,
    Menlo, Consolas, 'DejaVu Sans Mono', monospace;
    font-size: 12px;
    font-weight: 500;
    color: #333333;
    line-height: 1.5;
    white-space: pre;
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
    divCss,
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