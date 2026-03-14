#' Capture and Style Output as HTML
#'
#' Captures the output of an expression (like `summary()`) and wraps it in a
#' styled HTML container. This ensures the output is 100% width and uses
#' a consistent monospaced font.
#'
#' @param ... Expressions to be evaluated. Their output is captured via
#'   `capture.output`.
#' @param title Optional string to add a native-looking jamovi `<h2>` title
#'   above the html container.
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

  # Build optional title matching jamovi's Preformatted style
  titleHtml <- ""
  if (!is.null(title)) {
    titleHtml <- paste0(
      "<h2 class='jmv-results-image-title'>",
      title,
      "</h2>"
    )
  }

  # --- CSS Definitions ---

  # Container style (Outer Box)
  divCss <- "
    background-color: #f8f9fa;
    border: 1px solid #e9ecef;
    border-radius: 4px;
    padding: 15px;
  "

  # Text style (Inner Pre)
  # Uses a premium font stack with fallback to system monospace
  preCss <- "
    font-family: 'Fira Code', 'JetBrains Mono', 'Roboto Mono', 
    'Cascadia Code', 'Source Code Pro', ui-monospace, SFMono-Regular, 
    Menlo, Consolas, 'DejaVu Sans Mono', monospace;
    font-size: 13.5px;
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

  # 1. Scoped Style: Forces this specific result container to 100% width using
  # :has()
  # 2. Structure: DIV (Box) containing PRE (Text)
  htmlContent <- paste0(
    "<style>
      .jmv-results-html:has(.metaport-output) { width: max-content !important; }
    </style>",
    titleHtml,
    "<div class='metaport-output' style=\"",
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