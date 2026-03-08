#' Calculate Height for Forest Plot
#'
#' This function evaluates a forest plot function from the `meta` package in a
#' null device to calculate the required height for the plot without rendering
#' it.
#'
#' @param expr A quoted expression which calls a forest plot function from the
#'   `meta` package.
#' @return The calculated height in points.
#' @noRd
calcForestHeight <- function(expr) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  # Evaluate the expression in the parent environment where variables are
  # defined
  res <- eval(expr, envir = parent.frame())

  res$figheight$total_height * 72
}

#' Capture and Style Output as HTML
#'
#' Captures the output of an expression (like `summary()`) and wraps it in a
#' styled HTML container. This ensures the output is 100% width and uses
#' a consistent monospaced font.
#'
#' @param ... Expressions to be evaluated. Their output is captured via
#'   `capture.output`.
#' @return A string containing the styled HTML.
#' @noRd
asHtml <- function(...) {
  # Capture the printed output of the expression(s)
  text <- capture.output(...)
  text <- paste0(text, collapse = "\n")

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

  # 1. Scoped Style: Forces this specific result container to 100% width using
  # :has()
  # 2. Structure: DIV (Box) containing PRE (Text)
  htmlContent <- paste0(
    "<style>
      .jmv-results-html:has(.metaport-output) { width: 100% !important; }
    </style>
    <div class='metaport-output' style=\"",
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