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

  # Mirror jmvcore::htmlEscape(attribute = FALSE) for minApp compatibility.
  # Escape & first so the entities introduced for < and > are not re-escaped.
  # Source: jmvcore/R/utils.R:1360-1364.
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)

  # Build optional title styled to match jamovi table titles visually.
  # We only take what we actually need from .jmv-results-table-title-cell:
  #   - the 1px bottom border (the line under the title)
  #   - white-space: nowrap (title stays on one line)
  #   - vertical spacing (padding)
  # Font/weight/color match our content <pre> for visual consistency.
  titleHtml <- ""
  if (!is.null(title)) {
    titleHtml <- paste0(
      "<div class='metajam-title' style=\"",
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

  # Summary body styles
  # Use jamovi's native monospace stack:
  # jamovi/jamovi/client/resultsview/main.css:693
  # Size/color are intentionally stronger than jamovi's inherited 12px/#333:
  # Material data tables use 13sp table content and 87% black text; use the
  # CSS-screen equivalents 13px and #212121 here.
  # https://m1.material.io/components/data-tables.html
  # white-space: pre keeps the printed spacing/alignment from summary().
  # Summary body border and spacing:
  # border-bottom: 2px is the same as the table last-row border
  # padding: 8px 0 gives 8px top and bottom spacing inside the box
  bodyCss <- "
    background-color: transparent;
    border-bottom: 2px solid #333333;
    padding: 8px 0;
    font-family: Consolas, 'Liberation Mono', Menlo, Courier, monospace;
    font-size: 13px;
    color: #212121;
    line-height: 1.5;
    white-space: pre;
    margin: 0;
  "

  # --- HTML Construction ---

  # Include the body only when text exists; otherwise show the title alone.
  bodyHtml <- ""
  if (text != "") {
    bodyHtml <- paste0(
      "<pre style=\"",
      bodyCss,
      "\">",
      text,
      "</pre>"
    )
  }

  # jamovi gives Html results a default width of 500px.
  # jamovi 28.8.0 (commit 5ec9db87, issue #1850) adds min-width: min-content,
  # allowing wider output to expand beyond 500px, while shorter output
  # still retains the default 500px width. Relying on this change would
  # require minApp: 28.8.0.
  # We keep our scoped max-content override so table-like output fits its
  # content, including shorter output without unused space, while
  # preserving our minApp of 2.7.27.
  #
  # Our width override makes the result fit its content in jamovi, but
  # this sizing does not carry over to HTML/PDF exports because they omit
  # jamovi's outer container. To preserve the same sizing in exports,
  # we wrap the title and summary together in our own max-content container.
  #
  # jamovi's HTML exporter (client/main/formatio/htmlify.ts) uses
  # break-inside: avoid for both tables and <pre>. Keep jamovi's default
  # page-break behavior so summaries stay together when they fit on a page,
  # while longer summaries can still continue across pages when necessary.
  # We add break-after: avoid on the title to keep it with the beginning
  # of the summary.
  # Include the legacy page-break-after property for compatibility.
  htmlContent <- paste0(
    "<style>
      .jmv-results-html:has(.metajam-output) { width: max-content; }
      @media print {
        .metajam-output > .metajam-title {
          break-after: avoid;
          page-break-after: avoid;
        }
      }
    </style>",
    "<div class='metajam-output' style='width: max-content;'>",
    titleHtml,
    bodyHtml,
    "</div>"
  )

  htmlContent
}
