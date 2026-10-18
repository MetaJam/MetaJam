forestPlotClass <- R6::R6Class(
  "forestPlotClass",
  inherit = forestPlotBase,

  private = list(
    # This analysis always displays its plot, so no visibility checks are
    # needed.
    .postInit = function() {
      image <- self$results$plot
      size <- self$results$plotSizeCache$state

      # Each request creates a new image at the YAML/default size; jamovi
      # restores result state but not the dimensions. Reapply the last
      # calculated size on every request. If it is still correct, it remains in
      # use. If clearWith cleared the plot, the old size is kept until .run()
      # calculates, applies, and caches the current size. This avoids changing
      # first from the old size to the YAML/default size and then changing again
      # to the current size; the plot changes only once, directly from the old
      # size to the current size.
      if (!is.null(size)) {
        image$setSize(size$width, size$height)
      }
    },

    .run = function() {
      # Check required inputs and cached state ---------------------------------

      if (
        !hasRequiredVars(self$options, c("effectSize", "ciLower", "ciUpper"))
      ) {
        return(invisible(NULL))
      }

      image <- self$results$plot
      # Use state as a proxy for clearWith to decide whether plot preparation
      # must be recalculated. This analysis always stores non-NULL state after
      # preparing the image. clearWith removes it when plot inputs change;
      # therefore NULL state means preparation must run again. isFilled()
      # becomes FALSE for those same changes, but also when the user resizes the
      # image or changes the global theme/palette. State remains non-NULL in
      # those two rerender-only cases, so using it instead of isFilled() avoids
      # redundant data preparation and size calculation.
      if (!is.null(image$state)) {
        return(invisible(NULL))
      }

      # Prepare inputs, row types, and weights ---------------------------------

      data <- self$data
      leftVariables <- c(self$options$rowLabel, self$options$leftColumns)
      rightVariables <- self$options$rightColumns
      est <- jmvcore::toNumeric(data[[self$options$effectSize]])
      lower <- jmvcore::toNumeric(data[[self$options$ciLower]])
      upper <- jmvcore::toNumeric(data[[self$options$ciUpper]])

      # Normalize and match row types, ignoring blank or missing values.
      rowTypes <- rep("", nrow(data))
      if (!is.null(self$options$rowType)) {
        values <- trimws(tolower(data[[self$options$rowType]]))
        hasType <- !is.na(values) & nzchar(values)
        choices <- c("heading", "item", "summary")
        matches <- pmatch(values[hasType], choices, duplicates.ok = TRUE)

        if (anyNA(matches)) {
          jmvcore::reject(
            '"Row Type" values must be "heading", "item", "summary", or blank.'
          )
        }

        rowTypes[hasType] <- choices[matches]
      }

      isSummary <- rowTypes == "summary"

      # Prepare optional numeric weights for marker scaling. NULL means no
      # Weight input was selected, so the plot uses equal-size markers.
      weight <- NULL
      if (!is.null(self$options$weight)) {
        weight <- jmvcore::toNumeric(data[[self$options$weight]])

        # Summary weights are excluded from scaling and receive the maximum
        # size. Supply a positive drawing placeholder for missing summary
        # weights so users can leave those cells blank without losing the
        # diamond. Any positive value works here: forestploter excludes it
        # before normalizing study weights and then assigns the summary the
        # maximum marker size.
        weight[isSummary & is.na(weight)] <- 1
      }

      # Format left and right columns ------------------------------------------

      # Format numeric columns with shared decimal places chosen automatically
      # by .formatNumbers().
      # Keep other columns as their character labels and show missing values
      # as blanks.
      displayColumns <- lapply(
        c(leftVariables, rightVariables),
        function(variable) {
          column <- data[[variable]]
          if (is.numeric(column)) {
            private$.formatNumbers(column)
          } else {
            ifelse(is.na(column), "", as.character(column))
          }
        }
      )
      alignment <- vapply(
        c(leftVariables, rightVariables),
        function(variable) {
          private$.inferAlignment(data[[variable]])
        },
        numeric(1)
      )

      # Preserve row-label text and left alignment, regardless of storage type.
      if (!is.null(self$options$rowLabel)) {
        column <- data[[self$options$rowLabel]]
        displayColumns[[1L]] <- ifelse(is.na(column), "", as.character(column))
        alignment[1L] <- 0
      }

      # Format effect size and CI columns --------------------------------------

      # Format the values to the selected decimal places.
      # Show missing values as an em dash.
      # When both CI limits are missing, show only the effect size in a combined
      # column, or leave the separate CI column blank.
      # Leave the cells blank when all three values are missing.
      effectCiColumns <- list()
      effectCiHeaders <- character()
      effectCiAlignment <- numeric()

      if (self$options$effectCi) {
        digits <- as.integer(self$options$effectCiDigits)
        effectText <- private$.formatNumbers(est, digits, "\u2014")
        lowerText <- private$.formatNumbers(lower, digits, "\u2014")
        upperText <- private$.formatNumbers(upper, digits, "\u2014")
        ciMissing <- is.na(lower) & is.na(upper)
        allMissing <- is.na(est) & ciMissing
        separator <- switch(
          self$options$ciSeparator,
          comma = ", ",
          semicolon = "; ",
          enDash = "\u2013",
          to = " to "
        )
        ciText <- paste0(lowerText, separator, upperText)

        # Leave the CI blank when both limits are missing.
        # Leave the effect size blank when all three values are missing.
        ciText[ciMissing] <- ""
        effectText[allMissing] <- ""

        if (self$options$effectCiLayout == "combined") {
          brackets <- switch(
            self$options$ciBrackets,
            round = c("(", ")"),
            square = c("[", "]")
          )
          # Show only the effect size when both CI limits are missing.
          # The same ciMissing check also covers rows where all three values
          # are missing. For those rows, effectText was set to blank above,
          # so the cell stays blank.
          effectCiColumns <- list(ifelse(
            ciMissing,
            effectText,
            paste0(effectText, " ", brackets[1L], ciText, brackets[2L])
          ))
          effectCiHeaders <- paste0(
            self$options$effectHeader,
            " ",
            brackets[1L],
            self$options$ciHeader,
            brackets[2L]
          )
          # Center the effect size and CI together as one compound value.
          effectCiAlignment <- 0.5
        } else {
          # Keep the CI unbracketed when it has its own column.
          effectCiColumns <- list(effectText, ciText)
          effectCiHeaders <- c(self$options$effectHeader, self$options$ciHeader)
          # Right-align the effect size and center the CI range.
          effectCiAlignment <- c(1, 0.5)
        }
      }

      # Assemble the display table ---------------------------------------------

      leftIndices <- seq_along(leftVariables)
      rightIndices <- length(leftVariables) + seq_along(rightVariables)

      leftColumns <- displayColumns[leftIndices]
      rightColumns <- displayColumns[rightIndices]
      leftAlignment <- alignment[leftIndices]
      rightAlignment <- alignment[rightIndices]
      leftHeaders <- leftVariables
      rightHeaders <- rightVariables
      if (self$options$effectCiPosition == "left") {
        leftColumns <- c(leftColumns, effectCiColumns)
        leftHeaders <- c(leftHeaders, effectCiHeaders)
        leftAlignment <- c(leftAlignment, effectCiAlignment)
      } else {
        rightColumns <- c(effectCiColumns, rightColumns)
        rightHeaders <- c(effectCiHeaders, rightHeaders)
        rightAlignment <- c(effectCiAlignment, rightAlignment)
      }

      # Use spaces to size the CI column, following forestploter's examples.
      # The target is inspired by meta's standard 6 cm plotting area.
      # With default settings on a Windows ragg device, 48 spaces measured about
      # 5.64 cm; table padding added 0.40 cm, giving a column about 6.04 cm
      # wide. The physical width varies with the font and device.
      plotColumns <- c(
        leftColumns,
        list(rep(strrep(" ", 48), nrow(data))),
        rightColumns
      )
      plotData <- as.data.frame(plotColumns)
      names(plotData) <- c(leftHeaders, " ", rightHeaders)
      # Center the plotting column so any future heading sits over the middle
      # of the forest plot area. Body cells contain only spaces; this text
      # alignment does not affect the positions of CI lines or markers.
      alignment <- c(leftAlignment, 0.5, rightAlignment)

      # Apply row styling ------------------------------------------------------

      # Prepare row styling before forestploter constructs and measures the
      # table, so bold labels and indentation contribute to its dimensions.
      #
      # Font faces follow grid::gpar(): 1 = plain, 2 = bold.
      #
      # Create a fontface matrix with the same number of rows and columns as
      # plotData. Each matrix entry controls the text in the corresponding
      # table cell. Start with plain text in every cell.
      #
      # For summary rows, set every cell in the row to bold.
      # For heading rows, set only the row-label cell to bold. When selected,
      # the row-label column is always the first column of plotData.
      # Item rows keep plain text; their labels are indented separately below.
      #
      # gridExtra needs these font faces as a vector in core$fg_params, with
      # one value for each body cell. The values must follow its cell order:
      # down the first column, then down the second column, and so on.
      #
      # gridExtra pairs each formatting value with the corresponding table
      # label, reading the labels column by column. It applies that formatting
      # when creating the cell's text grob, then arranges the grobs into a
      # matrix column by column (byrow = FALSE), matching the table layout.
      #
      # We work backwards from that layout: start with a fontface matrix
      # matching the displayed table, where row and cell styling is easy to
      # assign, then flatten it column by column with as.vector(fontface).
      # This produces the vector gridExtra needs to reconstruct the intended
      # cell formatting. Store this vector in state for measurement and drawing.
      # Source: gridExtra R/tableGrob.r, gtable_table().
      fontface <- matrix(1L, nrow = nrow(plotData), ncol = ncol(plotData))
      fontface[isSummary, ] <- 2L
      if (!is.null(self$options$rowLabel)) {
        fontface[rowTypes == "heading", 1L] <- 2L
        itemRows <- rowTypes == "item"
        plotData[[1L]][itemRows] <- paste0(
          strrep(" ", 4L),
          plotData[[1L]][itemRows]
        )
      }

      # Prepare plot state and dimensions --------------------------------------

      # Store the display table and numeric inputs in image state below so
      # rendering and export can construct the plot without reloading the raw
      # dataset.
      state <- list(
        data = plotData,
        est = est,
        lower = lower,
        upper = upper,
        weight = weight,
        # Locate the plotting column after inserting generated columns.
        ci_column = length(leftColumns) + 1L,
        is_summary = isSummary,
        fontface = as.vector(fontface),
        alignment = alignment
      )

      # Measure the plot dimensions.
      oldDev <- grDevices::dev.cur()

      # Open a non-rendering ragg device to query systemfonts/textshaping
      # metrics without allocating an in-memory pixel buffer or creating
      # temporary files.
      ragg::agg_record()
      on.exit({
        grDevices::dev.off()
        if (oldDev > 1) grDevices::dev.set(oldDev)
      })

      # Print devices normally initialize the graphics-engine display list
      # with recording OFF. ragg::agg_record() starts with recording ON so
      # graphics can be captured later with recordPlot(). Here we only
      # construct and measure the plot; we do not use recordPlot().
      # Turn recording OFF so the graphics-engine display list does not
      # keep an unused record of those operations in memory.
      grDevices::dev.control(displaylist = "inhibit")

      # Construct and measure on the same device: forestploter resolves some
      # layout units during construction. Convert inches to jamovi's image units
      # (72 per inch).
      plot <- private$.buildPlot(state)
      dimensions <- forestploter::get_wh(plot, unit = "in")
      size <- list(
        width = dimensions[["width"]] * 72,
        height = dimensions[["height"]] * 72
      )

      # Apply and cache the dimensions, and save the prepared plot inputs.
      image$setSize(size$width, size$height)
      self$results$plotSizeCache$setState(size)
      image$setState(state)
    },

    .forestPlot = function(image, ...) {
      if (is.null(image$state)) {
        return(FALSE)
      }

      plot <- private$.buildPlot(image$state)
      # Grid starts its first page automatically when drawing or measuring
      # needs it. Calling .buildPlot() already starts that first page, because
      # forestploter's measurements (such as convertHeight and convertWidth)
      # query the active device. Even without those calls, grid.draw() would
      # start the first page itself, so no grid.newpage() is needed here.
      # While grid.newpage() is useful to clear an earlier plot or start
      # another page when reusing a device, we do neither here.
      # Therefore, we use grid.draw() directly. forestploter's plot() and
      # print() methods simply wrap grid.draw() with an extra grid.newpage(),
      # forcing a second page. That pushes the plot behind a blank first page
      # in PDFs and causes PowerPoint export to fail because its device
      # supports only one page.
      grid::grid.draw(plot)
      TRUE
    },

    .buildPlot = function(state) {
      # Prepare text alignment and padding -------------------------------------

      alignment <- state$alignment

      # hjust chooses which part of the text sits on the anchor:
      # 0 = left edge, 0.5 = midpoint, 1 = right edge.
      # x chooses where that anchor sits inside the cell.
      #
      # tableGrob() sizes each column from its text width plus horizontal
      # padding; it does not create a separate inner area for the text.
      # forestploter positions left-aligned text at 0.05 npc, or 5% of the cell
      # width. That inset grows with the column and can exceed the fixed
      # padding. Use half the horizontal padding at either edge instead.
      # Sources: gridExtra R/tableGrob.r, gtable_table(); grid::textGrob().
      #
      # Keep forestploter's default padding: 4 mm horizontally, with 3 mm
      # vertically for body cells and 4 mm for headers. Set it explicitly
      # because the text positions below use half the horizontal padding.
      # This keeps the padding and text positions consistent if the package
      # defaults change.
      # Source: forestploter R/theme.R, core_internal and colhead_internal.
      horizontalPadding <- 4

      # Start at 0, 0.5, or 1 of the cell width. Then add the half-padding on
      # the left, leave the center unchanged, or subtract it on the right.
      # The multiplier 1 - 2 * alignment gives exactly these offsets: +1, 0, -1.
      # With 4 mm padding, x is 2 mm, 0.5 npc, or 1 npc minus 2 mm.
      # Vertical text stays centered at y = 0.5, vjust = 0.5, which shares the
      # vertical padding above and below the text without an edge inset.
      x <- grid::unit(alignment, "npc") +
        (1 - 2 * alignment) * grid::unit(horizontalPadding / 2, "mm")

      # Prepare the plot style -------------------------------------------------

      # forest_style() prepares settings; it does not construct a plot.
      # Pass the settings directly to forest() so the table is constructed
      # and measured with its final text formatting. This avoids a later
      # set_style() rebuild or edit_plot() edits to existing grobs.
      # edit_plot() changes the grobs without recalculating column widths.
      #
      # forest_style() accepts gridExtra table settings through ...:
      # core controls body cells; colhead controls column headings.
      # Each part can specify fg_params (text), bg_params (background),
      # padding, and the drawing functions fg_fun and bg_fun.
      # forestploter merges these settings into the theme used by tableGrob().
      #
      # With the default drawing functions, fg_params are passed to
      # gridExtra:::text_grob() and bg_params to gridExtra:::rect_grob().
      # Inspect formals() on those helpers for accepted parameter names.
      #
      # Headers follow their column's alignment. For body cells, rep() with
      # each = nrow(state$data) repeats each value down its column before
      # moving to the next. This follows gridExtra's column-wise cell order,
      # as with the fontface matrix above.
      # Source: gridExtra R/tableGrob.r, gtable_table().
      #
      # gridExtra collects fg_params in a data frame before drawing the cells.
      # Simple mm or npc units have numeric storage. Our npc + mm positions
      # have list storage so they can retain both parts of the calculation.
      #
      # Neither form has its own as.data.frame() method. R's default method
      # accepts the numeric storage of simple units but rejects our
      # list-based x.
      # I() adds the AsIs class, whose method stores x as one list column.
      # Each entry contains the x position for one table cell, including its
      # npc and mm components, ready to pass to the cell's text grob.
      # Sources: gridExtra gtable_table(); base as.data.frame.default()
      # and as.data.frame.AsIs().
      style <- forestploter::forest_style(
        core = list(
          padding = grid::unit(c(horizontalPadding, 3), "mm"),
          fg_params = list(
            fontface = state$fontface,
            hjust = rep(alignment, each = nrow(state$data)),
            x = I(rep(x, each = nrow(state$data)))
          )
        ),
        colhead = list(
          padding = grid::unit(c(horizontalPadding, 4), "mm"),
          fg_params = list(
            hjust = alignment,
            x = I(x)
          )
        )
      )

      # Build the plot ---------------------------------------------------------

      # Allow effect sizes without CI limits to appear as points.
      # This lets users plot reference values such as 0 or 1 without
      # supplying a CI.
      # forestploter needs both CI limits to draw the point.
      # Set both to the effect size so the CI line has no length.
      # Change only the drawing values and leave the original values unchanged.
      # Skip summary rows because their diamonds need a CI.
      #
      # If transparency or CI end caps are added later, draw only the point
      # for these rows, without CI lines, end caps, or center marks.
      lower <- state$lower
      upper <- state$upper
      estOnly <- !state$is_summary &
        !is.na(state$est) &
        is.na(lower) &
        is.na(upper)
      lower[estOnly] <- state$est[estOnly]
      upper[estOnly] <- state$est[estOnly]

      # Use 0.8 for equal-size markers and as the maximum when weights are
      # supplied. This is within forestploter's documented useful range of
      # about 0.2 to 1.5, giving more prominent squares than its default of
      # 0.4 while leaving room above and below the summary diamonds.
      plot <- forestploter::forest(
        data = state$data,
        est = state$est,
        lower = lower,
        upper = upper,
        sizes = if (is.null(state$weight)) 0.8 else state$weight,
        ci_column = state$ci_column,
        is_summary = state$is_summary,
        style = style
      )

      # Read the supplied values as weights, rather than raw marker sizes.
      # "proportional" scaling follows meta's largest-weight normalization:
      # the largest study gets size 0.8, and square roots of relative weights
      # determine the smaller marker dimensions. Equal study weights therefore
      # give equal sizes of 0.8, matching the default without a Weight input.
      # Unlike "range" scaling, this does not stretch nearly equal weights to
      # opposite ends of the size range.
      #
      # Missing summary weights have a drawing placeholder from preparation;
      # otherwise keep forestploter's handling of missing and summary weights.
      # Its "proportional" method also has a visibility floor of 0.35 times
      # the maximum of 0.8, so very small study markers stop at 0.28.
      # Keep the package's default lower range value of 0.2; "proportional"
      # uses only the upper value for sizing and applies its built-in floor.
      # scale_sizes() rebuilds the plot, but the package reuses its cached
      # table when the display data and table theme have not changed. Accept
      # the rebuild to use this public helper rather than maintaining our own
      # scaling logic solely to avoid it.
      # Sources: forestploter R/scale_sizes.R, R/helper.R, R/draw_forest.R
      # and R/forest.R, cached_table().
      if (!is.null(state$weight)) {
        plot <- forestploter::scale_sizes(
          plot,
          method = "proportional",
          range = c(0.2, 0.8)
        )
      }

      plot
    },

    # Format numeric values as fixed-decimal strings using digits when supplied.
    # When digits is omitted, infer shared decimal places for consistent column
    # formatting, since numeric inputs do not retain trailing zeros. Try zero
    # through four places; if none fits, use two places for the whole column.
    # Generated effect-size/CI columns supply digits directly.
    # Missing values use the supplied replacement string (blank by default).
    .formatNumbers = function(column, digits = NULL, missingText = "") {
      if (is.null(digits)) {
        # The loop below compares each value with its rounded value using
        # abs(values - round(values, candidate)). Missing values propagate
        # through this calculation, and Inf - Inf produces NaN. Exclude both
        # so the comparison cannot make the loop's if condition missing.
        values <- column[is.finite(column)]

        # Zero through four places covers typical hand-entered values, from
        # integer counts to four-decimal rates or p-values. More than four
        # places suggests unrounded or computed values, which fall back to a
        # conventional two-decimal table format. The 1e-8 tolerance (around
        # sqrt(.Machine$double.eps)) ignores binary floating-point noise when
        # choosing digits, while formatC below formats the original values.
        maxDigits <- 4L
        tolerance <- 1e-8
        digits <- 2L
        for (candidate in 0L:maxDigits) {
          if (all(abs(values - round(values, candidate)) < tolerance)) {
            digits <- candidate
            break
          }
        }
      }

      text <- formatC(column, format = "f", digits = digits)
      # Check the original numbers: formatted NA and NaN are character labels,
      # so checking the strings would no longer identify these missing values.
      text[is.na(column)] <- missingText
      text
    },

    # Infer horizontal alignment for a display column:
    #   0   = left   (text or empty text columns)
    #   0.5 = center (numeric combinations: ranges, counts with percentages)
    #   1   = right  (single numbers, percentages, or p-values)
    # Numeric columns align right directly; other columns are checked by text.
    .inferAlignment = function(column) {
      if (is.numeric(column)) {
        return(1)
      }

      # Use as.character() to check factor columns by their text labels.
      # Replace nonbreaking and thin spaces with ordinary spaces: pasted values
      # such as "1 234" and "25 %" may contain these lookalike characters.
      # This also lets trimws() remove cells containing only those spaces.
      # Only this copy is changed; the displayed text keeps its original spaces.
      values <- chartr("\u00a0\u202f\u2009", "   ", as.character(column))
      values <- trimws(values)
      # Ignore missing and blank cells when counting matches. If none remain,
      # use left alignment.
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0L) {
        return(0)
      }

      # Raw strings r"(...)" keep backslashes as written. (?x:...) allows
      # indentation and comments; spaces inside [...] still match spaces.
      # Non-capturing groups (?:...) keep alternatives together without saving
      # matched text. Use perl = TRUE for this syntax.
      # Leave ^ and $ out of the number pattern so we can reuse it inside
      # ranges and other combinations. Add them when matching a whole cell.
      number <- r"((?x:
        # Optional comparison, with or without p/P: <0.01 or P = 0.05
        (?:(?:[pP]\s*)?[<>=≤≥]{1,2}\s*)?

        # Optional plus or minus sign: +, -, or −
        [+−-]?

        (?:
          # Digits before the decimal point: 1234 or 1,234
          (?:[0-9]{1,3}(?:[, '’][0-9]{3})+|[0-9]+)
          # Optional decimal part: 1234.56 or 1,234.56
          (?:\.[0-9]*)?
          |
          # Or a decimal without a leading zero: .25
          \.[0-9]+
        )

        # Optional exponent: 1.2e-3
        (?:[eE][+−-]?[0-9]+)?

        # Optional percent sign
        \s*%?
      ))"
      # Match the whole cell, allowing up to three trailing significance stars.
      isNumber <- grepl(
        paste0("^", number, "\\s*\\*{0,3}$"),
        values,
        perl = TRUE
      )

      # Require more than half the nonblank cells to match. This allows
      # occasional annotations such as "Reference" or "Not estimable" without
      # changing the column's alignment. If neither category exceeds 50%,
      # keep left alignment.
      if (mean(isNumber) > 0.5) {
        return(1)
      }

      # A pair is two numbers separated by punctuation, a symbol, or "to".
      # Examples include ranges, events/totals, mean +/- SD, and statistics
      # followed by p-values:
      #   "0.80, 1.25", "1.2 to 3.4", "15/120", "12 +/- 3"
      #   "152, P < 0.001", "23.1, p = 0.006"
      pair <- paste0(
        number,
        "\\s*(?:,|;|to|/|±|\\+/-|–|−|-)\\s*",
        number
      )

      # A pair enclosed in matching round or square brackets.
      # Examples: "(0.80, 1.25)" or "[0.80, 1.25]".
      # fmt: skip
      bracketedPair <- paste0(
        "(?:\\[\\s*", pair, "\\s*\\]|",
        "\\(\\s*", pair, "\\s*\\))"
      )

      # A number enclosed in matching round or square brackets.
      # Examples: "(15.3%)" or "[0.05]".
      # fmt: skip
      bracketedNumber <- paste0(
        "(?:\\(\\s*", number, "\\s*\\)|",
        "\\[\\s*", number, "\\s*\\])"
      )

      # Match numeric combinations that read best when centered.
      # fmt: skip
      compound <- paste0(
        "^(?:",
        # A pair on its own.
        pair, "|",

        # A bracketed number or pair, alone or after a number or pair:
        # "(15.3%)", "[0.80, 1.25]", "1.05 [0.80, 1.25]",
        # "42 (15.3%)", "15/120 (12.5%)", "15/120 [0.08, 0.20]".
        "(?:(?:", pair, "|", number, ")\\s*)?",
        "(?:", bracketedPair, "|", bracketedNumber, ")",

        # Allow up to three significance stars after the complete value.
        ")\\s*\\*{0,3}$"
      )

      # Without a space after the comma, "1,234" matches both a grouped number
      # and a pair. Count it only as a single number. With a space, "1, 234"
      # does not match thousands grouping and counts as a pair.
      isCompound <- !isNumber & grepl(compound, values, perl = TRUE)
      if (mean(isCompound) > 0.5) {
        return(0.5)
      }
      0
    }
  )
)
