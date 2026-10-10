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

      data <- self$data
      left <- c(self$options$rowLabel, self$options$leftColumns)
      right <- self$options$rightColumns

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

      # Convert display columns to text and show missing values as blanks.
      # Check the original values: converting NaN to text gives the literal
      # "NaN".
      displayColumns <- lapply(c(left, right), function(variable) {
        column <- data[[variable]]
        ifelse(is.na(column), "", as.character(column))
      })

      # Use spaces to size the CI column, following forestploter's examples.
      # The target is inspired by meta's standard 6 cm plotting area.
      # With default settings on a Windows ragg device, 48 spaces measured about
      # 5.64 cm; table padding added 0.40 cm, giving a column about 6.04 cm
      # wide. The physical width varies with the font and device.
      nLeftColumns <- length(left)
      plotColumns <- c(
        displayColumns[seq_len(nLeftColumns)],
        list(rep(strrep(" ", 48), nrow(data))),
        displayColumns[nLeftColumns + seq_len(length(right))]
      )
      plotData <- as.data.frame(plotColumns)
      names(plotData) <- c(left, " ", right)

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
      isSummary <- rowTypes == "summary"
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

      # Store the display table and numeric inputs in image state below so
      # rendering and export can construct the plot without reloading the raw
      # dataset.
      state <- list(
        data = plotData,
        est = jmvcore::toNumeric(data[[self$options$effectSize]]),
        lower = jmvcore::toNumeric(data[[self$options$ciLower]]),
        upper = jmvcore::toNumeric(data[[self$options$ciUpper]]),
        weight = weight,
        ci_column = nLeftColumns + 1L,
        is_summary = isSummary,
        fontface = as.vector(fontface)
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

      # Print devices normally initialize the graphics-engine display list with
      # recording OFF. ragg::agg_record() is different: it deliberately
      # initializes that display list with recording ON so graphics can be
      # captured later with recordPlot(). This function does not use
      # recordPlot(). grid::grid.grab() reads grid's separate display list,
      # which remains available when the graphics-engine list is inhibited.
      # Turning off the unused engine list here avoids recording a second copy
      # of the drawing operations in memory.
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

    .buildPlot = function(state) {
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
      # Full cell-formatting vectors use column-wise order, as prepared above.
      # Source: gridExtra R/tableGrob.r (theme structure and cell order).
      style <- forestploter::forest_style(
        core = list(fg_params = list(fontface = state$fontface))
      )

      # Use 0.8 for equal-size markers and as the maximum when weights are
      # supplied. This is within forestploter's documented useful range of
      # about 0.2 to 1.5, giving more prominent squares than its default of
      # 0.4 while leaving room above and below the summary diamonds.
      plot <- forestploter::forest(
        data = state$data,
        est = state$est,
        lower = state$lower,
        upper = state$upper,
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
    }
  )
)
