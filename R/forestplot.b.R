forestPlotClass <- R6::R6Class(
  "forestPlotClass",
  inherit = forestPlotBase,

  private = list(
    # This analysis always displays its plot, so no visibility checks are needed.
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
      left <- self$options$leftColumns
      right <- self$options$rightColumns

      # Convert display columns to text and show missing values as blanks.
      # Check the original values: converting NaN to text gives the literal "NaN".
      displayColumns <- lapply(c(left, right), function(variable) {
        column <- data[[variable]]
        ifelse(is.na(column), "", as.character(column))
      })

      # Use spaces to size the CI column, following forestploter's examples.
      # The target is inspired by meta's standard 6 cm plotting area.
      # With default settings on a Windows ragg device, 48 spaces measured about
      # 5.64 cm; table padding added 0.40 cm, giving a column about 6.04 cm wide.
      # The physical width varies with the font and device.
      nLeftColumns <- length(left)
      plotColumns <- c(
        displayColumns[seq_len(nLeftColumns)],
        list(rep(strrep(" ", 48), nrow(data))),
        displayColumns[nLeftColumns + seq_len(length(right))]
      )
      plotData <- as.data.frame(plotColumns)
      names(plotData) <- c(left, " ", right)

      # Store the display table and numeric inputs in image state below so rendering
      # and export can construct the plot without reloading the raw dataset.
      state <- list(
        data = plotData,
        est = jmvcore::toNumeric(data[[self$options$effectSize]]),
        lower = jmvcore::toNumeric(data[[self$options$ciLower]]),
        upper = jmvcore::toNumeric(data[[self$options$ciUpper]]),
        ci_column = nLeftColumns + 1L
      )

      # Measure the plot dimensions.
      oldDev <- grDevices::dev.cur()

      # Open a non-rendering ragg device to query systemfonts/textshaping metrics
      # without allocating an in-memory pixel buffer or creating temporary files.
      ragg::agg_record()
      on.exit({
        grDevices::dev.off()
        if (oldDev > 1) grDevices::dev.set(oldDev)
      })

      # Print devices normally initialize the graphics-engine display list with
      # recording OFF. ragg::agg_record() is different: it deliberately initializes
      # that display list with recording ON so graphics can be captured later with
      # recordPlot(). This function does not use recordPlot(). grid::grid.grab()
      # reads grid's separate display list, which remains available when the
      # graphics-engine list is inhibited. Turning off the unused engine list here
      # avoids recording a second copy of the drawing operations in memory.
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
      forestploter::forest(
        data = state$data,
        est = state$est,
        lower = state$lower,
        upper = state$upper,
        ci_column = state$ci_column
      )
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
