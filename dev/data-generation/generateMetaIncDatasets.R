# Generate the incidence rate outcomes meta-analysis example datasets.

output_dir <- file.path("dev", "data-generation")

# Adjusted-Dose Warfarin --------------------------------------------------
hart_source <- metadat::dat.hart1999

hart <- data.frame(
  "Study" = paste0(hart_source$study, " (", hart_source$year, ")"),
  "Events (Experimental)" = hart_source$x1i,
  "Person-Years (Experimental)" = hart_source$t1i,
  "Events (Control)" = hart_source$x2i,
  "Person-Years (Control)" = hart_source$t2i,
  "Year" = hart_source$year,
  "Comparison Group" = hart_source$compgrp |>
    forcats::fct_infreq() |>
    forcats::fct_recode(
      "Placebo" = "placebo",
      "Control" = "control"
    ),
  "Prevention Type" = hart_source$prevtype |>
    forcats::fct_infreq() |>
    forcats::fct_recode(
      "Primary" = "primary",
      "Secondary" = "secondary"
    ),
  "Target INR Range" = hart_source$trinr,
  check.names = FALSE
)

attr(hart$Study, "jmv-id") <- TRUE

hart_descriptions <- c(
  "Events (Experimental)" =
    "Number of strokes in the adjusted-dose warfarin group",
  "Person-Years (Experimental)" =
    "Person-years of follow-up in the adjusted-dose warfarin group",
  "Events (Control)" =
    "Number of strokes in the placebo or control group",
  "Person-Years (Control)" =
    "Person-years of follow-up in the placebo or control group",
  "Year" =
    "Publication year",
  "Comparison Group" =
    "Type of comparison group used in the trial",
  "Prevention Type" =
    "Whether the trial evaluated primary or secondary prevention of stroke",
  "Target INR Range" = paste(
    "Target international normalized ratio (INR) range for adjusted-dose",
    "warfarin"
  )
)

for (variable in names(hart_descriptions)) {
  attr(hart[[variable]], "jmv-desc") <- hart_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  hart,
  file.path(output_dir, "AdjustedDoseWarfarin.omv"),
  frcWrt = TRUE
)
