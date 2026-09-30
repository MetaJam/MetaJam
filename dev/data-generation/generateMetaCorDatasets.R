# Generate the correlations meta-analysis example datasets.

output_dir <- file.path("dev", "data-generation")

# Health and Well-Being -----------------------------------------------------
health_source <- dmetar::HealthWellbeing

health <- data.frame(
  "Study" = sub(", ", " (", paste0(health_source$author, ")")),
  "Correlation" = health_source$cor,
  "Total" = health_source$n,
  "Year" = as.integer(sub(".*, ", "", health_source$author)),
  "Population" = health_source$population |>
    forcats::fct_infreq() |>
    forcats::fct_recode(
      "General population" = "general population",
      "Chronic condition" = "chronic condition"
    ),
  "Country" = health_source$country |>
    forcats::fct_infreq(),
  check.names = FALSE
)

attr(health$Study, "jmv-id") <- TRUE

health_descriptions <- c(
  "Correlation" =
    "Pearson correlation between health status and subjective well-being",
  "Total" = "Number of participants",
  "Year" = "Publication year",
  "Population" = "Type of population sampled in the study",
  "Country" = "Country or region where the study was conducted"
)

for (variable in names(health_descriptions)) {
  attr(health[[variable]], "jmv-desc") <- health_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  health,
  file.path(output_dir, "HealthAndWellbeing.omv"),
  frcWrt = TRUE
)
