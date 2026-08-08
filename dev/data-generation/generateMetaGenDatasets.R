# Generate the precomputed-effect-size example datasets.

output_dir <- file.path("dev", "data-generation")

# Environmental Tobacco Smoke ----------------------------------------------
hackshaw_source <- metadat::dat.hackshaw1998

hackshaw <- data.frame(
  "Study" = paste0(hackshaw_source$author, " (", hackshaw_source$year, ")"),
  "Log Odds Ratio" = hackshaw_source$yi,
  "Standard Error" = sqrt(hackshaw_source$vi),
  "Odds Ratio" = hackshaw_source$or,
  "Lower 95% CI" = hackshaw_source$or.lb,
  "Upper 95% CI" = hackshaw_source$or.ub,
  "Year" = hackshaw_source$year,
  "Study Design" = hackshaw_source$design |>
    forcats::fct_infreq() |>
    forcats::fct_recode(
      "Case-control" = "case-control",
      "Cohort" = "cohort"
    ),
  "Country" = hackshaw_source$country |>
    forcats::fct_infreq(),
  check.names = FALSE
)

attr(hackshaw$Study, "jmv-id") <- TRUE

hackshaw_descriptions <- c(
  "Log Odds Ratio" = paste(
    "Log odds ratio for lung cancer in women exposed versus unexposed to",
    "environmental tobacco smoke from their smoking spouse"
  ),
  "Standard Error" = "Standard error of the log odds ratio",
  "Odds Ratio" = paste(
    "Odds ratio for lung cancer in women exposed versus unexposed to",
    "environmental tobacco smoke from their smoking spouse"
  ),
  "Lower 95% CI" = "Lower limit of the 95% confidence interval for the odds ratio",
  "Upper 95% CI" = "Upper limit of the 95% confidence interval for the odds ratio",
  "Year" = "Publication year",
  "Country" = "Country where the study was conducted"
)

for (variable in names(hackshaw_descriptions)) {
  attr(hackshaw[[variable]], "jmv-desc") <- hackshaw_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  hackshaw,
  file.path(output_dir, "EnvironmentalTobaccoSmoke.omv"),
  frcWrt = TRUE
)

# Purine Analogues ----------------------------------------------------------
steurer_source <- metabook::Steurer2006

steurer <- data.frame(
  "Study" = paste0(steurer_source$author, " (", steurer_source$year, ")"),
  "Log Hazard Ratio" = steurer_source$lnHR,
  "Standard Error" = steurer_source$selnHR,
  "Hazard Ratio" = steurer_source$HR,
  "Lower 95% CI" = steurer_source$lowHR,
  "Upper 95% CI" = steurer_source$uppHR,
  "Total" = steurer_source$Ne + steurer_source$Nc,
  "Year" = steurer_source$year,
  check.names = FALSE
)

attr(steurer$Study, "jmv-id") <- TRUE

steurer_descriptions <- c(
  "Log Hazard Ratio" = paste(
    "Log hazard ratio for overall survival in patients treated with",
    "purine analogues versus alkylator-based regimens"
  ),
  "Standard Error" = "Standard error of the log hazard ratio",
  "Hazard Ratio" = paste(
    "Hazard ratio for overall survival in patients treated with",
    "purine analogues versus alkylator-based regimens"
  ),
  "Lower 95% CI" = "Lower limit of the 95% confidence interval for the hazard ratio",
  "Upper 95% CI" = "Upper limit of the 95% confidence interval for the hazard ratio",
  "Total" = "Number of participants in both treatment groups",
  "Year" = "Publication year"
)

for (variable in names(steurer_descriptions)) {
  attr(steurer[[variable]], "jmv-desc") <- steurer_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  steurer,
  file.path(output_dir, "PurineAnalogues.omv"),
  frcWrt = TRUE
)
