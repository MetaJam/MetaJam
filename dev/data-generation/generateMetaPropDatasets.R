# Generate the single proportions meta-analysis example datasets.

output_dir <- file.path("dev", "data-generation")

# Prescription Opioid Misuse -----------------------------------------------
opioid_source <- dmetar::OpioidMisuse

opioid <- data.frame(
  "Study" = sub(", ", " (", paste0(opioid_source$author, ")")),
  "Events" = opioid_source$event,
  "Total" = opioid_source$n,
  "Year" = as.integer(sub(".*, ", "", opioid_source$author)),
  check.names = FALSE
)

attr(opioid$Study, "jmv-id") <- TRUE

opioid_descriptions <- c(
  "Events" = "Number of prescription opioid misuse cases",
  "Total" = "Number of participants",
  "Year" = "Publication year"
)

for (variable in names(opioid_descriptions)) {
  attr(opioid[[variable]], "jmv-desc") <- opioid_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  opioid,
  file.path(output_dir, "PrescriptionOpioidMisuse.omv"),
  frcWrt = TRUE
)
