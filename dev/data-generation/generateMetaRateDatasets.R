# Generate the single incidence rates meta-analysis example datasets.

output_dir <- file.path("dev", "data-generation")

# Catheter-Related Bloodstream Infections -------------------------------
catheter_source <- metadat::dat.nielweise2008

catheter <- data.frame(
  "Study" = paste0(
    catheter_source$authors,
    " (",
    catheter_source$year,
    ")"
  ),
  "Events" = catheter_source$x2i,
  "Catheter-Days" = catheter_source$t2i,
  "Year" = catheter_source$year,
  check.names = FALSE
)

attr(catheter$Study, "jmv-id") <- TRUE

catheter_descriptions <- c(
  "Events" = paste(
    "Number of catheter-related bloodstream infections in patients receiving",
    "standard central venous catheters"
  ),
  "Catheter-Days" = paste(
    "Total number of catheter-days in patients receiving standard central",
    "venous catheters"
  ),
  "Year" = "Publication year"
)

for (variable in names(catheter_descriptions)) {
  attr(catheter[[variable]], "jmv-desc") <-
    catheter_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  catheter,
  file.path(output_dir, "CatheterRelatedBloodstreamInfections.omv"),
  frcWrt = TRUE
)
