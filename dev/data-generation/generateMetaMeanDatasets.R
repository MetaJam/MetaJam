# Generate the single means meta-analysis example datasets.

output_dir <- file.path("dev", "data-generation")

# Depression Severity -------------------------------------------------------
bdi_source <- dmetar::BdiScores

bdi <- data.frame(
  "Study" = sub(", ", " (", paste0(bdi_source$author, ")")),
  "Mean" = bdi_source$mean,
  "SD" = bdi_source$sd,
  "Total" = as.integer(bdi_source$n),
  "Year" = as.integer(sub(".*, ", "", bdi_source$author)),
  check.names = FALSE
)

attr(bdi$Study, "jmv-id") <- TRUE

bdi_descriptions <- c(
  "Mean" = "Mean BDI-II score at baseline",
  "SD" = "Standard deviation of BDI-II scores at baseline",
  "Total" = "Number of participants assessed at baseline",
  "Year" = "Publication year"
)

for (variable in names(bdi_descriptions)) {
  attr(bdi[[variable]], "jmv-desc") <- bdi_descriptions[[variable]]
}

jmvReadWrite::write_omv(
  bdi,
  file.path(output_dir, "DepressionSeverity.omv"),
  frcWrt = TRUE
)
