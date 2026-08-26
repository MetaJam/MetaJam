# Generate the risk-of-bias example datasets.

output_dir <- file.path("dev", "data-generation")

write_rob_dataset <- function(data, file, judgements, descriptions) {
  data[-1] <- lapply(data[-1], function(variable) {
    forcats::fct_relevel(variable, intersect(judgements, variable))
  })

  attr(data$Study, "jmv-id") <- TRUE

  for (variable in names(descriptions)) {
    attr(data[[variable]], "jmv-desc") <- descriptions[[variable]]
  }

  jmvReadWrite::write_omv(
    data,
    file.path(output_dir, file),
    frcWrt = TRUE
  )
}

# RoB 2 ---------------------------------------------------------------------
write_rob_dataset(
  data = robvis::data_rob2,
  file = "RoB2.omv",
  judgements = c("Low", "Some concerns", "High", "No information"),
  descriptions = c(
    "D1" = "Domain 1: Bias arising from the randomization process",
    "D2" = "Domain 2: Bias due to deviations from intended interventions",
    "D3" = "Domain 3: Bias due to missing outcome data",
    "D4" = "Domain 4: Bias in measurement of the outcome",
    "D5" = "Domain 5: Bias in selection of the reported result",
    "Overall" = "Overall risk of bias"
  )
)

# RoB 2 (Cluster) -----------------------------------------------------------
write_rob_dataset(
  data = robvis::data_rob2_cluster,
  file = "RoB2Cluster.omv",
  judgements = c(
    "Low",
    "Some concerns",
    "High",
    "No information",
    "Not applicable"
  ),
  descriptions = c(
    "D1" = "Domain 1: Bias arising from the randomization process",
    "D1b" = paste(
      "Domain 1b: Bias arising from the timing of identification and",
      "recruitment of individual participants in relation to the timing of",
      "randomization"
    ),
    "D2" = "Domain 2: Bias due to deviations from intended interventions",
    "D3" = "Domain 3: Bias due to missing outcome data",
    "D4" = "Domain 4: Bias in measurement of the outcome",
    "D5" = "Domain 5: Bias in selection of the reported result",
    "Overall" = "Overall risk of bias"
  )
)

# ROBINS-I ------------------------------------------------------------------
write_rob_dataset(
  data = robvis::data_robins_i,
  file = "ROBINSI.omv",
  judgements = c("Low", "Moderate", "Serious", "Critical", "No information"),
  descriptions = c(
    "D1" = "Domain 1: Bias due to confounding",
    "D2" = "Domain 2: Bias due to selection of participants",
    "D3" = "Domain 3: Bias in classification of interventions",
    "D4" = "Domain 4: Bias due to deviations from intended interventions",
    "D5" = "Domain 5: Bias due to missing data",
    "D6" = "Domain 6: Bias in measurement of outcomes",
    "D7" = "Domain 7: Bias in selection of the reported result",
    "Overall" = "Overall risk of bias"
  )
)

# ROBINS-E ------------------------------------------------------------------
write_rob_dataset(
  data = robvis::data_robins_e,
  file = "ROBINSE.omv",
  judgements = c(
    "Low",
    "Some concerns",
    "High",
    "Very high",
    "No information"
  ),
  descriptions = c(
    "D1" = "Domain 1: Bias due to confounding",
    "D2" = "Domain 2: Bias arising from measurement of the exposure",
    "D3" = paste(
      "Domain 3: Bias in selection of participants into the study",
      "(or into the analysis)"
    ),
    "D4" = "Domain 4: Bias due to post-exposure interventions",
    "D5" = "Domain 5: Bias due to missing data",
    "D6" = "Domain 6: Bias arising from measurement of the outcome",
    "D7" = "Domain 7: Bias in selection of the reported result",
    "Overall" = "Overall risk of bias"
  )
)

# QUADAS-2 ------------------------------------------------------------------
write_rob_dataset(
  data = robvis::data_quadas,
  file = "QUADAS2.omv",
  judgements = c("Low", "Some concerns", "High", "No information"),
  descriptions = c(
    "D1" = "Domain 1: Patient selection",
    "D2" = "Domain 2: Index test",
    "D3" = "Domain 3: Reference standard",
    "D4" = "Domain 4: Flow and timing",
    "Overall" = "Overall risk of bias"
  )
)

# QUIPS ---------------------------------------------------------------------
write_rob_dataset(
  data = robvis::data_quips,
  file = "QUIPS.omv",
  judgements = c("Low", "Moderate", "High", "No information"),
  descriptions = c(
    "D1" = "Domain 1: Bias due to participation",
    "D2" = "Domain 2: Bias due to attrition",
    "D3" = "Domain 3: Bias due to prognostic factor measurement",
    "D4" = "Domain 4: Bias due to outcome measurement",
    "D5" = "Domain 5: Bias due to confounding",
    "D6" = "Domain 6: Bias in statistical analysis and reporting",
    "Overall" = "Overall risk of bias"
  )
)
