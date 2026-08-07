const analysisScaleEffectSizeLabels = {
  GEN: "Effect Size",
  HR: "Log Hazard Ratio",
  MD: "Mean Difference",
  SMD: "Standardised Mean Difference",
  ROM: "Log Ratio of Means",
  RR: "Log Risk Ratio",
  OR: "Log Odds Ratio",
  RD: "Risk Difference",
  VE: "Log Vaccine Ratio",
  IRR: "Log Incidence Rate Ratio",
  IRD: "Incidence Rate Difference",
  IRSD: "Square Root Transformed Incidence Rate Difference",
};

const naturalScaleEffectSizeLabels = {
  GEN: "Effect Size",
  HR: "Hazard Ratio",
  MD: "Mean Difference",
  SMD: "Standardised Mean Difference",
  ROM: "Ratio of Means",
  RR: "Risk Ratio",
  OR: "Odds Ratio",
  RD: "Risk Difference",
  VE: "Vaccine Efficacy/Effectiveness",
  IRR: "Incidence Rate Ratio",
  IRD: "Incidence Rate Difference",
  IRSD: "Square Root Transformed Incidence Rate Difference",
};

const updateEffectSizeLabel = function (ui) {
  const effectMeasure = ui.sm.value();
  ui.effectSizeTarget.setPropertyValue(
    "label",
    analysisScaleEffectSizeLabels[effectMeasure],
  );
  ui.ciEffectSizeTarget.setPropertyValue(
    "label",
    naturalScaleEffectSizeLabels[effectMeasure],
  );
};

module.exports = {
  view_updated: function (ui) {
    // TargetLayoutBox labels are not saved with analysis options; opening an
    // analysis restores the YAML label, so reapply it for the saved measure.
    updateEffectSizeLabel(ui);
  },

  sm_changed: function (ui) {
    updateEffectSizeLabel(ui);
  },
};
