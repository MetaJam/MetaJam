const effectSizeLabels = {
  GEN: "Effect Size",
  HR: "Log Hazard Ratio",
  MD: "Mean Difference",
  SMD: "Standardised Mean Difference",
  ROM: "Log Ratio of Means",
  RR: "Log Risk Ratio",
  OR: "Log Odds Ratio",
  RD: "Risk Difference",
  IRR: "Log Incidence Rate Ratio",
};

const updateEffectSizeLabel = function (ui) {
  const label = effectSizeLabels[ui.sm.value()];
  ui.effectSizeTarget.setPropertyValue("label", label);
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
