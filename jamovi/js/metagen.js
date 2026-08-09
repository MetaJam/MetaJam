const regression = require("./regression");
const sort = require("./sort");

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

    regression.updateModelLabels(ui.metaRegBlocks);
    // See regression.updateModelTerms(): this panel-update call seeds
    // findChanges() before the first user-added moderator.
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  sm_changed: function (ui) {
    updateEffectSizeLabel(ui);
  },

  // Fires when the main VariableSupplier refreshes its available variables.
  // Rebuilds all Sort by ComboBoxes from the current supplier items.
  mainVariablesSupplier_changed: function (ui) {
    sort.refresh(ui, ui.mainVariablesSupplier, [
      sort.main,
      sort.subgroup,
      sort.leaveOneOut,
      sort.cumulative,
    ]);
  },

  // Fires when the user changes the main forest plot Sort by ComboBox.
  // Copies variable selections into the hidden Variable option for R.
  sortBy_changed: function (ui) {
    sort.syncVariable(ui, ui.mainVariablesSupplier, sort.main);
  },

  // Fires when the user changes a subgroup forest plot Sort by ComboBox.
  subgroupSortBy_changed: function (ui) {
    sort.syncVariable(ui, ui.mainVariablesSupplier, sort.subgroup);
  },

  // Fires when the user changes the leave-one-out forest plot Sort by ComboBox.
  leaveOneOutSortBy_changed: function (ui) {
    sort.syncVariable(ui, ui.mainVariablesSupplier, sort.leaveOneOut);
  },

  // Fires when the user changes the cumulative meta-analysis Sort by ComboBox.
  cumulativeSortBy_changed: function (ui) {
    sort.syncVariable(ui, ui.mainVariablesSupplier, sort.cumulative);
  },

  // Fires when the Supplier needs to refresh its available items.
  // Keep this supplier-only: block syncing belongs to covs/factors changes.
  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelSupplier(ui);
  },

  // Fires when the user adds/removes variables in Covariates.
  metaRegCovs_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  // Fires when the user adds/removes variables in Factors.
  metaRegFactors_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  // Fires when blocks array value changes (e.g. null blocks from adding).
  metaRegBlocks_changed: function (ui) {
    regression.checkForNullBlocks(ui, this);
  },

  // Fires when user adds a new block — relabel all blocks.
  metaRegBlocks_listItemAdded: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when user removes a block — relabel remaining blocks.
  metaRegBlocks_listItemRemoved: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when terms inside a block change (drag/drop reorder).
  blockList_changed: function (ui) {
    regression.enforceBlockTermOrder(ui);
  },
};
