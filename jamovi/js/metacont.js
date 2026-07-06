const regression = require("./regression");
const sort = require("./sort");

module.exports = {
  // Set initial enabled state and relabel blocks when the panel opens
  view_updated: function (ui) {
    regression.updateEnableState(ui);
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when the main VariableSupplier refreshes its available variables.
  // Rebuilds all Sort by ComboBoxes from the current supplier items.
  mainVariablesSupplier_changed: function (ui) {
    sort.refresh(ui, ui.mainVariablesSupplier, [
      sort.main,
      sort.subgroup,
      sort.leaveOneOut,
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

  // Fires when the Supplier needs to refresh its available items.
  // Keep this supplier-only: block syncing belongs to covs/factors changes.
  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelSupplier(ui);
  },

  // Fires when the user adds/removes variables in Covariates
  metaRegCovs_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  // Fires when the user adds/removes variables in Factors
  metaRegFactors_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  // Fires when blocks array value changes (e.g. null blocks from adding)
  metaRegBlocks_changed: function (ui) {
    regression.checkForNullBlocks(ui, this);
  },

  // Fires when user adds a new block — relabel all blocks
  metaRegBlocks_listItemAdded: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when user removes a block — relabel remaining blocks
  metaRegBlocks_listItemRemoved: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when terms inside a block change (drag/drop reorder)
  blockList_changed: function (ui) {
    regression.enforceBlockTermOrder(ui);
  },
};
