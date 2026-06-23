const regression = require("./regression");
const sort = require("./sort");

module.exports = {
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

  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  metaRegCovs_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  metaRegFactors_changed: function (ui) {
    regression.updateModelTerms(ui, this);
    regression.updateEnableState(ui);
  },

  metaRegBlocks_changed: function (ui) {
    regression.checkForNullBlocks(ui, this);
  },

  metaRegBlocks_listItemAdded: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  metaRegBlocks_listItemRemoved: function (ui) {
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  blockList_changed: function (ui) {
    regression.enforceBlockTermOrder(ui);
  },
};
