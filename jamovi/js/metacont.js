const regression = require("./regression");

module.exports = {
  // Set initial enabled state and relabel blocks when the panel opens
  view_updated: function (ui) {
    regression.updateEnableState(ui);
    regression.updateModelLabels(ui.metaRegBlocks);
  },

  // Fires when the Supplier needs to refresh its available items.
  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelTerms(ui, this);
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
