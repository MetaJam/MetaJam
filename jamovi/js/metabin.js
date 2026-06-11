const regression = require("./regression");

module.exports = {
  view_updated: function (ui) {
    regression.updateEnableState(ui);
    regression.updateModelLabels(ui.metaRegBlocks);
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
