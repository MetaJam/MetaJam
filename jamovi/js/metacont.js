const regression = require("./regression");

module.exports = {
  // Fires when the Supplier needs to refresh its available items.
  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  // Fires when the user adds/removes variables in Covariates
  metaRegCovs_changed: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  // Fires when the user adds/removes variables in Factors
  metaRegFactors_changed: function (ui) {
    regression.updateModelTerms(ui, this);
  },

  // Fires when the user manually reorders terms (drag/drop inside ListBox)
  metaRegTerms_changed: function (ui) {
    regression.enforceTermOrder(ui);
  },
};
