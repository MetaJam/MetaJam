const VARIABLE_PREFIX = "varid::";
const DEFAULT_VALUE = "none";

/**
 * Fixed sort choices used by main and subgroup forest plots.
 */
const mainSortChoices = [
  { title: "Original order", name: DEFAULT_VALUE },
  { title: "Effect size", name: "effect" },
  { title: "Weight", name: "weight" },
];

/**
 * Fixed sort choices used by leave-one-out forest plots.
 */
const leaveOneOutSortChoices = [
  { title: "Original order", name: DEFAULT_VALUE },
  { title: "Effect size", name: "effect" },
  { title: "I²", name: "i2" },
  { title: "Tau²", name: "tau2" },
];

/**
 * Sort option names for the main forest plot.
 */
const main = {
  sortBy: "sortBy",
  sortVariable: "sortVariable",
  choices: mainSortChoices,
};

/**
 * Sort option names for subgroup forest plots.
 */
const subgroup = {
  sortBy: "subgroupSortBy",
  sortVariable: "subgroupSortVariable",
  choices: mainSortChoices,
};

/**
 * Sort option names for leave-one-out forest plots.
 */
const leaveOneOut = {
  sortBy: "leaveOneOutSortBy",
  sortVariable: "leaveOneOutSortVariable",
  choices: leaveOneOutSortChoices,
};

/**
 * Build ComboBox options from the VariableSupplier.
 *
 * The ComboBox stores the stable jamovi column id (`varid::<id>`) while
 * showing the current variable name. The hidden Variable option stores the
 * name that R needs for data loading.
 *
 * @param {Object} supplier - The main VariableSupplier control.
 * @returns {Array<Object>} ComboBox option objects.
 */
const makeVariableOptions = function (supplier) {
  const supplierItems = supplier.value();
  const variableOptions = [];

  for (let i = 0; i < supplierItems.length; i++) {
    const variableName = supplierItems[i].value.raw;
    const columnId = supplierItems[i].properties.id;

    variableOptions.push({
      title: "Variable: " + variableName,
      name: VARIABLE_PREFIX + columnId,
    });
  }

  return variableOptions;
};

/**
 * Sync the hidden Variable option from the visible Sort by ComboBox.
 *
 * Fixed choices clear the hidden variable. Variable choices use the stored
 * column id to look up the current variable name from the supplier.
 *
 * @param {Object} ui       - The UI controls object.
 * @param {Object} supplier - The main VariableSupplier control.
 * @param {Object} config   - Option names for one sort control.
 */
const syncVariable = function (ui, supplier, config) {
  const sortBy = ui[config.sortBy];
  const sortVariable = ui[config.sortVariable];
  const sortValue = sortBy.value();
  let variable = null;

  if (sortValue.indexOf(VARIABLE_PREFIX) === 0) {
    // sortBy keeps the stable column id; sortVariable keeps the current name.
    const columnId = sortValue.substring(VARIABLE_PREFIX.length);
    const supplierItems = supplier.value();

    for (let i = 0; i < supplierItems.length; i++) {
      if (String(supplierItems[i].properties.id) === columnId) {
        variable = supplierItems[i].value.raw;
        break;
      }
    }
  }

  if (sortVariable.value() !== variable) {
    sortVariable.setValue(variable);
  }
};

/**
 * Reset one sort control to original order.
 *
 * runInEditScope groups the visible sortBy and hidden sortVariable writes
 * into the same option edit.
 *
 * @param {Object} ui     - The UI controls object.
 * @param {Object} config - Option names for one sort control.
 */
const resetSelection = function (ui, config) {
  const sortBy = ui[config.sortBy];
  const sortVariable = ui[config.sortVariable];

  ui.view.model.options.runInEditScope(function () {
    sortBy.setValue(DEFAULT_VALUE);
    sortVariable.setValue(null);
  });
};

/**
 * Refresh the available choices for one Sort by ComboBox.
 *
 * If the current value still exists, keep it and sync the hidden variable.
 * If its column was removed, reset to original order.
 *
 * @param {Object} ui              - The UI controls object.
 * @param {Object} supplier        - The main VariableSupplier control.
 * @param {Object} config          - Option names for one sort control.
 * @param {Array<Object>} variableOptions - Dataset variable choices.
 */
const refreshSort = function (ui, supplier, config, variableOptions) {
  const sortBy = ui[config.sortBy];
  const currentSortValue = sortBy.value();
  const comboOptions = config.choices.concat(variableOptions);

  sortBy.setPropertyValue("options", comboOptions);

  for (let i = 0; i < comboOptions.length; i++) {
    if (comboOptions[i].name === currentSortValue) {
      syncVariable(ui, supplier, config);
      return;
    }
  }

  resetSelection(ui, config);
};

/**
 * Refresh all Sort by ComboBoxes for an analysis.
 *
 * Called when the main VariableSupplier changes, including column add/delete
 * and variable rename events.
 *
 * @param {Object} ui       - The UI controls object.
 * @param {Object} supplier - The main VariableSupplier control.
 * @param {Array<Object>} sorts - Sort configs to refresh.
 */
const refresh = function (ui, supplier, sorts) {
  const variableOptions = makeVariableOptions(supplier);

  for (let i = 0; i < sorts.length; i++) {
    refreshSort(ui, supplier, sorts[i], variableOptions);
  }
};

module.exports = {
  main: main,
  subgroup: subgroup,
  leaveOneOut: leaveOneOut,
  refresh: refresh,
  syncVariable: syncVariable,
};
