/**
 * Update the predictor Supplier from metaRegCovs and metaRegFactors.
 *
 * metaRegModelSupplier_updated calls this directly because the Supplier items
 * are derived UI state. updateModelTerms() also calls it first, then reuses
 * the returned variable list for the block diff.
 *
 * This function only updates metaRegModelSupplier. It does not touch
 * metaRegBlocks; block syncing belongs to updateModelTerms(), and jamovi core
 * handles column renames inside option values.
 *
 * @param {Object} ui - The UI controls object.
 * @returns {string[]} The current meta-regression variable names.
 */
const updateModelSupplier = function (ui) {
  // 1. Combine both variable slots into one list
  const covsList = utils.clone(ui.metaRegCovs.value(), []);
  const factorsList = utils.clone(ui.metaRegFactors.value(), []);
  const allVars = covsList.concat(factorsList);

  // 2. Feed the combined list to the Supplier (creates proper
  //    FormattedValue items that the Supplier knows how to render)
  ui.metaRegModelSupplier.setValue(
    utils.valuesToItems(allVars, FormatDef.variable),
  );

  return allVars;
};

/**
 * Shared model builder logic for meta-regression blocks.
 *
 * Keeps the metaRegModelSupplier and metaRegBlocks controls in sync with
 * metaRegCovs and metaRegFactors.
 *
 * When the user adds or removes a variable, this function:
 *   1. Updates the Supplier so the user can drag variables into terms.
 *   2. Auto-adds main effects for newly added variables to the selected block.
 *   3. Removes terms whose source variable was removed from all blocks.
 *
 * This mirrors jmv's regression model-builder pattern: slot changes update the
 * Supplier, diff the current moderator list against the previous one stored in
 * the View workspace, then remove or auto-add terms in metaRegBlocks.
 *
 * Uses the `utils` and `FormatDef` globals that jamovi injects into every
 * module's JS context (both jus 2.0 and 3.0).
 *
 * @param {Object} ui      - The UI controls object.
 * @param {Object} context - The View instance (`this` in handlers).
 */
const updateModelTerms = function (ui, context) {
  // 1. Refresh Supplier items and reuse the same list for the block diff.
  const allVars = updateModelSupplier(ui);

  // 2. Diff against previous state to detect added / removed variables.
  const diff = context.findChanges(
    "metaRegModelSupplier_vars",
    allVars,
    true,
    FormatDef.variable,
  );

  let blocks = utils.clone(ui.metaRegBlocks.value(), []);
  let changed = false;

  // 3. Remove terms that contain any removed variable from all blocks.
  for (let i = 0; i < diff.removed.length; i++) {
    for (let b = 0; b < blocks.length; b++) {
      if (blocks[b] === null) blocks[b] = [];
      for (let j = blocks[b].length - 1; j >= 0; j--) {
        if (FormatDef.term.contains(blocks[b][j], diff.removed[i])) {
          blocks[b].splice(j, 1);
          changed = true;
        }
      }
    }
  }

  // 4. Auto-add main effects for newly added variables to the selected block.
  //    Pattern from jmv linreg.events.js lines 152-159.
  let selectedRows = ui.metaRegBlocks.getSelectedRowIndices();
  if (selectedRows.length > 0) {
    let targetBlock = selectedRows[selectedRows.length - 1];
    if (blocks[targetBlock] === null) blocks[targetBlock] = [];
    for (let i = 0; i < diff.added.length; i++) {
      blocks[targetBlock].push([diff.added[i]]);
    }
    changed = changed || diff.added.length > 0;
  }

  // 5. Sort terms within each block by length: main effects before interactions.
  for (let b = 0; b < blocks.length; b++) {
    if (blocks[b] && utils.sortArraysByLength(blocks[b])) {
      changed = true;
    }
  }

  if (changed) {
    ui.metaRegBlocks.setValue(blocks);
  }
};

/**
 * Relabel block headers: "Model 1", "Model 2", etc.
 *
 * @param {Object} list - The blocks ListBox control.
 */
const updateModelLabels = function (list) {
  list.applyToItems(0, function (item, index) {
    item.controls[0].setPropertyValue("label", "Model " + (index + 1));
  });
};

/**
 * Replace null blocks with empty arrays.
 *
 * When a new block is added, it starts as null. This converts it to []
 * so downstream code doesn't crash.
 *
 * @param {Object} ui      - The UI controls object.
 * @param {Object} context - The View instance.
 */
const checkForNullBlocks = function (ui, context) {
  let changed = false;
  let blocks = utils.clone(ui.metaRegBlocks.value(), []);
  for (let b = 0; b < blocks.length; b++) {
    if (blocks[b] === null) {
      changed = true;
      blocks[b] = [];
    }
  }
  if (changed) {
    ui.metaRegBlocks.setValue(blocks);
  }
};

/**
 * Sort terms within each block after manual drag/drop inside a block ListBox.
 *
 * @param {Object} ui - The UI controls object.
 */
const enforceBlockTermOrder = function (ui) {
  let blocks = utils.clone(ui.metaRegBlocks.value(), []);
  let changed = false;
  for (let b = 0; b < blocks.length; b++) {
    if (blocks[b] && utils.sortArraysByLength(blocks[b])) {
      changed = true;
    }
  }
  if (changed) {
    ui.metaRegBlocks.setValue(blocks);
  }
};

/**
 * Enable or disable the meta-regression Supplier and blocks ListBox.
 *
 * Enabled when at least one covariate or factor is assigned.
 * Toggles the disabled-list CSS class directly since ListBox enable
 * is commented out in jamovi core (optionlistcontrol.ts).
 *
 * @param {Object} ui - The UI controls object.
 */
const updateEnableState = function (ui) {
  const hasVars =
    (ui.metaRegCovs.value() || []).length > 0 ||
    (ui.metaRegFactors.value() || []).length > 0;
  const method = hasVars ? "remove" : "add";
  ui.metaRegModelSupplier.el.classList[method]("disabled-list");
  ui.metaRegBlocks.el.classList[method]("disabled-list");
};

module.exports = {
  updateModelSupplier,
  updateModelTerms,
  updateModelLabels,
  checkForNullBlocks,
  enforceBlockTermOrder,
  updateEnableState,
};
