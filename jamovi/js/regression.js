/**
 * Shared model builder logic for meta-regression.
 *
 * Keeps the metaRegModelSupplier and metaRegTerms controls in sync
 * with the metaRegCovs and metaRegFactors variable slots.
 *
 * When the user adds or removes a variable, this function:
 *   1. Updates the Supplier so the user can drag variables into terms
 *   2. Auto-adds main effects for newly added variables
 *   3. Removes terms whose source variable was removed
 *
 * Uses the `utils` and `FormatDef` globals that jamovi injects into
 * every module's JS context (both jus 2.0 and 3.0).
 *
 * @param {Object} ui      - The UI controls object.
 * @param {Object} context - The View instance (`this` in handlers).
 */
const updateModelTerms = function (ui, context) {
  // 1. Combine both variable slots into one list
  const covsList = utils.clone(ui.metaRegCovs.value(), []);
  const factorsList = utils.clone(ui.metaRegFactors.value(), []);
  const allVars = covsList.concat(factorsList);

  // 2. Feed the combined list to the Supplier (creates proper
  //    FormattedValue items that the Supplier knows how to render)
  ui.metaRegModelSupplier.setValue(
    utils.valuesToItems(allVars, FormatDef.variable),
  );

  // 3. Diff against previous state to detect added / removed variables
  const diff = context.findChanges(
    "metaRegModelSupplier_vars",
    allVars,
    true,
    FormatDef.variable,
  );

  const terms = utils.clone(ui.metaRegTerms.value(), []);
  let changed = false;

  // 4. Remove terms that contain any removed variable
  for (let i = 0; i < diff.removed.length; i++) {
    for (let j = terms.length - 1; j >= 0; j--) {
      if (FormatDef.term.contains(terms[j], diff.removed[i])) {
        terms.splice(j, 1);
        changed = true;
      }
    }
  }

  // 5. Auto-add main effects for newly added variables
  for (let i = 0; i < diff.added.length; i++) {
    const newTerm = [diff.added[i]];
    if (!utils.listContains(terms, newTerm, FormatDef.term)) {
      terms.push(newTerm);
      changed = true;
    }
  }

  // 6. Sort terms by length (main effects before interactions)
  if (utils.sortArraysByLength(terms)) {
    changed = true;
  }

  if (changed) {
    ui.metaRegTerms.setValue(terms);
  }
};

/**
 * Re-sort terms after manual drag/drop inside the ListBox.
 *
 * Without this, the user can drag an interaction (e.g. ["age","sex"])
 * above its constituent main effects — violating the hierarchy principle.
 * Call this from the terms ListBox's `_changed` handler.
 *
 * @param {Object} ui - The UI controls object.
 */
const enforceTermOrder = function (ui) {
  const terms = utils.clone(ui.metaRegTerms.value(), []);
  if (utils.sortArraysByLength(terms)) {
    ui.metaRegTerms.setValue(terms);
  }
};

module.exports = { updateModelTerms, enforceTermOrder };
