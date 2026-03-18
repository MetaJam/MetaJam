# Notes:
#
# `settings.meta()` for "BMJ" had purple color and other layout settings not
# implemented when we choose `layout = "BMJ"` in the forest plot. I need to open
# issue with this in meta package
#
# `settings.meta()` supposed to change layout and other things including
# rounding, some statistical preferences, etc and some settings not change
# layout, I need to think if we should implement them or not I think no for now
#
# Related to the first point the `colgap.forest` should be 5mm for BMJ but it is
# not applied when we set it at the layout argument level not settings.meta
# level so I deleted auto setting for colgap.forest for now, and hardcoded them
# with the default which is 2mm
#
# `xlim` argument intenally could be symmetric or NUll, right now we do not
# differentiate between them, we use auto to let the function decide


# isFilled() removal from initForestPlot ----------------------------------
#
# We removed isFilled() from initForestPlot for two reasons:
#
# 1. It is always FALSE during .init() — isFilled() checks filePath which
#    is set to NULL at create(). Only .load() (which runs AFTER .init())
#    can restore it. So this guard is dead code in .init().
#
# 2. Image dimensions set by setSize() are NOT saved/restored across
#    requests. fromProtoBuf() reads the CURRENT dimensions (set by .init())
#    and compares them against saved ones to detect sizeChanged. If we
#    skipped setSize() (via isFilled), the image would have default 400×300,
#    causing false sizeChanged detection and re-render at wrong dimensions.
#    So setSize() in .init() is mandatory on every request — we cannot skip
#    it, and therefore cannot skip the model computation that feeds it.


# Model caching via state -------------------------------------------------
#
# With the current guards (hasRequiredVars in .run(), !visible in
# initMainText, initForestPlot, populateMainText, populateSubgroupText),
# the model is never computed when ALL outputs are hidden. R's lazy
# evaluation ensures self$model promises are never forced.
#
# Caching the model in state (via setState in .run(), read in active binding)
# would NOT help when a forest plot is visible, because .init() accesses the
# model for calcForestHeight() BEFORE .load() restores state.
#
# The only cases where state caching could avoid model recomputation:
#   1. Forest plot unchecked but text visible — .init() never accesses the
#      model (!visible guard exits), .run() could read the cached model
#      from state instead of recomputing via the active binding.
#   2. Sub-analyses without forest plots (e.g., publication bias using base R
#      plots) — no .init() sizing needed, so changing their options could
#      reuse the cached model instead of recomputing.
#
# These are narrow conditions but a viable future optimization path.
