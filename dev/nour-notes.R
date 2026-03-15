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