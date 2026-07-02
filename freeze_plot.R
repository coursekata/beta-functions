# freeze_plot.R
#
# Shared helper for gf_lm.R, gf_lm_cat.R, and gf_coef.R.
#
# When these functions are packaged into coursekata-r, replace the embedded
# copy of .freeze_plot_xy in each file with a single source() of this file
# (or an internal package function).
#
# Until then each file is self-contained and carries its own copy so a single
# source() call is all a user needs.

# Evaluate the y and x mappings of a plot ONCE and lock the results into
# hidden columns (.gf_y / .gf_x) in p$data and in each raw-data layer.
#
# Why this is needed:
#   ggformula formulas like shuffle(y) ~ x are re-evaluated at render time by
#   ggplot2. That means the jitter dots and any downstream stat (regression
#   line, group-mean segments, gf_coef arrows) could all see DIFFERENT
#   shuffles of y. Freezing evaluates shuffle() exactly once and stores the
#   result so every layer uses the same permutation.
#
# What it does:
#   - Evaluates p$mapping$y and p$mapping$x against p$data
#   - Stores results in p$data$.gf_y and p$data$.gf_x
#   - For every StatIdentity layer that still holds the original data frame,
#     also stores the values in the layer's own data and updates its y/x
#     mapping to point to .gf_y / .gf_x
#   - Restores p$labels$y / p$labels$x from the original mapping expressions
#     so axis titles stay as "shuffle(later_anxiety)" rather than ".gf_y"
#   - Is a no-op if the plot is already frozen (.gf_y already in p$data)
#
# Only StatIdentity layers are updated (geom_point, geom_jitter). Layers that
# use a computed stat (StatSmooth, StatLm, gf_model) receive data that has
# already been transformed, so they never have .gf_y — those are skipped.

.freeze_plot_xy <- function(p) {
  if (".gf_y" %in% names(p$data)) return(p)

  orig_data <- p$data

  .label_from <- function(quo) tryCatch(rlang::as_label(quo), error = function(e) NULL)
  orig_y_label <- .label_from(p$mapping$y) %||%
    if (length(p$layers)) .label_from(p$layers[[1]]$mapping$y)
  orig_x_label <- .label_from(p$mapping$x) %||%
    if (length(p$layers)) .label_from(p$layers[[1]]$mapping$x)

  y_vals <- rlang::eval_tidy(p$mapping$y, data = orig_data)
  x_vals <- rlang::eval_tidy(p$mapping$x, data = orig_data)

  frozen_y <- rlang::new_quosure(quote(.gf_y), rlang::base_env())
  frozen_x <- rlang::new_quosure(quote(.gf_x), rlang::base_env())

  p$data[[".gf_y"]] <- y_vals
  p$data[[".gf_x"]] <- x_vals

  for (i in seq_along(p$layers)) {
    ld <- p$layers[[i]]$data
    is_identity_stat <- inherits(p$layers[[i]]$stat, "StatIdentity")
    if (is_identity_stat && is.data.frame(ld) && identical(ld, orig_data)) {
      p$layers[[i]]$data[[".gf_y"]] <- y_vals
      p$layers[[i]]$data[[".gf_x"]] <- x_vals
      lm <- p$layers[[i]]$mapping
      if (!is.null(lm[["y"]])) p$layers[[i]]$mapping[["y"]] <- frozen_y
      if (!is.null(lm[["x"]])) p$layers[[i]]$mapping[["x"]] <- frozen_x
    }
  }

  if (!is.null(orig_y_label)) p$labels$y <- orig_y_label
  if (!is.null(orig_x_label)) p$labels$x <- orig_x_label

  p
}
