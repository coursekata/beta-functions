# freeze_plot.R
#
# Shared helpers:
#   .freeze_plot_xy — gf_lm.R, gf_lm_cat.R, gf_coef.R
#   .freeze_jitter  — gf_resid_gf_squaresid.R, gf_reduce.R
#
# When these functions are packaged into coursekata-r, replace the embedded
# copy in each file with a single source() of this file (or an internal
# package function).
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

# Pin every unseeded jitter layer to a fixed seed so all builds of the plot
# draw identical dot positions.
#
# Why this is needed:
#   position_jitter() defaults to seed = NA, which re-rolls the jitter on
#   every ggplot_build(). Functions that anchor overlays to the jittered
#   positions (gf_resid, gf_square_resid, gf_reduce, gf_square_reduce) used
#   to bracket their build with sample()/set.seed() so the *next* render
#   would redraw the same jitter — but that bracket breaks under lazy pipe
#   evaluation, is last-writer-wins when several such functions are chained,
#   and only survives a single render. Failing chains and diagnosis:
#   tests/test_resid_square_alignment.ipynb
#
# What it does:
#   - Assigns a random fixed seed to each PositionJitter layer that has none.
#     ggproto objects are environments, so the assignment sticks to the layer
#     itself and every subsequent build reproduces the same jitter — whichever
#     function builds it, however many times.
#   - No-op for layers that already have a seed (user-set or frozen by an
#     earlier call in the chain) and for non-jitter plots (e.g. gf_point).
#   - Never calls set.seed(), so the user's RNG stream is not reset.
#
# Note: the seed is assigned in place, so a base plot saved in a variable is
# frozen too — everything later built from it stays mutually consistent.

.freeze_jitter <- function(plot) {
  for (l in plot$layers) {
    pos <- l$position
    if (inherits(pos, "PositionJitter") && !isTRUE(is.finite(pos$seed))) {
      pos$seed <- sample.int(.Machine$integer.max, 1L)
    }
  }
  plot
}
