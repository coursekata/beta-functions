# deps: ggplot2, rlang
#
# Overlay a categorical linear model on an existing ggformula / ggplot2 plot.
# Works like gf_lm() but for factor/character x variables: draws a horizontal
# segment at the group mean (fitted value) for each level, spanning the group's
# x position.
#
# Because it reads the y and x mappings off the plot object (just like gf_lm()),
# it works transparently with in-formula transformations including shuffle():
#
#   gf_jitter(shuffle(later_anxiety) ~ condition, data = er, width = .1) %>%
#     gf_lm_cat()
#
# Parameters:
#   p         - existing ggplot/ggformula plot
#   width     - total width of each group segment (default 0.4)
#   color     - segment color
#   linewidth - segment linewidth
#   ...       - additional arguments passed to geom_segment

gf_lm_cat <- function(p, ..., width = 0.4, color = "steelblue", linewidth = 1) {

  # Freeze the plot on first touch: evaluate y and x once and lock the values
  # into hidden columns (.gf_y / .gf_x) in p$data, then update the plot and
  # layer mappings to reference those columns instead of the original expressions.
  # This means gf_lm_cat, gf_coef, and the final display all see the SAME
  # shuffle / transformation — not independent re-evaluations of shuffle().
  p <- .freeze_plot_xy(p)

  y_vals <- p$data[[".gf_y"]]
  x_raw  <- p$data[[".gf_x"]]

  if (is.numeric(x_raw)) {
    stop(
      "gf_lm_cat() requires a categorical (factor or character) x variable. ",
      "For continuous x, use gf_lm()."
    )
  }

  if (!is.factor(x_raw)) x_raw <- factor(x_raw)
  lvls <- levels(x_raw)

  group_means <- tapply(y_vals, x_raw, mean, na.rm = TRUE)

  seg_data <- data.frame(
    x    = seq_along(lvls) - width / 2,
    xend = seq_along(lvls) + width / 2,
    y    = as.numeric(group_means[lvls]),
    yend = as.numeric(group_means[lvls])
  )

  p + ggplot2::geom_segment(
    data    = seg_data,
    mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color     = color,
    linewidth = linewidth,
    ...
  )
}


# ── shared helpers ────────────────────────────────────────────────────────────

# Evaluate y and x expressions ONCE and lock them into hidden columns
# (.gf_y / .gf_x) in p$data AND in each layer's own data frame, then replace
# every mapping that referenced y or x with symbols pointing to those columns.
#
# Why both p$data AND layer data:
#   ggformula passes data explicitly to each layer (layer$data is a data frame,
#   not NULL), so ggplot2 uses layer$data — not p$data — when rendering.
#   We must update both to ensure the frozen columns are visible at render time.
#
# Why new_quosure(..., base_env()) instead of quo():
#   quo() captures the local function environment, which no longer exists after
#   the function returns. new_quosure with base_env() creates a stable quosure;
#   eval_tidy checks the data mask first anyway, where .gf_y / .gf_x exist.
#
# No-op if the plot is already frozen.
.freeze_plot_xy <- function(p) {
  if (".gf_y" %in% names(p$data)) return(p)

  orig_data <- p$data

  y_vals <- rlang::eval_tidy(p$mapping$y, data = orig_data)
  x_vals <- rlang::eval_tidy(p$mapping$x, data = orig_data)

  frozen_y <- rlang::new_quosure(quote(.gf_y), rlang::base_env())
  frozen_x <- rlang::new_quosure(quote(.gf_x), rlang::base_env())

  p$data[[".gf_y"]] <- y_vals
  p$data[[".gf_x"]] <- x_vals

  for (i in seq_along(p$layers)) {
    ld  <- p$layers[[i]]$data
    is_identity_stat <- inherits(p$layers[[i]]$stat, "StatIdentity")
    if (is_identity_stat && is.data.frame(ld) && identical(ld, orig_data)) {
      p$layers[[i]]$data[[".gf_y"]] <- y_vals
      p$layers[[i]]$data[[".gf_x"]] <- x_vals
      lm <- p$layers[[i]]$mapping
      if (!is.null(lm[["y"]])) p$layers[[i]]$mapping[["y"]] <- frozen_y
      if (!is.null(lm[["x"]])) p$layers[[i]]$mapping[["x"]] <- frozen_x
    }
  }

  p
}
