# deps: ggplot2, ggformula, rlang
#
# Extended gf_lm() that adds two capabilities on top of ggformula::gf_lm():
#
#   1. Categorical x support — when the x variable is a factor or character,
#      draws horizontal group-mean segments instead of a regression line
#      (the same output as gf_lm_cat()).
#
#   2. Shuffle-safe continuous x — when the y mapping contains shuffle() or
#      any other non-trivial expression, evaluates it once and locks the result
#      into a hidden column (.gf_y) before passing to StatLm. This ensures the
#      regression line and the jitter dots reflect the same permutation.
#
# When called standalone (object is a formula, not a plot), behaviour is
# identical to ggformula::gf_lm() — the new code only activates when piped
# onto an existing ggplot object.
#
# Usage:
#   source("gf_lm.R")        # shadows ggformula::gf_lm in the global env
#
#   # categorical x
#   gf_jitter(later_anxiety ~ condition, data = er, width = 0.1) %>% gf_lm()
#
#   # continuous x with shuffle
#   gf_jitter(shuffle(later_anxiety) ~ condition, data = er) %>% gf_lm()
#
#   # standalone — identical to ggformula::gf_lm()
#   gf_lm(Thumb ~ Height, data = Fingers)

gf_lm <- function(object = NULL, gformula = NULL, data = NULL, ...,
                  width = 0.4, color = "#663abe", linewidth = 1) {

  # ── Standalone use: forward everything to ggformula ────────────────────────
  # object is a formula or data frame, not a plot. Don't touch it.
  if (!inherits(object, c("gg", "ggplot"))) {
    return(ggformula::gf_lm(object = object, gformula = gformula,
                            data = data, ...))
  }

  # ── Piped use: object is an existing plot ──────────────────────────────────
  p <- object

  # Evaluate the y and x mappings once, locking shuffle() (and any other
  # random/computed expression) into hidden columns so all downstream layers
  # see the same values.
  p <- .freeze_plot_xy(p)

  x_raw <- p$data[[".gf_x"]]
  is_cat <- is.factor(x_raw) || is.character(x_raw)

  if (is_cat) {
    # ── Categorical x: delegate to gf_lm_cat ────────────────────────────────
    # p is already frozen, so gf_lm_cat's freeze call is a no-op.
    gf_lm_cat(p, width = width, color = color, linewidth = linewidth, ...)
  } else {
    # ── Continuous x: hand off to StatLm using frozen columns ────────────────
    # Replace the formula mapping with .gf_y ~ .gf_x so StatLm never
    # re-evaluates shuffle(). The frozen data is already in p$data.
    ggformula::gf_lm(p, gformula = .gf_y ~ .gf_x, data = p$data, ...)
  }
}


# ── .freeze_plot_xy ───────────────────────────────────────────────────────────
# Duplicated here so gf_lm.R is self-contained (single source() call).
# TODO (coursekata-r package): replace this block in gf_lm.R, gf_lm_cat.R,
# and gf_coef.R with a shared internal function from freeze_plot.R.

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
