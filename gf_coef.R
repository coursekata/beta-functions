# deps: ggplot2, rlang
#
# Overlay b0 (intercept) and b1, b2, ... coefficients from an lm() onto a
# ggformula / ggplot2 plot.
#
# - b0 is drawn as a light horizontal line across the plot.
# - For CATEGORICAL X:
#     Each b_k is drawn as an arrow from b0 to (b0 + b_k) near group k+1,
#     labeled "b[k]".
# - For CONTINUOUS X:
#     b1 is drawn as a rise-over-run right-angle annotation: a horizontal
#     segment (the run) and a vertical arrow (the rise), labeled "b[1]".
#     The run unit is auto-selected as a "nice" number near 10% of the x span,
#     and the actual run distance is printed below the horizontal segment.
#
# gf_b() is an alias for gf_coef().
#
# Usage (pipe-friendly):
#   gf_jitter(later_anxiety ~ condition, data = er) %>%
#     gf_coef(lm(later_anxiety ~ condition, data = er))
#
#   gf_point(Thumb ~ Height, data = Fingers) %>%
#     gf_coef(lm(Thumb ~ Height, data = Fingers))
#
# Parameters:
#   p               - existing ggplot/ggformula plot
#   model           - fitted lm object
#   color           - color for arrows, lines, and dots (default: CourseKata
#                     purple-500 "#c4aeee"). Does NOT affect text labels.
#   label_color     - color for all text labels (default "black")
#   b0_color        - (continuous) color for the b0 hollow dot; defaults to
#                     `color` so it matches the rest of the annotation
#   b0_alpha        - (categorical) transparency of the b0 horizontal line
#   b0_linewidth    - (categorical) linewidth of the b0 horizontal line
#   b0_size         - (continuous) size of the hollow b0 dot at (0, b0)
#   arrow_linewidth - linewidth of b1/b2/... arrows
#   label_size      - font size for coefficient labels
#   show_b0_label   - whether to annotate b0 (line + label for categorical;
#                     hollow dot + label at x=0 for continuous)
#   arrow_nudge     - (categorical) how far LEFT of each group to place arrow
#   label_nudge     - (categorical) extra leftward offset for the text label
#   run             - (continuous) override the auto-selected run unit
#   run_x           - (continuous) override the x position of the annotation

gf_coef <- function(p, model = NULL,
                    color = "#b599ed",
                    label_color = "black",
                    b0_color = NULL,
                    b0_alpha = 0.3,
                    b0_linewidth = 0.8,
                    b0_size = 4,
                    arrow_linewidth = 0.5,
                    label_size = 3.5,
                    show_b0_label = TRUE,
                    arrow_nudge = 0.18,
                    label_nudge = 0.08,
                    run = NULL,
                    run_x = NULL,
                    ...) {

  if (is.null(b0_color)) b0_color <- color

  # If no model supplied, freeze the plot and build a model from the frozen data.
  # Freezing locks shuffle() and other random expressions into hidden columns
  # so the arrows and the jitter dots all reflect the same shuffle.
  if (is.null(model)) {
    p     <- .freeze_plot_xy(p)
    model <- .model_from_plot(p)
  }

  coefs  <- coef(model)
  b0_val <- coefs[[1]]

  # Detect categorical vs continuous from the model's data matrix
  model_terms <- attr(terms(model), "term.labels")
  is_cat <- if (length(model_terms) == 0) {
    FALSE
  } else {
    x_var  <- model_terms[1]
    x_vals <- model$model[[x_var]]
    is.factor(x_vals) || is.character(x_vals)
  }

  # b0 horizontal line: drawn for categorical models (it marks the reference
  # group mean) and intercept-only models. Omitted for continuous X because
  # b0 is shown as a hollow dot at x=0 instead.
  out <- p
  if (is_cat || length(model_terms) == 0) {
    out <- out + ggplot2::geom_hline(
      yintercept = b0_val,
      color = color, alpha = b0_alpha, linewidth = b0_linewidth
    )
  }

  # Intercept-only model: just the line + optional label, then return
  if (length(model_terms) == 0) {
    if (show_b0_label) {
      x_rng <- .x_range_from_plot(p, model)
      out <- out + ggplot2::annotate(
        "label",
        x = x_rng[1], y = b0_val,
        label = "b[0]", parse = TRUE,
        color = label_color, size = label_size, hjust = 0, vjust = 0.5,
        fill = "white", border.color = NA
      )
    }
    return(out)
  }

  if (is_cat) {
    out <- .add_cat_coefs(out, coefs, b0_val, is_cat, x_vals,
                          color, label_color, arrow_linewidth, label_size,
                          show_b0_label, arrow_nudge, label_nudge)
  } else {
    out <- .add_cont_coef(out, coefs, b0_val, x_vals,
                          color, label_color, b0_color, b0_size,
                          arrow_linewidth, label_size,
                          show_b0_label, run, run_x, p, model)
  }

  out
}

# Alias
gf_b <- gf_coef


# ── helpers ──────────────────────────────────────────────────────────────────

# .freeze_plot_xy is duplicated here so gf_coef.R is self-contained (single
# source() call). TODO (coursekata-r package): replace this block in gf_lm.R,
# gf_lm_cat.R, and gf_coef.R with a shared internal function from freeze_plot.R.

# Evaluate y and x expressions ONCE and lock them into hidden columns
# (.gf_y and .gf_x) in p$data, replacing the plot-level and layer-level
# mappings so that the final display and all downstream functions all use the
# same frozen values — not independent re-evaluations of shuffle().
# No-op if already frozen.
.freeze_plot_xy <- function(p) {
  if (".gf_y" %in% names(p$data)) return(p)

  # Snapshot the original data BEFORE adding any columns, so we can identify
  # which layers share the user's data vs. layers with computed data (gf_model,
  # gf_lm, etc. produce layers whose data frames are NOT identical to p$data).
  orig_data <- p$data

  # Capture axis label strings now so we can restore them after freezing.
  # ggplot2 derives axis titles from mapping expressions; replacing them with
  # .gf_y/.gf_x would show those internal names instead of the original formula
  # terms (e.g. "shuffle(later_anxiety)" → ".gf_y"). Try plot-level mapping
  # first; fall back to first layer if the plot mapping is NULL.
  .label_from <- function(quo) tryCatch(rlang::as_label(quo), error = function(e) NULL)
  orig_y_label <- .label_from(p$mapping$y) %||%
    if (length(p$layers)) .label_from(p$layers[[1]]$mapping$y)
  orig_x_label <- .label_from(p$mapping$x) %||%
    if (length(p$layers)) .label_from(p$layers[[1]]$mapping$x)

  y_vals <- rlang::eval_tidy(p$mapping$y, data = orig_data)
  x_vals <- rlang::eval_tidy(p$mapping$x, data = orig_data)

  frozen_y <- rlang::new_quosure(quote(.gf_y), rlang::base_env())
  frozen_x <- rlang::new_quosure(quote(.gf_x), rlang::base_env())

  # Store frozen values in p$data so .model_from_plot() and gf_lm_cat() can
  # read them directly. Do NOT update p$mapping — model layers (gf_model, gf_lm)
  # inherit from p$mapping, and pointing it to .gf_y would make them fail with
  # "object not found" when their stat-computed data lacks that column.
  p$data[[".gf_y"]] <- y_vals
  p$data[[".gf_x"]] <- x_vals

  for (i in seq_along(p$layers)) {
    ld  <- p$layers[[i]]$data
    # Only freeze raw-data layers: StatIdentity (geom_point, geom_jitter) passes
    # data through unchanged, so .gf_y will still be there at render time.
    # Non-identity stats (StatSmooth, StatModel) replace data with computed frames
    # that won't have .gf_y — skip those entirely.
    is_identity_stat <- inherits(p$layers[[i]]$stat, "StatIdentity")
    if (is_identity_stat && is.data.frame(ld) && identical(ld, orig_data)) {
      p$layers[[i]]$data[[".gf_y"]] <- y_vals
      p$layers[[i]]$data[[".gf_x"]] <- x_vals
      lm <- p$layers[[i]]$mapping
      if (!is.null(lm[["y"]])) p$layers[[i]]$mapping[["y"]] <- frozen_y
      if (!is.null(lm[["x"]])) p$layers[[i]]$mapping[["x"]] <- frozen_x
    }
  }

  # Restore labels so .gf_y / .gf_x never appear in axis titles
  if (!is.null(orig_y_label)) p$labels$y <- orig_y_label
  if (!is.null(orig_x_label)) p$labels$x <- orig_x_label

  p
}

# Build an lm from the frozen plot data.
.model_from_plot <- function(p) {
  p      <- .freeze_plot_xy(p)
  y_vals <- p$data[[".gf_y"]]
  x_raw  <- p$data[[".gf_x"]]

  if (!is.numeric(x_raw) && !is.factor(x_raw)) x_raw <- factor(x_raw)
  lm(y ~ x, data = data.frame(y = y_vals, x = x_raw))
}

.add_cat_coefs <- function(out, coefs, b0_val, is_cat, x_vals,
                           color, label_color, arrow_linewidth, label_size,
                           show_b0_label, arrow_nudge, label_nudge) {

  if (!is.factor(x_vals)) x_vals <- factor(x_vals)
  lvls   <- levels(x_vals)

  # b0 label: left of group 1
  if (show_b0_label) {
    out <- out + ggplot2::annotate(
      "label",
      x = 1 - arrow_nudge - label_nudge, y = b0_val,
      label = "b[0]", parse = TRUE,
      color = label_color, size = label_size, hjust = 1, vjust = 0.5,
      fill = "white", border.color = NA
    )
  }

  # One arrow per non-reference coefficient
  if (length(coefs) < 2) return(out)

  for (i in seq(2, length(coefs))) {
    bi        <- coefs[[i]]
    group_pos <- i
    x_arr     <- group_pos - arrow_nudge
    x_lbl     <- x_arr - label_nudge
    y_top     <- b0_val + bi
    y_bot     <- b0_val

    out <- out +
      ggplot2::geom_segment(
        data = data.frame(x = x_arr, xend = x_arr, y = y_top, yend = y_bot),
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        inherit.aes = FALSE,
        color = color, linewidth = arrow_linewidth,
        arrow = ggplot2::arrow(
          ends = "first", type = "open",
          length = ggplot2::unit(0.15, "cm")
        )
      ) +
      ggplot2::annotate(
        "label",
        x = x_lbl, y = (y_top + y_bot) / 2,
        label = paste0("b[", i - 1, "]"), parse = TRUE,
        color = label_color, size = label_size, hjust = 1,
        fill = "white", border.color = NA
      )
  }

  out
}


.add_cont_coef <- function(out, coefs, b0_val, x_vals,
                           color, label_color, b0_color, b0_size,
                           arrow_linewidth, label_size,
                           show_b0_label, run, run_x, p, model) {

  if (length(coefs) < 2) return(out)
  b1_val <- coefs[[2]]

  # Use the effective display range, not just the data range. When show_b0_label
  # is TRUE, expand_limits(x = 0) widens the axis to include 0. Computing run
  # and run_x from the narrower data range would make the annotation look tiny
  # on the expanded axis.
  x_data_range    <- range(x_vals, na.rm = TRUE)
  x_display_range <- if (show_b0_label) range(c(x_data_range, 0)) else x_data_range
  x_span          <- diff(x_display_range)
  x_lo            <- x_display_range[1]

  # Auto-select a nice run unit near 10% of the x span
  if (is.null(run)) {
    run <- .nice_run(x_span)
  }

  # Position the annotation. When b0 is visible at x=0, choose between a
  # left-third and right-third candidate and pick whichever places the
  # annotation center farther from x=0, so the two labels don't crowd each
  # other regardless of where 0 falls on the axis.
  if (is.null(run_x)) {
    if (show_b0_label) {
      run_x_left  <- x_lo + 0.15 * x_span
      run_x_right <- x_lo + 0.60 * x_span
      if (abs(run_x_left + run / 2) >= abs(run_x_right + run / 2)) {
        run_x <- run_x_left
      } else {
        run_x <- run_x_right
      }
    } else {
      run_x <- x_lo + 0.15 * x_span
    }
  }

  # y values at the left and right edges of the run
  y_at_run_x       <- b0_val + b1_val * run_x
  y_at_run_x_plus  <- b0_val + b1_val * (run_x + run)
  rise              <- y_at_run_x_plus - y_at_run_x   # signed

  # Layout: rise arrow is on the LEFT (vertical), run segment is at the TIP of
  # the arrow (top if positive slope, bottom if negative slope).
  y_base <- y_at_run_x
  y_tip  <- y_at_run_x_plus
  y_mid  <- (y_base + y_tip) / 2

  # Vertical "rise" arrow
  out <- out + ggplot2::geom_segment(
    data = data.frame(x = run_x, xend = run_x, y = y_base, yend = y_tip),
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = color, linewidth = arrow_linewidth,
    arrow = ggplot2::arrow(
      ends = "last", type = "open",
      length = ggplot2::unit(0.15, "cm")
    )
  )

  # Horizontal "run" segment at the tip of the arrow
  out <- out + ggplot2::geom_segment(
    data = data.frame(x = run_x, xend = run_x + run, y = y_tip, yend = y_tip),
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = color, linewidth = arrow_linewidth
  )

  # Rise label: "b[1]" when run == 1, otherwise "run * b[1]" (e.g. "10 * b[1]")
  rise_label <- if (run == 1) {
    "b[1]"
  } else {
    paste0(.format_run(run), " %*% b[1]")
  }
  out <- out + ggplot2::annotate(
    "label",
    x = run_x - 0.015 * x_span, y = y_mid,
    label = rise_label, parse = TRUE,
    color = label_color, size = label_size, hjust = 1,
    fill = "white", border.color = NA
  )

  # Run-distance label below (or above) the horizontal run segment
  run_label <- .format_run(run)
  vjust_run <- if (rise >= 0) -0.4 else 1.4
  out <- out + ggplot2::annotate(
    "label",
    x = run_x + run / 2, y = y_tip,
    label = run_label,
    color = label_color, size = label_size - 0.5, vjust = vjust_run,
    fill = "white", border.color = NA
  )

  # b0: hollow dot at (x=0, y=b0), expanding x scale so it's always visible.
  if (show_b0_label) {
    out <- out +
      ggplot2::expand_limits(x = 0) +
      ggplot2::annotate(
        "point",
        x = 0, y = b0_val,
        color = b0_color, size = b0_size, shape = 1, stroke = 1.2
      ) +
      ggplot2::annotate(
        "label",
        x = 0, y = b0_val,
        label = "b[0]", parse = TRUE,
        color = label_color, size = label_size, hjust = -0.5, vjust = 0.5,
        fill = "white", border.color = NA
      )
  }

  out
}


# Pick a power of 10 (10^k) that falls in [5%, 80%] of x_span and is closest
# to 10% of x_span. Falls back to raw 10% only if no power of 10 qualifies.
.nice_run <- function(x_span) {
  # Generate a wide range of powers of 10
  ks         <- seq(-10, 10)
  candidates <- 10^ks
  # Keep only those in the [5%, 80%] window
  ok         <- candidates >= 0.05 * x_span & candidates <= 0.80 * x_span
  if (any(ok)) {
    valid  <- candidates[ok]
    target <- 0.10 * x_span
    valid[which.min(abs(valid - target))]
  } else {
    0.10 * x_span   # no power of 10 fits; use raw 10%
  }
}


# Format the run label cleanly (no trailing zeros, no scientific notation)
.format_run <- function(run) {
  if (run == round(run)) {
    as.character(as.integer(run))
  } else {
    # remove trailing zeros after decimal point
    sub("\\.?0+$", "", sprintf("%.10g", run))
  }
}


# Get x range from p$data or fall back to the model data
.x_range_from_plot <- function(p, model) {
  d <- if (!is.null(p$data) && nrow(p$data) > 0) p$data else model$model
  x_col <- tryCatch(rlang::as_name(p$mapping$x), error = function(e) NULL)
  if (!is.null(x_col) && x_col %in% names(d)) {
    x_vals <- d[[x_col]]
    if (is.numeric(x_vals)) return(range(x_vals, na.rm = TRUE))
    # categorical: positions 1..K
    return(c(1, length(unique(x_vals))))
  }
  c(0, 1)
}
