# deps: ggplot2, rlang
#
# Extended prototype of gf_sd_ruler() — adds histogram support.
# The coursekata-r version only handles scatter/jitter (y-axis outcome).
# This version auto-detects axis orientation:
#
#   Outcome on y (scatter, jitter) → vertical segment at (x_placement, mean) to (x_placement, mean+sd)
#   Outcome on x (histogram)       → horizontal segment at (mean, 0) to (mean+sd, 0)
#
# Usage:
#   # scatter (existing behavior)
#   gf_point(Thumb ~ Height, data = Fingers) %>%
#     gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#     gf_sd_ruler()
#
#   # histogram (new)
#   gf_histogram(~ Thumb, data = Fingers) %>%
#     gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#     gf_sd_ruler()

gf_sd_ruler <- function(p, y = NULL, data = NULL, x = NULL,
                        where = c("middle", "mean", "median"),
                        color = "red", size = 0.8, ...) {
  where <- match.arg(where)
  if (is.null(data)) data <- p$data

  # ── Infer which axis the outcome is on ──────────────────────────────────────
  has_y_mapping <- !is.null(p$mapping$y)
  has_x_mapping <- !is.null(p$mapping$x)

  # Explicit y arg → scatter mode; explicit x → histogram mode;
  # otherwise infer from which aesthetic is present.
  if (!is.null(y)) {
    mode <- "scatter"
    y_name <- if (is.character(y)) y else deparse(substitute(y))
  } else if (!is.null(x)) {
    mode <- "histogram"
    x_name <- if (is.character(x)) x else deparse(substitute(x))
  } else if (has_y_mapping) {
    mode <- "scatter"
    y_name <- rlang::as_name(p$mapping$y)
  } else if (has_x_mapping) {
    mode <- "histogram"
    x_name <- rlang::as_name(p$mapping$x)
  } else {
    stop("gf_sd_ruler: cannot infer outcome variable — pass y or x explicitly.")
  }

  # ── Scatter mode: vertical segment ──────────────────────────────────────────
  if (mode == "scatter") {
    y_vals <- data[[y_name]]
    m <- mean(y_vals, na.rm = TRUE)
    s <- sd(y_vals, na.rm = TRUE)

    # x placement
    if (has_x_mapping) {
      x_raw <- data[[rlang::as_name(p$mapping$x)]]
      x_num <- if (is.numeric(x_raw)) x_raw else as.numeric(factor(x_raw))
    } else {
      x_num <- seq_along(y_vals)
    }

    x0 <- switch(where,
      middle = (min(x_num, na.rm = TRUE) + max(x_num, na.rm = TRUE)) / 2,
      mean   = mean(x_num, na.rm = TRUE),
      median = stats::median(x_num, na.rm = TRUE)
    )

    seg <- data.frame(x = x0, xend = x0, y = m, yend = m + s)
    p + ggplot2::geom_segment(
      data        = seg,
      mapping     = ggplot2::aes(x = .data$x, xend = .data$xend,
                                 y = .data$y,  yend = .data$yend),
      inherit.aes = FALSE,
      color       = color,
      linewidth   = size,
      ...
    )

  # ── Histogram mode: horizontal segment ──────────────────────────────────────
  } else {
    x_vals <- data[[x_name]]
    m <- mean(x_vals, na.rm = TRUE)
    s <- sd(x_vals, na.rm = TRUE)

    seg <- data.frame(x = m, xend = m + s, y = 0, yend = 0)
    p + ggplot2::geom_segment(
      data        = seg,
      mapping     = ggplot2::aes(x = .data$x, xend = .data$xend,
                                 y = .data$y,  yend = .data$yend),
      inherit.aes = FALSE,
      color       = color,
      linewidth   = size,
      ...
    )
  }
}
