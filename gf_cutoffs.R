# deps: ggplot2, rlang
#
# gf_cutoffs() — add cutoff markers to a histogram
#
# A cleaner, more explicit alternative to show_cutoffs(). Works with any
# distribution-part function (middle, tails, upper, lower, outer) passed
# directly as an argument, or auto-detects one from the histogram fill.
#
# Two usage modes:
#
#   # 1. Explicit — no fill needed; expression drives the cutoffs
#   gf_histogram(~ b1, data = sdob1) %>%
#     gf_cutoffs(middle(b1, .95))
#
#   # 2. Auto-detect — reads the fill aesthetic if no expression is given
#   gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
#     gf_cutoffs()
#
#   # 3. Stack multiple cutoff sets with different colors
#   gf_histogram(~ b1, data = sdob1) %>%
#     gf_cutoffs(middle(b1, .95), color = "blue") %>%
#     gf_cutoffs(middle(b1, .99), color = "red")
#
# Label logic: each cutoff marker labels the SMALLER region on its side.
# This means labels always describe the tail (the extreme/unusual region),
# regardless of which distribution-part function was used:
#   middle(.95)  → ".025 of values below" / ".025 of values above"
#   upper(.05)   → ".05 of values above"
#   lower(.05)   → ".05 of values below"
#   tails(.05)   → ".025 of values below" / ".025 of values above"


#' Add cutoff markers to a histogram
#'
#' Draws dashed vertical lines and downward-pointing triangle markers at the
#' empirical quantile cutoffs defined by a distribution-part function
#' (\code{middle()}, \code{tails()}, \code{upper()}, \code{lower()}, or
#' \code{outer()}).
#'
#' Pass the expression directly, or omit it and \code{gf_cutoffs()} will
#' auto-detect a distribution-part function from the histogram's fill aesthetic.
#'
#' @param p A ggplot histogram (from \code{gf_histogram()}).
#' @param expr A bare distribution-part call, e.g. \code{middle(b1, .95)}.
#'   If omitted, the fill aesthetic is inspected for a distribution-part function.
#' @param color Color for markers, lines, and labels. Default \code{"#555555"}
#'   (dark charcoal — unobtrusive against the histogram fill).
#' @param size Size of the downward-pointing triangle markers. Default \code{4}.
#' @param labels If \code{TRUE}, adds text annotations showing the tail proportion
#'   at each cutoff. The label always describes the smaller (tail) region on that
#'   side of the cutoff. Default \code{FALSE}.
#' @param ... Currently unused.
#' @return A ggplot object with cutoff markers added.
#' @export
gf_cutoffs <- function(p, expr = NULL, color = "#555555", size = 4,
                       labels = FALSE, ...) {
  expr_quo <- rlang::enquo(expr)

  # ── 1. Get x values from histogram ──────────────────────────────────────────
  if (is.null(p$mapping$x)) {
    stop("gf_cutoffs: plot must have an x aesthetic — use with gf_histogram()")
  }
  x_name <- rlang::as_name(p$mapping$x)
  if (!x_name %in% names(p$data)) {
    stop(paste0(
      "gf_cutoffs: variable '", x_name, "' not found in plot data.\n",
      "Make sure the variable name in your expression matches the histogram x variable."
    ))
  }
  x_vals <- p$data[[x_name]]

  # ── 2. Get logical vector from expression or fill ────────────────────────────
  if (!rlang::quo_is_null(expr_quo)) {
    logical_vec <- rlang::eval_tidy(expr_quo, data = p$data)
  } else {
    fill_quo <- .extract_dist_fill(p)
    if (is.null(fill_quo)) {
      stop(paste0(
        "gf_cutoffs: no expression given and no distribution-part fill found.\n",
        "Either pass an expression: gf_cutoffs(middle(", x_name, ", .95))\n",
        "Or set fill = ~middle(", x_name, ", .95) in gf_histogram()."
      ))
    }
    logical_vec <- rlang::eval_tidy(fill_quo, data = p$data)
  }

  if (!is.logical(logical_vec)) {
    stop("gf_cutoffs: expression must return a logical vector (TRUE/FALSE).")
  }

  # ── 3. Find cutoff positions from the logical vector ─────────────────────────
  cutoffs <- .find_cutoffs(x_vals, logical_vec)

  if (nrow(cutoffs) == 0) {
    stop("gf_cutoffs: no cutoff positions found.")
  }

  # ── 4. Compute y positions from built plot ───────────────────────────────────
  built   <- ggplot2::ggplot_build(p)
  y_range <- built$layout$panel_params[[1]]$y.range
  if (is.null(y_range)) y_range <- c(0, 30)

  arrow_y    <- -y_range[2] * 0.06
  line_top_y <- y_range[2] * 0.20
  line_bot_y <- arrow_y + y_range[2] * 0.015
  x_range    <- range(x_vals, na.rm = TRUE)
  x_span     <- diff(x_range)

  # ── 5. Draw markers ──────────────────────────────────────────────────────────
  for (i in seq_len(nrow(cutoffs))) {
    x0        <- cutoffs$x[i]
    tail_prop <- cutoffs$tail_prop[i]
    side      <- cutoffs$side[i]

    p <- p +
      ggplot2::annotate("segment",
        x = x0, xend = x0, y = line_bot_y, yend = line_top_y,
        linetype = "dashed", linewidth = 0.5, color = color
      ) +
      ggplot2::annotate("point",
        x = x0, y = arrow_y,
        shape = 25, size = size, fill = color, color = color
      )

    if (labels) {
      label_str <- .format_prop(tail_prop)

      if (side == "lower") {
        label_x    <- x_range[1] + x_span * 0.08
        label_y    <- y_range[2] * 0.65
        line_end_x <- label_x + x_span * 0.02
        line_end_y <- label_y - y_range[2] * 0.08
        label_text <- paste0(label_str, " of\nvalues below")
      } else {
        label_x    <- x_range[2] - x_span * 0.08
        label_y    <- y_range[2] * 0.65
        line_end_x <- label_x - x_span * 0.02
        line_end_y <- label_y - y_range[2] * 0.08
        label_text <- paste0(label_str, " of\nvalues above")
      }

      p <- p +
        ggplot2::annotate("segment",
          x = x0, xend = line_end_x, y = line_top_y, yend = line_end_y,
          linetype = "dashed", linewidth = 0.5, color = color
        ) +
        ggplot2::annotate("text",
          x = label_x, y = label_y, label = label_text,
          hjust = 0.5, vjust = 0.5, size = 3.2,
          color = color, fontface = "italic"
        )
    }
  }

  p + ggplot2::coord_cartesian(clip = "off")
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Extract a distribution-part quosure from the plot's fill aesthetic.
.extract_dist_fill <- function(p) {
  valid <- c("middle", "tails", "upper", "lower", "outer")

  is_dist_call <- function(quo) {
    if (is.null(quo)) return(FALSE)
    expr <- rlang::get_expr(quo)
    is.call(expr) && as.character(expr[[1]]) %in% valid
  }

  if (is_dist_call(p$mapping$fill))                       return(p$mapping$fill)
  if (length(p$layers) > 0 &&
      is_dist_call(p$layers[[1]]$mapping$fill))           return(p$layers[[1]]$mapping$fill)
  NULL
}

# Find cutoff x-positions and label metadata from a logical vector.
#
# Returns a data frame: x (cutoff position), tail_prop (proportion in the
# smaller/tail region on that side), side ("lower" or "upper").
#
# Label logic: at each TRUE/FALSE transition, label the SMALLER of the two
# sides. This ensures the label always describes the tail region, regardless
# of which distribution-part function was used:
#   middle(.95) F→T at i=25:  left=.025, right=.975 → "lower", .025
#   upper(.05)  F→T at i=950: left=.95,  right=.05  → "upper", .05
#   lower(.05)  T→F at i=50:  left=.05,  right=.95  → "lower", .05
#   tails(.05)  T→F at i=25:  left=.025, right=.975 → "lower", .025
.find_cutoffs <- function(x_vals, logical_vec) {
  keep      <- !is.na(x_vals) & !is.na(logical_vec)
  x_clean   <- x_vals[keep]
  log_clean <- logical_vec[keep]
  n         <- length(x_clean)

  ord      <- order(x_clean)
  x_sorted <- x_clean[ord]
  l_sorted <- log_clean[ord]

  diffs <- diff(as.integer(l_sorted))

  rows <- lapply(which(diffs != 0), function(i) {
    left_prop  <- i / n
    right_prop <- (n - i) / n

    # Place marker at the boundary value on the TRUE side of the transition
    x0 <- if (diffs[i] == 1L) x_sorted[i + 1L] else x_sorted[i]

    # Label the smaller (tail) side
    if (left_prop <= right_prop) {
      data.frame(x = x0, tail_prop = left_prop,  side = "lower",
                 stringsAsFactors = FALSE)
    } else {
      data.frame(x = x0, tail_prop = right_prop, side = "upper",
                 stringsAsFactors = FALSE)
    }
  })

  if (length(rows) == 0) {
    return(data.frame(x = numeric(0), tail_prop = numeric(0),
                      side = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# Format a tail proportion for display, snapping common alpha levels to
# clean strings (e.g. 0.025000001 → ".025").
.format_prop <- function(p) {
  snaps  <- c(0.005, 0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
  labels <- c(".005", ".01", ".025", ".05", ".10", ".15", ".20", ".25", ".30")
  idx <- which(abs(snaps - p) < 1e-9)
  if (length(idx)) labels[[idx[1]]] else format(round(p, 3), nsmall = 3)
}
