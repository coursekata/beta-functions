# deps: ggplot2, rlang
#
# gf_cutoffs() -- add cutoff markers to a histogram
#
# A cleaner, more explicit alternative to show_cutoffs(). Works with any
# distribution-part function (middle, tails, upper, lower, outer) passed
# directly as an argument, or auto-detects one from the histogram fill.
#
# Two usage modes:
#
#   # 1. Explicit -- no fill needed; expression drives the cutoffs
#   gf_histogram(~ b1, data = sdob1) %>%
#     gf_cutoffs(middle(b1, .95))
#
#   # 2. Auto-detect -- reads the fill aesthetic if no expression is given
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
#'   (dark charcoal -- unobtrusive against the histogram fill).
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
    stop("gf_cutoffs: plot must have an x aesthetic -- use with gf_histogram()")
  }
  if (!is.null(p$mapping$y)) {
    stop(
      "gf_cutoffs: plot has a y aesthetic -- gf_cutoffs() only works with ",
      "gf_histogram(), not scatter plots or other two-variable plots."
    )
  }
  x_name   <- tryCatch(rlang::as_name(p$mapping$x), error = function(e) NULL)
  # Use p$data as a data mask when it's a real data frame; otherwise fall back
  # to the quosure's captured environment (handles bare variables with no data=).
  data_mask <- if (is.data.frame(p$data)) p$data else NULL
  x_vals <- tryCatch(
    rlang::eval_tidy(p$mapping$x, data = data_mask),
    error = function(e) stop(
      "gf_cutoffs: could not evaluate x variable",
      if (!is.null(x_name)) paste0(" '", x_name, "'") else "",
      ".", call. = FALSE
    )
  )

  # ── 2. Get logical vector from expression or fill ────────────────────────────
  if (!rlang::quo_is_null(expr_quo)) {
    logical_vec <- rlang::eval_tidy(expr_quo, data = data_mask)
  } else {
    fill_quo <- .extract_dist_fill(p)
    if (is.null(fill_quo)) {
      stop(paste0(
        "gf_cutoffs: no expression given and no distribution-part fill found.\n",
        "Either pass an expression: gf_cutoffs(middle(",
        if (!is.null(x_name)) x_name else "x", ", .95))\n",
        "Or set fill = ~middle(",
        if (!is.null(x_name)) x_name else "x", ", .95) in gf_histogram()."
      ))
    }
    logical_vec <- rlang::eval_tidy(fill_quo, data = data_mask)
  }

  if (!is.logical(logical_vec)) {
    stop("gf_cutoffs: expression must return a logical vector (TRUE/FALSE).")
  }

  # ── 3. Find cutoff positions from the logical vector ─────────────────────────
  cutoffs <- .find_cutoffs(x_vals, logical_vec)

  if (nrow(cutoffs) == 0) {
    warning("gf_cutoffs: no cutoff positions found -- returning plot unchanged.",
            call. = FALSE)
    return(p)
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

  # ── 5. Warn if labels would overlap existing text annotations ────────────────
  if (labels) {
    has_text_annotations <- any(sapply(p$layers, function(l) {
      inherits(l$geom, "GeomText") && !isTRUE(l$inherit.aes)
    }))
    if (has_text_annotations) {
      warning(
        "gf_cutoffs: labels = TRUE on multiple stacked calls will overlap. ",
        "Use labels = TRUE on only one gf_cutoffs() call at a time.",
        call. = FALSE
      )
    }
  }

  # ── 6. Draw markers ──────────────────────────────────────────────────────────
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

  if (inherits(p$coordinates, "CoordCartesian") && p$coordinates$clip == "off") {
    p
  } else {
    p + ggplot2::coord_cartesian(clip = "off")
  }
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

# Find cutoff x-positions using the same rank-based logic as distribution_parts.
#
# lower() sorts ascending and takes the first k obs → lower boundary = x at
# rank k in ascending sort. upper() sorts DESCENDING and takes the first k obs
# → upper boundary = x at rank k in descending sort (= minimum x among the
# upper tail). We mirror each sort exactly so ties are broken identically.
#
# Which region is the tail? FALSE is the minority for middle/upper/lower; TRUE
# is the minority for tails/outer. Tie-breaking for 50/50 cases matches the
# single-boundary (upper/lower .5) and two-boundary (outer .5) patterns.
.find_cutoffs <- function(x_vals, logical_vec) {
  keep      <- !is.na(x_vals) & !is.na(logical_vec)
  x_clean   <- x_vals[keep]
  log_clean <- logical_vec[keep]
  n         <- length(x_clean)

  # Ascending sort (mirrors lower())
  ord_asc  <- order(x_clean)
  x_asc    <- x_clean[ord_asc]
  l_asc    <- log_clean[ord_asc]

  false_total <- sum(!l_asc)
  true_total  <- n - false_total
  n_trans     <- sum(diff(as.integer(l_asc)) != 0)

  tail_is_false <- if (false_total != true_total) {
    false_total < true_total
  } else if (n_trans <= 1L) {
    FALSE          # upper(.5) / lower(.5): label the TRUE region
  } else {
    !l_asc[1L]     # two-boundary equal: outer pattern → first value is tail
  }

  is_tail_asc <- if (tail_is_false) !l_asc else l_asc

  # Lower boundary: consecutive tail at the START of ascending sort
  k_lower <- if (is_tail_asc[1L]) rle(is_tail_asc)$lengths[1L] else 0L

  # Descending sort (mirrors upper())
  ord_desc    <- order(x_clean, decreasing = TRUE)
  l_desc      <- log_clean[ord_desc]
  x_desc      <- x_clean[ord_desc]
  is_tail_desc <- if (tail_is_false) !l_desc else l_desc

  # Upper boundary: consecutive tail at the START of descending sort
  k_upper <- if (is_tail_desc[1L]) rle(is_tail_desc)$lengths[1L] else 0L

  rows <- list()

  if (k_lower > 0L) {
    rows <- c(rows, list(data.frame(
      x         = x_asc[k_lower],
      tail_prop = k_lower / n,
      side      = "lower",
      stringsAsFactors = FALSE
    )))
  }

  if (k_upper > 0L) {
    # x_desc[k_upper] is the minimum x in the upper tail
    # (x_desc is descending, so rank k_upper is the smallest value in the tail)
    rows <- c(rows, list(data.frame(
      x         = x_desc[k_upper],
      tail_prop = k_upper / n,
      side      = "upper",
      stringsAsFactors = FALSE
    )))
  }

  if (length(rows) == 0) {
    return(data.frame(x = numeric(0), tail_prop = numeric(0),
                      side = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# Format a tail proportion for display, snapping common alpha levels to
# clean strings (e.g. 0.025000001 → ".025").
.format_prop <- function(p) {
  snaps  <- c(0.005, 0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
  labels <- c(".005", ".01", ".025", ".05", ".10", ".15", ".20", ".25", ".30", ".35", ".40", ".45", ".50")
  idx <- which(abs(snaps - p) < 1e-9)
  if (length(idx)) labels[[idx[1]]] else format(round(p, 3), nsmall = 3)
}
