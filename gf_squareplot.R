#' gf_squareplot: A Countable-Rectangle Histogram for Teaching Sampling Distributions
#'
#' @description
#' `gf_squareplot()` creates a histogram-like plot where **each individual
#' observation** is represented as a **small rectangle**, stacked vertically
#' to show bin counts.
#'
#' It is designed for teaching:
#'
#' * how individual values (e.g., bootstrap b1s) accumulate into histogram bars
#' * the conceptual progression:
#'     **unit blocks → bar outlines → solid histogram**
#' * how sampling distributions acquire their overall shape
#'
#' Three visualization modes are provided:
#'
#' * `"none"` – only individual rectangles (default)
#' * `"outline"` – rectangles + histogram bar outline
#' * `"solid"` – solid histogram bars, hiding rectangles
#'
#' Optionally, `show_dgp = TRUE` overlays a dark-blue “Data Generating Process
#' (DGP)” axis above the plot, labels it with the population model
#' \(Y_i = \beta_0 + \beta_1 X_i + \epsilon_i\), and marks β₁ = 0 on that axis
#' with a red tick mark and annotation box.
#'
#' @usage
#' gf_squareplot(x, data = NULL, binwidth = NULL, origin = NULL,
#'               boundary = NULL, fill = "#7fcecc", color = "black",
#'               alpha = 1, na.rm = TRUE, mincount = NULL,
#'               bars = c("none", "outline", "solid"),
#'               xbreaks = NULL, show_dgp = FALSE)
#'
#' @param x A numeric vector, or a one-sided formula like `~b1`
#' @param data A data frame (only used when `x` is a formula)
#' @param binwidth Width of each histogram bin (default ~30 bins)
#' @param origin Left boundary of first bin (ignored if `boundary` is used)
#' @param boundary Value at which a bin edge must align (like geom_histogram)
#' @param fill Fill color for blocks and solid bars
#' @param color Outline color for bar outlines (unit blocks always use white)
#' @param alpha Transparency (0–1)
#' @param na.rm Drop missing values?
#' @param mincount Minimum visible y-range for count axis
#' @param bars `"none"`, `"outline"`, or `"solid"`
#' @param xbreaks NULL (pretty), a number (# pretty breaks), or numeric vector
#' @param show_dgp If TRUE, add the DGP axis, labels, and β₁=0 annotation
#'
#' @export
#'

`%||%` <- function(a, b) if (!is.null(a)) a else b


gf_squareplot <- function(x,
                          data     = NULL,
                          binwidth = NULL,
                          origin   = NULL,
                          boundary = NULL,
                          fill     = "#7fcecc",
                          color    = "black",
                          alpha    = 1,
                          na.rm    = TRUE,
                          mincount = NULL,
                          bars     = c("none", "outline", "solid"),
                          xbreaks  = NULL,
                          show_dgp = FALSE) {

  bars <- match.arg(bars)
  dgp_color <- "#004f8f"

  # --- extract x vector ------------------------------------------------------

  is_formula <- inherits(x, "formula")
  if (is_formula) {
    if (is.null(data)) stop("If `x` is a formula, supply `data=`.")
    vars <- all.vars(x)
    if (length(vars) != 1L) stop("Formula must be of form ~var.")
    x_vec   <- data[[vars[1]]]
    x_label <- vars[1]
  } else {
    x_vec   <- x
    x_label <- NULL
  }

  if (na.rm) x_vec <- x_vec[!is.na(x_vec)]
  if (!is.numeric(x_vec)) stop("`x` must be numeric.")
  if (length(x_vec) == 0) stop("`x` has no non-missing values.")

  # --- determine binwidth ----------------------------------------------------

  if (is.null(binwidth)) {
    rng <- range(x_vec)
    binwidth <- if (diff(rng) == 0) 1 else diff(rng) / 30
  }

  # --- determine origin / boundary ------------------------------------------

  if (!is.null(boundary)) {
    origin <- boundary
  } else if (is.null(origin)) {
    origin <- floor(min(x_vec) / binwidth) * binwidth
  }

  # --- assign bins -----------------------------------------------------------

  bin  <- floor((x_vec - origin) / binwidth)
  xmin <- origin + bin * binwidth
  xmax <- xmin + binwidth

  slot <- ave(x_vec, bin, FUN = function(z) seq_along(z) - 1L)
  ymin <- slot
  ymax <- slot + 1

  rect_df <- data.frame(xmin, xmax, ymin, ymax)

  # --- bar counts ------------------------------------------------------------

  if (nrow(rect_df) > 0) {
    bar_df <- aggregate(ymax ~ xmin + xmax, rect_df, max)
    names(bar_df)[3] <- "count"
    max_count <- max(bar_df$count)
  } else {
    bar_df <- rect_df[FALSE, ]
    bar_df$count <- 0
    max_count <- 0
  }

  max_plot_count <- max(max_count, mincount %||% max_count)

  # --- vertical extension for DGP overlay ------------------------------------

  extra_space <- if (show_dgp) max(3, 0.25 * max_plot_count) else 0
  y_upper     <- max_plot_count + extra_space

  # --- y-axis tick calculation -----------------------------------------------

  if (max_plot_count <= 10)      step_y <- 1
  else if (max_plot_count <= 20) step_y <- 2
  else if (max_plot_count <= 50) step_y <- 5
  else if (max_plot_count <= 100) step_y <- 10
  else                            step_y <- ceiling(max_plot_count / 10)

  breaks_y <- seq(0, max_plot_count, by = step_y)

  # --- x-range and breaks -----------------------------------------------------

  rng_x <- range(x_vec)
  if (diff(rng_x) == 0) rng_x <- rng_x + c(-0.5, 0.5)

  x_limits <- rng_x

  if (is.null(xbreaks)) {
    breaks_x <- pretty(x_limits, n = 8)
  } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
    breaks_x <- pretty(x_limits, n = xbreaks)
  } else {
    breaks_x <- xbreaks
  }

  library(ggplot2)
  p <- ggplot()

  # --- unit rectangles --------------------------------------------------------

  if (bars != "solid") {
    p <- p + geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = fill, color = "white", alpha = alpha
    )
  }

  # --- bar outlines / solid bars --------------------------------------------

  if (bars %in% c("outline", "solid") && nrow(bar_df) > 0) {
    p <- p + geom_rect(
      data = bar_df,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count),
      fill      = if (bars == "solid") fill else NA,
      color     = color,
      linewidth = 0.7,
      alpha     = if (bars == "solid") alpha else 1
    )
  }

  # --- bottom axis label -----------------------------------------------------

  x_lab <- if (show_dgp) {
    expression(
      atop("Parameter Estimate",
           Y[i] == beta[0] + beta[1] * X[i] + epsilon[i])
    )
  } else x_label

  base_theme <- theme_minimal() +
    theme(
      axis.line.x  = element_line(color = if (show_dgp) dgp_color else "black"),
      axis.line.y  = element_line(color = "black"),
      axis.text.x  = element_text(color = if (show_dgp) dgp_color else "black"),
      axis.title.x = element_text(
        hjust      = 0,          # left-justify
        lineheight = 0.80,       # tightened spacing between lines
        color      = if (show_dgp) dgp_color else "black"
      )
    )

  p <- p +
    labs(x = x_lab, y = "count") +
    scale_y_continuous(limits = c(0, y_upper), breaks = breaks_y) +
    scale_x_continuous(limits = x_limits, breaks = breaks_x) +
    base_theme

  # ============================================================================
  # DGP OVERLAY
  # ============================================================================

  if (show_dgp) {

    x_min <- x_limits[1]
    x_max <- x_limits[2]

    axis_y  <- max_plot_count + extra_space * 0.25
    eq_y    <- max_plot_count + extra_space * 0.50
    title_y <- max_plot_count + extra_space * 0.80

    # --- blue DGP axis --------------------------------------------------------

    p <- p + annotate(
      "segment",
      x = x_min, xend = x_max,
      y = axis_y, yend = axis_y,
      color = dgp_color, linewidth = 0.8
    )

    # --- title ---------------------------------------------------------------

    p <- p + annotate(
      "text", x = x_min, y = title_y,
      label = "Data Generating Process (DGP)",
      hjust = 0, vjust = 0,
      size  = 4, color = dgp_color
    )

    # --- population model equation -------------------------------------------

    p <- p + annotate(
      "text", x = x_min, y = eq_y,
      label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]",
      parse  = TRUE,
      hjust  = 0,
      vjust  = 0.5,
      size   = 4,
      color  = dgp_color
    )

    # --- red β1 = 0 box + tick ------------------------------------------------

    if (0 >= x_min && 0 <= x_max) {

      tick_len <- extra_space * 0.20
      box_h    <- extra_space * 0.18
      box_w    <- diff(x_limits) * 0.02

      # red tick mark pointing up from the DGP axis
      p <- p + annotate(
        "segment",
        x = 0, xend = 0,
        y = axis_y, yend = axis_y + tick_len,
        color = "red3", linewidth = 0.7
      )

      # center of box above tick
      box_center_y <- axis_y + tick_len + box_h * 1.1

      # box + β1 = 0 text
      p <- p +
        annotate(
          "rect",
          xmin = 0 - box_w, xmax = 0 + box_w,
          ymin = box_center_y - box_h,
          ymax = box_center_y + box_h,
          color = "red3", fill = "white",
          linewidth = 0.7
        ) +
        annotate(
          "text",
          x = 0, y = box_center_y,
          label = "beta[1] == 0",
          parse = TRUE,
          size  = 4,
          color = "red3"
        )
    }
  }

  p
}
