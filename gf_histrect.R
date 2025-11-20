#' gf_histrect: A Countable-Rectangle Histogram for Teaching Sampling Distributions
#'
#' @description
#' `gf_histrect()` creates a histogram where **each individual observation** is
#' represented as a **small rectangle**—stacked to show bin counts.
#'
#' This visualization is designed for teaching:
#'
#' * how individual values (e.g., bootstrap b1s) accumulate into histogram bars
#' * the transition from **one block = one sample statistic** → bar outlines → solid histogram
#' * how the shape of a sampling distribution emerges from the underlying values
#'
#' It supports three display modes using the `bars` argument:
#'
#' * `"none"` – (default) only individual rectangles, no bars
#' * `"outline"` – rectangles + histogram bar outline
#' * `"solid"` – solid histogram bars, hiding the rectangles
#'
#' This allows instructors to reveal the histogram conceptually in stages.
#'
#' @usage
#' gf_histrect(x, data = NULL, binwidth = NULL, origin = NULL,
#'             boundary = NULL, fill = "#7fcecc", color = "black",
#'             alpha = 1, na.rm = TRUE, mincount = NULL,
#'             bars = c("none", "outline", "solid"),
#'             xbreaks = NULL)
#'
#' @param x A numeric vector, **or** a one-sided formula like `~b1` (with `data=`).
#' @param data A data frame (needed only when using the formula interface).
#' @param binwidth Width of each histogram bin. Defaults to approx. 30 bins over the data range.
#' @param origin Left boundary of the first bin. Defaults to a multiple of `binwidth`
#'   based on the minimum of `x`. Ignored if `boundary` is supplied.
#' @param boundary A bin boundary (edge) used to align the bins, similar to
#'   `geom_histogram(boundary = ...)`. When supplied, it overrides `origin`:
#'   one bin edge will be exactly at `boundary`, and other bins are spaced
#'   by `binwidth` from there.
#' @param fill Fill color for rectangles and (optionally) the histogram bars.
#'   Default is `"#7fcecc"`.
#' @param color Color for *bar outlines* when `bars = "outline"` or `"solid"`.
#'   Note: **unit rectangles always use white borders** so internal grid lines remain clear.
#' @param alpha Transparency level for the unit rectangles and (when `bars = "solid"`)
#'   the bar fill. Between 0 (fully transparent) and 1 (fully opaque).
#' @param na.rm Logical; whether to remove `NA` values.
#' @param mincount Minimum height of the y-axis (in count units).
#'   Useful to keep rectangles from looking extremely tall for small sample sizes.
#' @param bars `"none"` (default), `"outline"`, or `"solid"`.
#'   Controls the transition from unit-blocks → bar-outline → solid histogram.
#' @param xbreaks Controls x-axis tick marks. Options:
#'   * `NULL` (default): use about 8 "pretty" breaks over the data range.
#'   * a single number (e.g. `10`): ask for about that many pretty breaks.
#'   * a numeric vector: explicit tick positions (e.g. `seq(-40, 40, 5)`).
#'
#' @details
#' This function is pedagogically motivated. It allows students to:
#'
#' 1. **Count the blocks** representing individual sample statistics.
#' 2. See how blocks accumulate into **bar shapes**.
#' 3. Finally view the **formal histogram** by transitioning to `"outline"` or `"solid"`.
#'
#' The y-axis scaling, tick spacing, and optional `mincount` padding are designed
#' to avoid visual distortion with small samples. The x-axis ticks use `pretty()`
#' by default for a slightly denser and more readable set of labels than the
#' ggplot2 default, while still allowing full control via `xbreaks`.
#'
#' The `boundary` argument lets you align bin edges in a controlled way, e.g.
#' to ensure a bin edge is exactly at 0, or at some meaningful value, mirroring
#' the behavior of `geom_histogram(boundary = ...)`.
#'
#' @examples
#' # Using a vector
#' gf_histrect(sdob1$b1)
#'
#' # Formula interface
#' gf_histrect(~ b1, data = sdob1)
#'
#' # Guarantee the y-axis goes to at least 15 counts
#' gf_histrect(~ b1, data = sdob1, mincount = 15)
#'
#' # Add bar outline (stage 2 of the conceptual reveal)
#' gf_histrect(~ b1, data = sdob1, bars = "outline", color = "red")
#'
#' # Solid histogram bars (stage 3)
#' gf_histrect(~ b1, data = sdob1, bars = "solid")
#'
#' # Use custom fill and alpha
#' gf_histrect(~ b1, data = sdob1,
#'             fill = "orange", alpha = 0.6,
#'             bars = "outline")
#'
#' # Slightly denser x-axis ticks (about 10)
#' gf_histrect(~ b1, data = sdob1, xbreaks = 10)
#'
#' # Explicit x-axis ticks every 5 units, with aligned bin edges
#' gf_histrect(~ b1, data = sdob1,
#'             binwidth = 5, boundary = 0,
#'             xbreaks = seq(-40, 40, 5))
#'
#' # Add any ggplot theme
#' gf_histrect(~ b1, data = sdob1) + ggplot2::theme_classic()
#'
#' @export
#'

`%||%` <- function(a, b) if (!is.null(a)) a else b

gf_histrect <- function(x,
                        data     = NULL,
                        binwidth = NULL,
                        origin   = NULL,
                        boundary = NULL,
                        fill     = "#7fcecc",
                        color    = "black",   # bar outline color
                        alpha    = 1,
                        na.rm    = TRUE,
                        mincount = NULL,
                        bars     = c("none", "outline", "solid"),
                        xbreaks  = NULL) {

  bars <- match.arg(bars)

  # --- extract numeric x_vec ---
  is_formula <- inherits(x, "formula")
  if (is_formula) {
    if (is.null(data)) stop("If `x` is a formula, you must supply `data=`.")
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

  # --- choose binwidth ---
  if (is.null(binwidth)) {
    rng <- range(x_vec)
    binwidth <- if (diff(rng) == 0) 1 else diff(rng) / 30
  }

  # --- choose origin / boundary anchor ---
  # If boundary is supplied, align a bin edge at that value (like geom_histogram).
  if (!is.null(boundary)) {
    origin <- boundary
  } else {
    if (is.null(origin)) {
      origin <- floor(min(x_vec) / binwidth) * binwidth
    }
  }

  # --- assign bins ---
  bin  <- floor((x_vec - origin) / binwidth)
  xmin <- origin + bin * binwidth
  xmax <- xmin + binwidth

  # --- slot index within each bin ---
  slot <- ave(x_vec, bin, FUN = function(z) seq_along(z) - 1L)
  ymin <- slot
  ymax <- slot + 1

  rect_df <- data.frame(xmin = xmin, xmax = xmax,
                        ymin = ymin, ymax = ymax)

  # --- bar-level counts ---
  if (nrow(rect_df) > 0) {
    bar_df <- aggregate(ymax ~ xmin + xmax, data = rect_df, FUN = max)
    names(bar_df)[3] <- "count"
    max_count <- max(bar_df$count)
  } else {
    bar_df <- rect_df[FALSE, , drop = FALSE]
    bar_df$count <- numeric(0)
    max_count <- 0
  }

  # --- y-axis extent with mincount ---
  max_plot_count <- max(max_count, mincount %||% max_count)

  # --- choose readable y tick spacing ---
  if (max_plot_count <= 10) {
    step_y <- 1
  } else if (max_plot_count <= 20) {
    step_y <- 2
  } else if (max_plot_count <= 50) {
    step_y <- 5
  } else if (max_plot_count <= 100) {
    step_y <- 10
  } else {
    step_y <- ceiling(max_plot_count / 10)
  }
  breaks_y <- seq(0, max_plot_count, by = step_y)

  # --- x-axis range and breaks ---
  rng_x <- range(x_vec)
  if (diff(rng_x) == 0) {
    # Avoid degenerate range for pretty()
    rng_x <- rng_x + c(-0.5, 0.5)
  }

  if (is.null(xbreaks)) {
    # default: about 8 nice breaks
    breaks_x <- pretty(rng_x, n = 8)
  } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
    # xbreaks = 10 → ask pretty() for ~10 breaks
    breaks_x <- pretty(rng_x, n = xbreaks)
  } else if (is.numeric(xbreaks)) {
    # explicit vector of positions
    breaks_x <- xbreaks
  } else {
    stop("`xbreaks` must be NULL, a single number, or a numeric vector.")
  }

  library(ggplot2)

  p <- ggplot()

  # --- 1. Unit rectangles (unless "solid" mode) ---
  # Borders ALWAYS white: keeps the internal "count grid" clean.
  if (bars != "solid") {
    p <- p +
      geom_rect(
        data = rect_df,
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax),
        fill   = fill,
        color  = "white",
        alpha  = alpha
      )
  }

  # --- 2. Histogram outline or solid bars ---
  if (bars %in% c("outline", "solid") && nrow(bar_df) > 0) {
    p <- p +
      geom_rect(
        data = bar_df,
        aes(xmin = xmin, xmax = xmax,
            ymin = 0, ymax = count),
        fill     = if (bars == "solid") fill else NA,
        color    = color,      # bar outlines use color=
        linewidth = 0.7,
        alpha    = if (bars == "solid") alpha else 1
      )
  }

  # --- 3. Axes + theme ---
  p +
    labs(
      x = x_label,
      y = "count"
    ) +
    scale_y_continuous(
      limits = c(0, max_plot_count),
      breaks = breaks_y,
      labels = breaks_y
    ) +
    scale_x_continuous(
      breaks = breaks_x,
      labels = breaks_x
    ) +
    theme_minimal()
}
