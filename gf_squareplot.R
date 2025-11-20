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
#' @usage
#' gf_squareplot(x, data = NULL, binwidth = NULL, origin = NULL,
#'               boundary = NULL, fill = "#7fcecc", color = "black",
#'               alpha = 1, na.rm = TRUE, mincount = NULL,
#'               bars = c("none", "outline", "solid"),
#'               xbreaks = NULL)
#'
#' @param x A numeric vector, or a one-sided formula such as `~b1`
#'   (with `data=` supplied).
#' @param data A data frame (needed only when using the formula interface).
#' @param binwidth Width of each histogram bin. If omitted, defaults to about 30 bins.
#' @param origin Left boundary of the first bin. Ignored if `boundary` is supplied.
#' @param boundary A fixed bin boundary (edge) used to align bins, like
#'   `geom_histogram(boundary = ...)`. Overrides `origin`.
#' @param fill Fill color for the unit rectangles and (in `"solid"` mode)
#'   the histogram bars. Defaults to `"#7fcecc"`.
#' @param color Outline color for histogram bars when `bars = "outline"` or `"solid"`.
#'   Unit rectangles **always** use white borders for readability.
#' @param alpha Transparency of rectangles and solid bars (0 = transparent, 1 = opaque).
#' @param na.rm Whether to remove `NA` values.
#' @param mincount Ensures the y-axis extends to at least this many counts.
#'   Prevents extremely tall blocks for small samples.
#' @param bars Mode: `"none"` (unit blocks only), `"outline"`, or `"solid"`.
#' @param xbreaks Controls x-axis tick locations.  
#'   * `NULL` (default): choose ~8 readable ticks with `pretty()`  
#'   * one number (e.g. `10`): ask for that many pretty breaks  
#'   * numeric vector: explicit tick positions  
#'
#' @details
#' The plot is designed to help students see:
#'
#' 1. **Each block is one sample statistic**  
#' 2. Blocks accumulate into **bars**  
#' 3. Bars transition into the familiar **histogram**  
#'
#' This supports conceptual understanding of distributions built from repeated
#' sampling or simulation.
#'
#' Internal spacing uses white horizontal borders to keep counts visually separated.
#' `mincount` guarantees enough vertical room.  
#'
#' @examples
#' # Using a numeric vector
#' gf_squareplot(sdob1$b1)
#'
#' # Formula interface
#' gf_squareplot(~ b1, data = sdob1)
#'
#' # Keep y-axis tall enough to avoid overly tall blocks
#' gf_squareplot(~ b1, data = sdob1, mincount = 15)
#'
#' # Add bar outline (concept stage 2)
#' gf_squareplot(~ b1, data = sdob1,
#'               bars = "outline", color = "red")
#'
#' # Solid histogram bars (stage 3)
#' gf_squareplot(~ b1, data = sdob1, bars = "solid")
#'
#' # Custom fill + alpha
#' gf_squareplot(~ b1, data = sdob1,
#'               fill = "orange", alpha = 0.5,
#'               bars = "outline")
#'
#' # Ask for ~10 x-axis ticks
#' gf_squareplot(~ b1, data = sdob1, xbreaks = 10)
#'
#' # Explicit ticks and aligned bin edges
#' gf_squareplot(~ b1, data = sdob1,
#'               binwidth = 5, boundary = 0,
#'               xbreaks = seq(-40, 40, 5))
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
                          xbreaks  = NULL) {

  bars <- match.arg(bars)

  # --- extract numeric x_vec ---
  is_formula <- inherits(x, "formula")
  if (is_formula) {
    if (is.null(data))
      stop("If `x` is a formula, you must supply `data=`.")
    vars <- all.vars(x)
    if (length(vars) != 1L)
      stop("Formula must be of form ~var.")
    x_vec   <- data[[vars[1]]]
    x_label <- vars[1]
  } else {
    x_vec   <- x
    x_label <- NULL
  }

  if (na.rm) x_vec <- x_vec[!is.na(x_vec)]
  if (!is.numeric(x_vec))
    stop("`x` must be numeric.")
  if (length(x_vec) == 0)
    stop("`x` has no non-missing values.")

  # --- choose binwidth ---
  if (is.null(binwidth)) {
    rng <- range(x_vec)
    binwidth <- if (diff(rng) == 0) 1 else diff(rng) / 30
  }

  # --- choose origin / boundary anchor ---
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

  # --- slot index within each bin (vertical stacking) ---
  slot <- ave(x_vec, bin, FUN = function(z) seq_along(z) - 1L)
  ymin <- slot
  ymax <- slot + 1

  rect_df <- data.frame(xmin = xmin, xmax = xmax,
                        ymin = ymin, ymax = ymax)

  # --- bar-level dataframe for outlines/solid bars ---
  if (nrow(rect_df) > 0) {
    bar_df <- aggregate(ymax ~ xmin + xmax, data = rect_df, FUN = max)
    names(bar_df)[3] <- "count"
    max_count <- max(bar_df$count)
  } else {
    bar_df        <- rect_df[FALSE, , drop = FALSE]
    bar_df$count  <- numeric(0)
    max_count     <- 0
  }

  # --- y-axis extent with mincount ---
  max_plot_count <- max(max_count, mincount %||% max_count)

  # --- readable y-axis tick steps ---
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

  # --- x-axis break calculations ---
  rng_x <- range(x_vec)
  if (diff(rng_x) == 0)
    rng_x <- rng_x + c(-0.5, 0.5)

  if (is.null(xbreaks)) {
    breaks_x <- pretty(rng_x, n = 8)
  } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
    breaks_x <- pretty(rng_x, n = xbreaks)
  } else if (is.numeric(xbreaks)) {
    breaks_x <- xbreaks
  } else {
    stop("`xbreaks` must be NULL, a single number, or a numeric vector.")
  }

  library(ggplot2)

  p <- ggplot()

  # --- 1. Unit rectangles (unless "solid" mode) ---
  if (bars != "solid") {
    p <- p +
      geom_rect(
        data = rect_df,
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax),
        fill   = fill,
        color  = "white",   # keep internal grid lines clean
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
        fill      = if (bars == "solid") fill else NA,
        color     = color,
        linewidth = 0.7,
        alpha     = if (bars == "solid") alpha else 1
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
