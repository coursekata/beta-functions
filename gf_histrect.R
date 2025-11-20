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
#'
#' @usage
#' gf_histrect(x, data = NULL, binwidth = NULL, origin = NULL,
#'             fill = "steelblue", color = "black",
#'             na.rm = TRUE, mincount = NULL,
#'             bars = c("none", "outline", "solid"))
#'
#'
#' @param x A numeric vector, **or** a one-sided formula like `~b1` (with `data=`).
#' @param data A data frame (needed only when using the formula interface).
#' @param binwidth Width of each histogram bin. Defaults to approx. 30 bins.
#' @param origin Left boundary of the first bin. Defaults to a multiple of binwidth.
#' @param fill Fill color for rectangles and (optionally) the histogram bars.
#' @param color Color for *bar outlines* when `bars = "outline"` or `"solid"`.
#'        Note: **unit rectangles always use white borders** so internal grid lines remain clear.
#' @param na.rm Logical; whether to remove `NA` values.
#' @param mincount Minimum height of the y-axis (in count units).  
#'        Useful to keep rectangles from looking extremely tall for small sample sizes.
#' @param bars `"none"` (default), `"outline"`, or `"solid"`.  
#'        Controls transition from unit-blocks → bar-outline → solid histogram.
#'
#'
#' @details
#' This function is pedagogically motivated.  
#' It allows students to:
#'
#' 1. **Count the blocks** representing individual sample statistics.  
#' 2. See how blocks accumulate into **bar shapes**.  
#' 3. Finally view the **formal histogram** by transitioning to `"outline"` or `"solid"`.
#'
#' The axis scaling, tick spacing, and minimum height logic are designed
#' to avoid visual distortion with small samples.
#'
#'
#' @examples
#' # Using a vector
#' gf_histrect(sdob1$b1)
#'
#' # Formula interface
#' gf_histrect(~b1, data = sdob1)
#'
#' # Guarantee the y-axis goes to at least 15 counts
#' gf_histrect(~b1, data = sdob1, mincount = 15)
#'
#' # Add bar outline (stage 2 of the conceptual reveal)
#' gf_histrect(~b1, data = sdob1, bars = "outline", color = "red")
#'
#' # Solid histogram bars
#' gf_histrect(~b1, data = sdob1, bars = "solid", fill = "orange")
#'
#' # Add any ggplot theme
#' gf_histrect(~b1, data = sdob1) + theme_classic()
#'
#'
#' @export
#'

`%||%` <- function(a, b) if (!is.null(a)) a else b


gf_histrect <- function(x,
                        data     = NULL,
                        binwidth = NULL,
                        origin   = NULL,
                        fill     = "steelblue",
                        color    = "black",   # bar outline color
                        na.rm    = TRUE,
                        mincount = NULL,
                        bars     = c("none", "outline", "solid")) {

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

  # --- choose origin ---
  if (is.null(origin)) {
    origin <- floor(min(x_vec) / binwidth) * binwidth
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

  # --- choose readable tick spacing ---
  if (max_plot_count <= 10) {
    step <- 1
  } else if (max_plot_count <= 20) {
    step <- 2
  } else if (max_plot_count <= 50) {
    step <- 5
  } else if (max_plot_count <= 100) {
    step <- 10
  } else {
    step <- ceiling(max_plot_count / 10)
  }
  breaks <- seq(0, max_plot_count, by = step)

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
        color  = "white"
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
        linewidth = 0.7
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
      breaks = breaks,
      labels = breaks
    ) +
    theme_minimal()
}
