#' gf_squareplot: A Countable-Rectangle Histogram for Teaching Sampling Distributions
#'
#' Creates histograms where individual data points are visible as stacked unit rectangles,
#' making counts easy to visualize. Designed for teaching statistical concepts, particularly
#' sampling distributions and hypothesis testing.
#'
#' @section Basic Usage:
#' gf_squareplot(~variable, data = mydata)
#' gf_squareplot(~variable, data = mydata, bars = "outline")
#'
#' @section Main Parameters:
#' - x: Formula (~variable) or vector of numeric data
#' - data: Data frame (required if x is a formula)
#' - bars: Display style - "none" (squares only), "outline" (outlined bars), "solid" (filled bars)
#' - binwidth: Width of histogram bins (auto-calculated if NULL)
#' - mincount: Minimum y-axis height (useful for consistent scaling across plots)
#' - fill: Color for rectangles/bars (default: "#7fcecc")
#' - alpha: Transparency (0-1)
#'
#' @section X-Axis Control:
#' - xbreaks: Number of breaks (e.g., xbreaks=10) or vector of specific positions
#' - xrange: Set x-axis limits as c(min, max), e.g., xrange=c(-30, 30)
#'   Note: xbreaks uses R's pretty() for "nice" numbers, so actual count may vary.
#'   For exact control, use xbreaks=seq(-30, 30, by=5)
#'
#' @section DGP Overlay (show_dgp=TRUE):
#' Adds educational overlay for teaching hypothesis testing:
#' - Top axis: Shows population parameter (DGP) with equation Y = β₀ + β₁X + ε
#' - Bottom axis: Shows parameter estimate with equation Y = b₀ + b₁X + e
#' - Red triangle and β₁ = 0 label: Marks null hypothesis position
#' - Red b₁ label: Marks observed estimate position
#' All elements color-coordinated (blue for axes/equations, bright red for hypothesis test)
#'
#' @section Examples:
#' # Basic square plot
#' gf_squareplot(~b1, data = sampling_dist)
#'
#' # With outlined bars
#' gf_squareplot(~b1, data = sampling_dist, bars = "outline")
#'
#' # Teaching hypothesis testing with DGP overlay
#' gf_squareplot(~b1, data = sampling_dist, 
#'               show_dgp = TRUE,
#'               xrange = c(-30, 30),
#'               xbreaks = 10,
#'               mincount = 20)
#'
#' # Custom binning
#' gf_squareplot(~values, data = mydata, 
#'               binwidth = 2, 
#'               origin = 0)
#'
#' @section Teaching Notes:
#' - Individual squares make sample size and distribution shape concrete
#' - DGP overlay emphasizes difference between population parameters (Greek letters) 
#'   and sample estimates (regular letters)
#' - Red markers clearly indicate null hypothesis (β₁ = 0) vs. observed estimate (b₁)
#' - Use mincount to maintain consistent scale when comparing multiple plots
#' - Warning messages are automatically suppressed for cleaner classroom display
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
                          xrange   = NULL,
                          show_dgp = FALSE) {

  bars <- match.arg(bars)
  dgp_color <- "#003d70"

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

  # --- binwidth --------------------------------------------------------------
  if (is.null(binwidth)) {
    rng <- range(x_vec)
    binwidth <- if (diff(rng) == 0) 1 else diff(rng) / 30
  }

  # --- origin / boundary -----------------------------------------------------
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

  # space above for DGP overlay (add buffer to prevent clipping warnings)
  extra_top  <- if (show_dgp) max(3, 0.25 * max_plot_count) else 0
  y_upper    <- max_plot_count + extra_top + 1.0  # +1.0 buffer to prevent warnings

  # --- y-axis ticks ----------------------------------------------------------
  if (max_plot_count <= 10)      step_y <- 1
  else if (max_plot_count <= 20) step_y <- 2
  else if (max_plot_count <= 50) step_y <- 5
  else if (max_plot_count <= 100) step_y <- 10
  else                            step_y <- ceiling(max_plot_count / 10)

  breaks_y <- seq(0, max_plot_count, by = step_y)

  # --- x-range and breaks ----------------------------------------------------
  rng_x <- range(x_vec)
  if (diff(rng_x) == 0) rng_x <- rng_x + c(-0.5, 0.5)

  x_limits <- rng_x   # used for x-axis and DGP axis

  # Use xrange for breaks calculation if provided, otherwise use data range
  breaks_range <- if (!is.null(xrange)) xrange else x_limits

  if (is.null(xbreaks)) {
    breaks_x <- pretty(breaks_range, n = 8)
  } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
    breaks_x <- pretty(breaks_range, n = xbreaks)
  } else {
    breaks_x <- xbreaks
  }

  library(ggplot2)

  p <- ggplot()

  # --- unit rectangles -------------------------------------------------------
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

  # --- x-axis label (two-line teaching label) --------------------------------
  # When show_dgp is TRUE, we'll add the label as annotations instead
  x_lab <- if (show_dgp) "" else x_label

  base_theme <- theme_minimal() +
    theme(
      axis.line.x  = element_line(color = if (show_dgp) dgp_color else "black"),
      axis.line.y  = if (show_dgp) element_blank() else element_line(color = "black"),
      axis.text.x  = element_text(color = if (show_dgp) dgp_color else "black"),
      axis.title.x = element_text(color = if (show_dgp) dgp_color else "black"),
      plot.margin  = if (show_dgp) margin(5, 5, 45, 5) else margin(5, 5, 5, 5)
    )

  p <- p +
    labs(x = x_lab, y = "count") +
    scale_y_continuous(
      limits = c(0, y_upper),
      breaks = breaks_y,
      labels = breaks_y
    ) +
    scale_x_continuous(limits = xrange, breaks = breaks_x) +
    base_theme +
    coord_cartesian(clip = "off")

  x_min <- if (!is.null(xrange)) xrange[1] else x_limits[1]
  x_max <- if (!is.null(xrange)) xrange[2] else x_limits[2]

  # ============================================================================
  # DGP OVERLAY (top)
  # ============================================================================
  if (show_dgp) {

    axis_y  <- max_plot_count + extra_top * 0.40
    eq_y    <- max_plot_count + extra_top * 0.70
    title_y <- max_plot_count + extra_top * 0.98

    # DGP axis line (match x-axis width)
    p <- p + annotate(
      "segment",
      x = -Inf, xend = Inf,
      y = axis_y, yend = axis_y,
      color = dgp_color, linewidth = 0.5
    )

    # DGP title
    p <- p + annotate(
      "text", x = -Inf, y = title_y,
      label = "Population Parameter (DGP)",
      hjust = -0.01, vjust = 0,
      size  = 4, fontface = "bold", color = dgp_color
    )

    # Population model equation (top)
    p <- p + annotate(
      "text", x = -Inf, y = eq_y,
      label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]",
      parse  = TRUE,
      hjust  = -0.01,
      vjust  = 0.5,
      size   = 4, fontface = "bold",
      color  = dgp_color
    )

    # Red β1 = 0 triangle + label at 0 on DGP axis
    if (0 >= x_min && 0 <= x_max) {

      # Red triangle pointing down with tip touching the axis from above
      triangle_y <- axis_y + extra_top * 0.16
      
      p <- p + annotate(
        "point",
        x = 0, y = triangle_y,
        shape = 25,  # filled triangle pointing down
        size = 4,
        color = "#E60000",
        fill = "#E60000",
        alpha = 1
      )

      # label above triangle
      label_y <- axis_y + extra_top * 0.48

      p <- p + annotate(
        "text",
        x = 0, y = label_y,
        label = "beta[1] == 0",
        parse = TRUE,
        size  = 5,
        fontface = "bold",
        color = "#E60000",
        alpha = 1
      )
    }
  }

  # ============================================================================
  # Bottom x-axis labels (two lines, left-aligned to match upper DGP labels)
  # ============================================================================
  if (show_dgp) {
    # "Parameter Estimate" title - top aligned with red b₁
    p <- p + annotate(
      "text", x = -Inf, y = -Inf,
      label = "Parameter Estimate",
      hjust = -0.01, vjust = 3.2,
      size  = 4, fontface = "bold", color = dgp_color
    )
    
    # Equation below title (sample estimate version with b, e)
    p <- p + annotate(
      "text", x = -Inf, y = -Inf,
      label = "Y[i] == b[0] + b[1] * X[i] + e[i]",
      parse  = TRUE,
      hjust  = -0.01,
      vjust  = 4.0,
      size   = 4, fontface = "bold",
      color  = dgp_color
    )
    
    # Red b₁ label - top aligned with "Parameter Estimate"
    if (0 >= x_min && 0 <= x_max) {
      p <- p + annotate(
        "text",
        x = 0, y = -Inf,
        vjust = 2.5,
        label = "b[1]",
        parse = TRUE,
        size  = 5,
        fontface = "bold",
        color = "#E60000",
        alpha = 1
      )
    }
  }

  # Add custom class and print method to suppress warnings
  class(p) <- c("gf_squareplot", class(p))
  p
}

# Custom print method to suppress warnings for teaching clarity
#' @export
print.gf_squareplot <- function(x, ...) {
  suppressWarnings(NextMethod("print", x, ...))
  invisible(x)
}
