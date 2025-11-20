#' gf_squareplot: A Countable-Rectangle Histogram for Teaching Sampling Distributions
#'
#' See discussion in code comments for teaching use of the DGP overlay.
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

  # space above for DGP overlay
  extra_top  <- if (show_dgp) max(3, 0.25 * max_plot_count) else 0
  y_upper    <- max_plot_count + extra_top

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

  if (is.null(xbreaks)) {
    breaks_x <- pretty(x_limits, n = 8)
  } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
    breaks_x <- pretty(x_limits, n = xbreaks)
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
        hjust      = 0,          # left-justify the two-line label
        lineheight = 0.75,       # tighter spacing between the two lines
        color      = if (show_dgp) dgp_color else "black"
      )
    )

  p <- p +
    labs(x = x_lab, y = "count") +
    scale_y_continuous(
      limits = c(0, y_upper),
      breaks = breaks_y,
      labels = breaks_y
    ) +
    scale_x_continuous(limits = x_limits, breaks = breaks_x) +
    base_theme +
    coord_cartesian(clip = "off")

  x_min <- x_limits[1]
  x_max <- x_limits[2]

  # ============================================================================
  # DGP OVERLAY (top)
  # ============================================================================
  if (show_dgp) {

    axis_y  <- max_plot_count + extra_top * 0.25
    eq_y    <- max_plot_count + extra_top * 0.55
    title_y <- max_plot_count + extra_top * 0.85

    # DGP axis line (match x-axis width)
    p <- p + annotate(
      "segment",
      x = x_min, xend = x_max,
      y = axis_y, yend = axis_y,
      color = dgp_color, linewidth = 0.8
    )

    # DGP title
    p <- p + annotate(
      "text", x = x_min, y = title_y,
      label = "Data Generating Process (DGP)",
      hjust = 0, vjust = 0,
      size  = 4, color = dgp_color
    )

    # Population model equation (top)
    p <- p + annotate(
      "text", x = x_min, y = eq_y,
      label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]",
      parse  = TRUE,
      hjust  = 0,
      vjust  = 0.5,
      size   = 4,
      color  = dgp_color
    )

    # Red β1 = 0 tick + label at 0 on DGP axis
    if (0 >= x_min && 0 <= x_max) {

      tick_len <- extra_top * 0.20
      tick_top <- axis_y + tick_len

      # red tick mark
      p <- p + annotate(
        "segment",
        x = 0, xend = 0,
        y = axis_y, yend = tick_top,
        color = "red3", linewidth = 0.7
      )

      # label above tick
      label_y <- tick_top + extra_top * 0.10

      p <- p + annotate(
        "text",
        x = 0, y = label_y,
        label = "beta[1] == 0",
        parse = TRUE,
        size  = 5,
        fontface = "bold",
        color = "red3"
      )
    }
  }

  # ============================================================================
  # Bottom red b1 label (centered under the x-axis, below tick labels)
  # ============================================================================
  if (show_dgp) {
    if (0 >= x_min && 0 <= x_max) {
      p <- p + annotate(
        "text",
        x = 0, y = -Inf,          # attach to bottom margin
        vjust = -1.2,             # push a bit below tick labels
        label = "b[1]",
        parse = TRUE,
        size  = 5,
        fontface = "bold",
        color = "red3"
      )
    }
  }

  p
}
