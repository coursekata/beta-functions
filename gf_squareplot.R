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

  # --- extract x vector ---
  is_formula <- inherits(x, "formula")
  if (is_formula) {
    if (is.null(data)) stop("If `x` is a formula, supply `data=`.")
    vars <- all.vars(x)
    if (length(vars) != 1L) stop("Formula must be ~var.")
    x_vec   <- data[[vars[1]]]
    x_label <- vars[1]
  } else {
    x_vec   <- x
    x_label <- NULL
  }

  if (na.rm) x_vec <- x_vec[!is.na(x_vec)]
  if (!is.numeric(x_vec)) stop("`x` must be numeric.")
  if (length(x_vec) == 0) stop("`x` has no non-missing values.")

  # --- binwidth ---
  if (is.null(binwidth)) {
    rng <- range(x_vec)
    binwidth <- if (diff(rng) == 0) 1 else diff(rng) / 30
  }

  # --- origin / boundary ---
  if (!is.null(boundary)) {
    origin <- boundary
  } else if (is.null(origin)) {
    origin <- floor(min(x_vec) / binwidth) * binwidth
  }

  # --- bin assignment ---
  bin  <- floor((x_vec - origin) / binwidth)
  xmin <- origin + bin * binwidth
  xmax <- xmin + binwidth

  slot <- ave(x_vec, bin, FUN = function(z) seq_along(z) - 1L)
  ymin <- slot
  ymax <- slot + 1

  rect_df <- data.frame(xmin = xmin, xmax = xmax,
                        ymin = ymin, ymax = ymax)

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

  # extra room for the DGP overlay
  extra_space <- if (show_dgp) max(3, 0.25 * max_plot_count) else 0
  y_upper <- max_plot_count + extra_space

  # --- axis breaks (y) ---
  if (max_plot_count <= 10)      step_y <- 1
  else if (max_plot_count <= 20) step_y <- 2
  else if (max_plot_count <= 50) step_y <- 5
  else if (max_plot_count <= 100) step_y <- 10
  else                            step_y <- ceiling(max_plot_count / 10)

  breaks_y <- seq(0, max_plot_count, by = step_y)

  # --- x-axis limits & breaks ---
  rng_x <- range(x_vec)
  if (diff(rng_x) == 0) rng_x <- rng_x + c(-0.5, 0.5)

  # force the x-scale limits so the DGP axis matches EXACTLY
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

  # --- unit rectangles ---
  if (bars != "solid") {
    p <- p + geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = fill, color = "white", alpha = alpha
    )
  }

  # --- bar outlines or solids ---
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

  # --- axis labels ---
  x_lab <- if (show_dgp) {
    expression(atop("Parameter Estimate",
                    Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]))
  } else x_label

  # LEFT-JUSTIFY the x-axis title when show_dgp = TRUE
  base_theme <- theme_minimal() +
    theme(
      axis.line.x  = element_line(color = if (show_dgp) dgp_color else "black"),
      axis.line.y  = element_line(color = "black"),
      axis.text.x  = element_text(color = if (show_dgp) dgp_color else "black"),
      axis.title.x = element_text(hjust = 0,                 # <--- FIX 1
                                  color = if (show_dgp) dgp_color else "black")
    )

  p <- p +
    labs(x = x_lab, y = "count") +
    scale_y_continuous(limits = c(0, y_upper),
                       breaks = breaks_y) +
    scale_x_continuous(limits = x_limits,        # <--- FIX 3 (matching length)
                       breaks = breaks_x) +
    base_theme

  # --- DGP overlay ---
  if (show_dgp) {

    x_min <- x_limits[1]
    x_max <- x_limits[2]

    axis_y  <- max_plot_count + extra_space * 0.25
    eq_y    <- max_plot_count + extra_space * 0.50
    title_y <- max_plot_count + extra_space * 0.80

    # blue DGP axis
    p <- p + annotate(
      "segment",
      x = x_min, xend = x_max,
      y = axis_y, yend = axis_y,
      color = dgp_color, linewidth = 0.8
    )

    p <- p + annotate(
      "text", x = x_min, y = title_y,
      label = "Data Generating Process (DGP)",
      hjust = 0, vjust = 0,
      size = 4, color = dgp_color
    )

    p <- p + annotate(
      "text", x = x_min, y = eq_y,
      label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]",
      parse = TRUE,
      hjust = 0, vjust = 0.5,
      size = 4, color = dgp_color
    )

    # --- FIX 2: PROPER RED β1 BOX ---
    if (0 >= x_min && 0 <= x_max) {
      box_w <- diff(x_limits) * 0.02
      box_h <- extra_space * 0.18

      p <- p +
        annotate("rect",
                 xmin = 0 - box_w, xmax = 0 + box_w,
                 ymin = axis_y - box_h, ymax = axis_y + box_h,
                 color = "red3", fill = "white", alpha = 0.9, linewidth = 0.6) +
        annotate("text",
                 x = 0, y = axis_y,
                 label = "beta[1]",
                 parse = TRUE,
                 size = 4, color = "red3")
    }
  }

  p
}
