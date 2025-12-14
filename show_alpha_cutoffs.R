# show_alpha_cutoffs.R
# A single pipe-friendly function to add arrow markers at alpha cutoff points
# For use with coursekata package and ggformula histograms

#' Add arrow markers at alpha cutoff points on a histogram
#'
#' Pipe this function after a gf_histogram() call to add downward-pointing
#' triangular markers at the empirical quantile cutoffs. The function
#' automatically extracts both the variable and the prop value from the
#' middle() call in your fill aesthetic.
#'
#' @param plot A ggplot object (typically a histogram created with gf_histogram
#'        with fill = ~middle(...))
#' @param color Color of the arrow markers (default: "#1e3a8a" - dark blue)
#' @param size Size of the arrow markers (default: 4)
#' @param labels Logical; whether to add text annotations explaining the cutoffs
#'        (default: FALSE)
#'
#' @return A ggplot object with arrow markers added
#'
#' @examples
#' library(coursekata)
#'
#' # Basic usage - prop is automatically extracted from middle()
#' gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95)) %>%
#'   show_alpha_cutoffs()
#'
#' # With explanatory labels
#' gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95)) %>%
#'   show_alpha_cutoffs(labels = TRUE)
#'
#' # Custom styling
#' gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95)) %>%
#'   show_alpha_cutoffs(color = "darkred", size = 5)
#'
show_alpha_cutoffs <- function(plot, color = "#1e3a8a", size = 4, labels = FALSE) {

  # Extract the fill aesthetic to find the middle() call
  fill_expr <- NULL

  # Try plot's main mapping first
  if (!is.null(plot$mapping$fill)) {
    fill_expr <- rlang::quo_get_expr(plot$mapping$fill)
  }

  # Try first layer's mapping
  if (is.null(fill_expr) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]
    if (!is.null(layer$mapping$fill)) {
      fill_expr <- rlang::quo_get_expr(layer$mapping$fill)
    }
  }

  if (is.null(fill_expr)) {
    stop("Could not find fill aesthetic. Make sure you have fill = ~middle(...) in your histogram.")
  }

  # Parse the middle() call to extract prop
  # The expression should be something like: middle(Thumb, 0.95)

  # Check if it's a call to middle or middle_arrows
  if (!is.call(fill_expr) || !(as.character(fill_expr[[1]]) %in% c("middle", "middle_arrows"))) {
    stop("Expected fill = ~middle(...). Found: ", deparse(fill_expr))
  }

  # Extract the prop argument (second argument to middle())
  if (length(fill_expr) < 3) {
    stop("middle() requires at least 2 arguments: middle(variable, prop)")
  }

  prop <- eval(fill_expr[[3]])

  if (!is.numeric(prop) || prop <= 0 || prop >= 1) {
    stop("prop must be a number between 0 and 1. Found: ", prop)
  }

  # Extract the x variable data
  x_data <- NULL

  # Try to get from the plot's main mapping
  if (!is.null(plot$mapping$x)) {
    x_var <- rlang::as_name(plot$mapping$x)
    if (!is.null(plot$data) && x_var %in% names(plot$data)) {
      x_data <- plot$data[[x_var]]
    }
  }

  # If not found, try from the first layer
  if (is.null(x_data) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]

    if (!is.null(layer$mapping$x)) {
      x_var <- rlang::as_name(layer$mapping$x)
      layer_data <- if (!is.null(layer$data) && is.data.frame(layer$data)) {
        layer$data
      } else if (!is.null(plot$data)) {
        plot$data
      } else {
        NULL
      }
      if (!is.null(layer_data) && x_var %in% names(layer_data)) {
        x_data <- layer_data[[x_var]]
      }
    }
  }

  if (is.null(x_data)) {
    stop("Could not extract variable from plot. Make sure you're piping from a gf_histogram() call.")
  }

  # Calculate cutoffs to match middle() behavior exactly
  # For middle 95% of n=1000: exclude positions 1-25 and 976-1000
  # The boundary values are at positions 26 and 975
  x_clean <- x_data[!is.na(x_data)]
  x_sorted <- sort(x_clean)
  n <- length(x_sorted)
  
  alpha <- 1 - prop
  # Lower cutoff: first value IN the middle (position after excluded lower tail)
  lower_idx <- floor(alpha / 2 * n) + 1
  # Upper cutoff: last value IN the middle
  upper_idx <- ceiling((1 - alpha / 2) * n)
  
  # Clamp to valid indices
  lower_idx <- max(1, min(n, lower_idx))
  upper_idx <- max(1, min(n, upper_idx))
  
  cutoffs <- c(x_sorted[lower_idx], x_sorted[upper_idx])

  plot <- plot +
    ggplot2::annotate("point", x = cutoffs[1], y = 0,
                      shape = 25, size = size, fill = color, color = color) +
    ggplot2::annotate("point", x = cutoffs[2], y = 0,
                      shape = 25, size = size, fill = color, color = color)
  
  # Add explanatory labels if requested
  if (labels) {
    # Calculate positions for labels
    # Build the plot to get actual y-axis range
    plot_built <- ggplot2::ggplot_build(plot)
    y_range <- plot_built$layout$panel_params[[1]]$y.range
    if (is.null(y_range)) y_range <- c(0, 30)  # fallback
    label_y <- y_range[2] * 0.15  # Position labels at 15% of max height
    
    # Calculate alpha/2 for the label text
    alpha <- 1 - prop
    tail_prop <- alpha / 2
    
    # Format the proportion nicely
    tail_label <- if (tail_prop == 0.025) ".025" 
                  else if (tail_prop == 0.05) ".05"
                  else if (tail_prop == 0.005) ".005"
                  else format(tail_prop, digits = 3)
    
    # X range for positioning curved arrows
    x_range <- range(x_clean)
    x_span <- x_range[2] - x_range[1]
    
    # Position labels centered between cutoff and edge of data
    left_label_x <- (x_range[1] + cutoffs[1]) / 2
    right_label_x <- (cutoffs[2] + x_range[2]) / 2
    
    plot <- plot +
      # Left side: label in left margin, thin line to cutoff marker
      ggplot2::annotate("text",
                        x = left_label_x,
                        y = label_y * 2.0,
                        label = paste0(tail_label, " of\nvalues below"),
                        hjust = 0.5, vjust = 0, size = 3.2, color = color,
                        fontface = "italic") +
      ggplot2::annotate("segment", 
                        x = left_label_x,
                        xend = cutoffs[1],
                        y = label_y * 1.8, 
                        yend = 0,
                        linewidth = 0.3,
                        color = color) +
      # Right side: label in right margin, thin line to cutoff marker
      ggplot2::annotate("text",
                        x = right_label_x,
                        y = label_y * 2.0,
                        label = paste0(tail_label, " of\nvalues above"),
                        hjust = 0.5, vjust = 0, size = 3.2, color = color,
                        fontface = "italic") +
      ggplot2::annotate("segment",
                        x = right_label_x,
                        xend = cutoffs[2],
                        y = label_y * 1.8,
                        yend = 0,
                        linewidth = 0.3,
                        color = color)
  }
  
  plot
}