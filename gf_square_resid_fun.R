#' Add Squared Residual Visualization to a Plot from a Function
#'
#' Draws squared residuals (as polygons) between observed points and predicted
#' values computed by a user-supplied function of x.
#'
#' @param plot A ggformula/ggplot object, typically created with gf_point() or gf_jitter().
#' @param fun  A function that takes a numeric vector x and returns predicted y.
#'             Example: function(x) 2 + 5*x
#' @param aspect A numeric value controlling the square's aspect ratio. Default is 4/6.
#' @param alpha Transparency of the filled squares. Default 0.1.
#' @param ... Additional aesthetics passed to geom_polygon() (e.g., color, fill, linetype).
#'
#' @return A ggplot object with squared residuals added.
#' @export
#'
#' @examples
#' library(ggformula)
#' set.seed(1)
#' df <- data.frame(X = rnorm(50), Y = 2 + 5*rnorm(50))
#' my_fun <- function(x) 2 + 5*x
#'
#' gf_point(Y ~ X, data = df) %>%
#'   gf_function(my_fun) %>%
#'   gf_square_resid_fun(my_fun, color = "red", alpha = 0.3)
gf_square_resid_fun <- function(plot, fun, aspect = 4/6, alpha = 0.1, ...) {
  # Handles random jitter
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)
  
  # Access the x and y coordinates used in the plot
  plot_data <- ggplot_build(plot)$data[[1]]
  x_loc <- plot_data$x
  y_loc <- plot_data$y
  
  # Compute predicted values and residuals
  set.seed(rand_int)
  y_hat <- fun(x_loc)
  residual <- y_loc - y_hat
  
  # Access the range of x and y used in the panel
  plot_layout <- ggplot_build(plot)$layout
  panel_params <- plot_layout$panel_params[[1]]
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range
  
  # Compute ratio for proper aspect scaling
  range_ratio <- (x_range[2] - x_range[1]) / (y_range[2] - y_range[1])
  dir <- ifelse(x_loc > mean(x_range), -1, 1)
  adj_side <- x_loc + dir * abs(residual * aspect * range_ratio)
  
  # Build polygons for each residual square
  squares_data <- do.call(rbind, lapply(seq_along(x_loc), function(i) {
    resid_side <- x_loc[i]
    top <- y_hat[i]
    bottom <- y_loc[i]
    opp_side <- adj_side[i]
    
    data.frame(
      x = c(resid_side, opp_side, opp_side, resid_side),
      y = c(bottom, bottom, top, top),
      id = i
    )
  }))
  
  # Add polygons
  set.seed(rand_int)
  plot +
    ggplot2::geom_polygon(
      data = squares_data,
      ggplot2::aes(x = x, y = y, group = id),
      inherit.aes = FALSE,
      alpha = alpha,
      ...
    )
}

# Alias
gf_squaresid_fun <- gf_square_resid_fun
