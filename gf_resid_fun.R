#' Add Residual Lines to a Plot from a Function
#'
#' Draws vertical residual lines from observed points to y-hat values computed
#' by a user-supplied function of x (e.g., the function you just plotted with
#' gf_function()).
#'
#' @param plot A ggformula/ggplot object, typically created with gf_point() or gf_jitter().
#' @param fun  A function that takes a numeric vector x and returns predicted y.
#'             Example: function(x) 2 + 5*x (or my_fun <- function(x) 2 + 5*x).
#' @param linewidth Numeric width of the residual lines. Default 0.2.
#' @param ... Additional aesthetics passed to ggplot2::geom_segment(), e.g., color, alpha, linetype.
#'
#' @return A ggplot object with residual segments added.
#' @export
#'
#' @examples
#' library(ggformula)
#' set.seed(1)
#' df <- data.frame(X = rnorm(100), Y = 2 + 5*rnorm(100))
#' my_function <- function(x) 2 + 5*x
#'
#' gf_point(Y ~ X, data = df) %>%
#'   gf_function(my_function) %>%
#'   gf_resid_from_function(my_function, color = "red", alpha = 0.5)
gf_resid_fun <- function(plot, fun, linewidth = 0.2, ...) {
  # Handles random jitter
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)
  
  # Access the x and y coordinates used in the plot
  plot_data <- ggplot_build(plot)$data[[1]]
  x_loc <- plot_data$x
  y_loc <- plot_data$y
  
  # Compute predicted values at those x positions
  set.seed(rand_int)
  y_hat <- fun(x_loc)
  
  # Add vertical residual lines
  plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_loc, y = y_hat, xend = x_loc, yend = y_loc),
      inherit.aes = TRUE,
      linewidth = linewidth,
      ...
    )
}
