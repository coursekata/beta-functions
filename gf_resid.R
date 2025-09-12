#' Add Residual Lines to a Plot
#'
#' This function adds vertical lines representing residuals from a linear model to a ggformula plot. 
#' The residuals are drawn from the observed data points to the predicted values from the model.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()`.
#' @param linewidth A numeric value specifying the width of the residual lines. Default is `0.2`.
#' @param ... Additional aesthetics passed to `geom_segment()`, such as `color`, `alpha`, `linetype`.
#'
#' @return A ggplot object with residual lines added.
#'
#' @export
#' @examples
#' gf_point(Thumb ~ Height, data = Fingers) %>%
#'   gf_model(Height_model) %>%
#'   gf_resid(Height_model, color = "red", alpha = 0.5)
gf_resid <- function(plot, model, linewidth = 0.2, ...) {
  # Handles random jitter
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)

  # Get model predictions and residuals and assign them to the model data
  model_data <- model$model
  model_data$prediction <- predict(model)
  model_data$residual <- resid(model)
  
  # Access the x and y coordinates used in the plot
  plot_data <- ggplot_build(plot)$data[[1]]
  x_loc <- plot_data$x
  y_loc <- plot_data$y

  # Ensures same jitter as the x and y coord from plot
  set.seed(rand_int)
  plot +
    geom_segment(aes(
      x = x_loc,
      y = model_data$prediction,
      xend = x_loc,
      yend = y_loc
    ),
    inherit.aes = TRUE,
    linewidth = linewidth,
    ...
    )
}
