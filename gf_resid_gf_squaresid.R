# deps: ggplot2, lifecycle
#
# WORKING COPY of coursekata-r R/gf_resid_gf_squaresid.R (graduated 2026-03-09,
# see GRADUATED.md) brought back into beta to develop the jitter-alignment fix
# alongside gf_reduce.R before opening a PR to coursekata-r.
#
# Sourcing this file masks the package versions of gf_resid() and
# gf_square_resid(), so chains mixing these with gf_reduce()/gf_square_reduce()
# can be tested entirely from this repo. Diagnosis and failing chains:
# tests/test_resid_square_alignment.ipynb
#
# CHANGE vs coursekata-r: the sample()/set.seed() jitter bracket is replaced
# by .freeze_jitter(), which pins a fixed seed on the plot's jitter layer so
# every build draws identical dot positions. In the coursekata-r PR this
# becomes an internal helper.

# ── .freeze_jitter ────────────────────────────────────────────────────────────
# Embedded copy — canonical version and full rationale in freeze_plot.R.
# Pins every unseeded PositionJitter layer to a fixed seed (in place; ggproto
# objects are environments), so all builds of the plot draw the same jitter.
# No-op for already-seeded layers and non-jitter plots; never calls set.seed().
.freeze_jitter <- function(plot) {
  for (l in plot$layers) {
    pos <- l$position
    if (inherits(pos, "PositionJitter") && !isTRUE(is.finite(pos$seed))) {
      pos$seed <- sample.int(.Machine$integer.max, 1L)
    }
  }
  plot
}

#' Add Residual Lines to a Plot
#'
#' This function adds vertical lines representing residuals from a linear model to a ggformula plot.
#' The residuals are drawn from the observed data points to the predicted values from the model.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()`.
#' @param linewidth A numeric value specifying the width of the residual lines. Default is `0.2`.
#' @param ... Additional aesthetics passed to `geom_segment()`, such as `color`, `alpha`,
#'   `linetype`.
#'
#' @return A ggplot object with residual lines added.
#'
#' @export
#' @examples
#' Height_model <- lm(Thumb ~ Height, data = Fingers)
#' gf_point(Thumb ~ Height, data = Fingers) %>%
#'   gf_model(Height_model) %>%
#'   gf_resid(Height_model, color = "red", alpha = 0.5)
gf_resid <- function(plot, model, linewidth = 0.2, ...) {
  # Pin the jitter so every build of this plot draws the same dot positions
  plot <- .freeze_jitter(plot)

  # Get model predictions and residuals and assign them to the model data
  model_data <- model$model
  model_data$prediction <- stats::predict(model)
  model_data$residual <- stats::resid(model)

  # Access the x and y coordinates used in the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  x_loc <- plot_data$x
  y_loc <- plot_data$y

  plot +
    ggplot2::geom_segment(
      ggplot2::aes(
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

#' Add Squared Residual Visualization to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function adds squared residual representations to a ggformula plot, illustrating
#' squared error as a polygon. The function dynamically adjusts the aspect ratio to ensure
#' proper scaling of squares.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()`.
#' @param aspect A numeric value controlling the square's aspect ratio. Default is `4/6`.
#' @param alpha A numeric value specifying the transparency of the square's fill. Default is `0.1`.
#' @param ... Additional aesthetics passed to `geom_polygon()`, such as `color` and `fill`.
#'
#' @return A ggplot object with squared residuals added.
#'
#' @export
#' @examples
#' Height_model <- lm(Thumb ~ Height, data = Fingers)
#' gf_point(Thumb ~ Height, data = Fingers) %>%
#'   gf_model(Height_model) %>%
#'   gf_square_resid(Height_model, color = "blue", alpha = 0.5)
gf_square_resid <- function(plot, model, aspect = 4 / 6, alpha = 0.1, ...) {
  lifecycle::signal_stage("experimental", "gf_square_resid()")

  # Pin the jitter so every build of this plot draws the same dot positions
  plot <- .freeze_jitter(plot)

  # Get model predictions and residuals and assign them to the model data
  model_data <- model$model
  model_data$prediction <- stats::predict(model)
  model_data$residual <- stats::resid(model)

  # Access the x and y coordinates used in the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  model_data$x_loc <- plot_data$x
  model_data$y_loc <- plot_data$y

  # Access the range of x and y used in the panel
  plot_layout <- ggplot2::ggplot_build(plot)$layout
  panel_params <- plot_layout$panel_params[[1]]
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range

  # Compute ratio for proper aspect scaling
  range_ratio <- (x_range[2] - x_range[1]) / (y_range[2] - y_range[1])
  model_data$dir <- ifelse(model_data$x_loc > mean(x_range), -1, 1)
  side_length <- abs(model_data$residual) * aspect * range_ratio
  model_data$adj_side <- model_data$x_loc + model_data$dir * side_length

  # Create a dataframe for plotting polygons
  squares_data <- do.call(rbind, lapply(seq_len(nrow(model_data)), function(i) {
    resid_side <- model_data$x_loc[i]
    top <- model_data$prediction[i]
    bottom <- model_data$y_loc[i]
    opp_side <- model_data$adj_side[i]

    data.frame(
      x = c(resid_side, opp_side, opp_side, resid_side),
      y = c(bottom, bottom, top, top),
      id = i # Unique identifier for each square
    )
  }))

  plot +
    ggplot2::geom_polygon(
      data = squares_data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      inherit.aes = FALSE,
      alpha = alpha,
      ...
    )
}

#' @rdname gf_square_resid
#' @description
#' `gf_squaresid()` was renamed to [gf_square_resid()] for naming consistency
#' and is now deprecated.
#' @export
gf_squaresid <- function(plot, model, aspect = 4 / 6, alpha = 0.1, ...) {
  lifecycle::deprecate_warn("0.20.0", "gf_squaresid()", "gf_square_resid()")
  gf_square_resid(plot, model, aspect = aspect, alpha = alpha, ...)
}
