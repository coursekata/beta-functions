# deps: ggplot2, rlang
#
# gf_reduce() and gf_square_reduce() visualize SS Model — the reduction in
# error achieved by a predictor compared to the empty model.
#
# The three square functions together decompose total variation:
#
#   gf_square_resid(empty_model)   → SS Total   (obs to grand mean)
#   gf_square_resid(complex_model) → SS Error   (obs to model predictions)
#   gf_square_reduce(complex_model)→ SS Model   (grand mean to model predictions)
#
# SS Total = SS Model + SS Error, so the squares visually add up.
#
# The connection to PRE: SS Model / SS Total = PRE. The squares drawn by
# gf_square_reduce() are the numerator; the squares drawn by
# gf_square_resid(empty_model) are the denominator.
#
# Usage:
#   empty_model   <- lm(Thumb ~ NULL,   data = Fingers)
#   complex_model <- lm(Thumb ~ Height, data = Fingers)
#
#   gf_point(Thumb ~ Height, data = Fingers) %>%
#     gf_model(complex_model) %>%
#     gf_reduce(complex_model)
#
#   gf_point(Thumb ~ Height, data = Fingers) %>%
#     gf_model(complex_model) %>%
#     gf_square_reduce(complex_model)


#' Add SS Model lines to a plot
#'
#' Draws a vertical line for each observation from the empty model prediction
#' (grand mean) to the complex model prediction. The length of each line is the
#' reduction in error that the predictor achieves for that observation.
#'
#' @param plot A ggformula plot object.
#' @param model A fitted \code{lm()} object (the complex model).
#' @param linewidth Line width. Default \code{0.2}.
#' @param ... Additional arguments passed to \code{geom_segment()} (e.g. \code{color}, \code{alpha}).
#' @return A ggplot object with SS Model lines added.
#' @export
gf_reduce <- function(plot, model, linewidth = 0.2, ...) {
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)

  y_fitted  <- stats::fitted(model)
  y_empty   <- mean(y_fitted)   # grand mean = empty model prediction for every point

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  x_loc     <- plot_data$x

  set.seed(rand_int)
  plot +
    ggplot2::geom_segment(
      ggplot2::aes(
        x    = x_loc,
        xend = x_loc,
        y    = y_empty,
        yend = y_fitted
      ),
      inherit.aes = TRUE,
      linewidth   = linewidth,
      ...
    )
}


#' Add SS Model squares to a plot
#'
#' Draws a square for each observation whose side length equals the reduction in
#' error achieved by the predictor — the distance from the grand mean (empty
#' model prediction) to the complex model prediction. The total area of all
#' squares equals SS Model, the numerator of PRE.
#'
#' @param plot A ggformula plot object.
#' @param model A fitted \code{lm()} object (the complex model).
#' @param aspect Aspect ratio correction for proper square scaling. Default \code{4/6}.
#' @param alpha Fill transparency. Default \code{0.1}.
#' @param linewidth Border line width of each square. Default \code{0.25}.
#' @param ... Additional arguments passed to \code{geom_polygon()} (e.g. \code{color}, \code{fill}).
#' @return A ggplot object with SS Model squares added.
#' @export
gf_square_reduce <- function(plot, model, aspect = 4 / 6, alpha = 0.1, linewidth = 0.25, ...) {
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)

  y_fitted  <- stats::fitted(model)
  y_empty   <- mean(y_fitted)

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  x_loc     <- plot_data$x

  plot_layout  <- ggplot2::ggplot_build(plot)$layout
  panel_params <- plot_layout$panel_params[[1]]
  x_range      <- panel_params$x.range
  y_range      <- panel_params$y.range

  range_ratio  <- (x_range[2] - x_range[1]) / (y_range[2] - y_range[1])
  dir          <- ifelse(x_loc > mean(x_range), -1, 1)
  reduction    <- y_fitted - y_empty          # signed: positive if model predicts above mean
  side_length  <- abs(reduction) * aspect * range_ratio
  x_opp        <- x_loc + dir * side_length

  squares_data <- do.call(rbind, lapply(seq_along(x_loc), function(i) {
    data.frame(
      x  = c(x_loc[i], x_opp[i],  x_opp[i],  x_loc[i]),
      y  = c(y_empty,  y_empty,    y_fitted[i], y_fitted[i]),
      id = i
    )
  }))

  set.seed(rand_int)
  plot +
    ggplot2::geom_polygon(
      data        = squares_data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      inherit.aes = FALSE,
      alpha       = alpha,
      linewidth   = linewidth,
      ...
    )
}

#' @rdname gf_square_reduce
#' @export
gf_squareduce <- function(plot, model, aspect = 4 / 6, alpha = 0.1, linewidth = 0.25, ...) {
  gf_square_reduce(plot, model, aspect = aspect, alpha = alpha, linewidth = linewidth, ...)
}
