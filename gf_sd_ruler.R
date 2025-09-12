# deps:
# library(ggplot2); library(magrittr)

# Add a vertical SD "ruler" to an existing ggplot/ggformula plot.
# - p: an existing ggplot (what %>% will pass in)
# - y: the y-variable (bare name or "string"); defaults to the plot's mapped y if omitted
# - data: dataset (defaults to p$data)
# - x: x-variable to find a placement; defaults to the plot's mapped x or row index
# - where: where on the x-axis to place the ruler ("middle", "mean", "median")
# - color/size/...: passed to geom_segment
gf_sd_ruler <- function(p, y = NULL, data = NULL, x = NULL,
                        where = c("middle","mean","median"),
                        color = "red", size = 0.8, ...) {
  where <- match.arg(where)
  if (is.null(data)) data <- p$data

  # infer y if not supplied
  if (is.null(y)) {
    if (is.null(p$mapping$y)) stop("Can't infer y; please pass y explicitly.")
    y_name <- rlang::as_name(p$mapping$y)
  } else {
    y_name <- if (is.character(y)) y else deparse(substitute(y))
  }

  y_vals <- data[[y_name]]
  m <- mean(y_vals, na.rm = TRUE)
  s <- sd(y_vals, na.rm = TRUE)

  # infer x for placement
  if (is.null(x)) {
    if (!is.null(p$mapping$x)) {
      x_name <- rlang::as_name(p$mapping$x)
      x_vals <- data[[x_name]]
    } else {
      x_vals <- seq_along(y_vals)
    }
  } else {
    x_name <- if (is.character(x)) x else deparse(substitute(x))
    x_vals <- data[[x_name]]
  }

  x0 <- switch(where,
               middle = (min(x_vals, na.rm = TRUE) + max(x_vals, na.rm = TRUE)) / 2,
               mean   = mean(x_vals, na.rm = TRUE),
               median = stats::median(x_vals, na.rm = TRUE))

  seg <- data.frame(x = x0, xend = x0, y = m, yend = m + s)

  p +
    ggplot2::geom_segment(
      data = seg,
      mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = color,
      size = size,
      ...
    )
}
