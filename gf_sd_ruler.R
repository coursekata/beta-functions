# deps:
# library(ggplot2); library(magrittr)

# Add a vertical SD "ruler" to an existing ggplot/ggformula plot.
# - p: an existing ggplot (what %>% will pass in)
# - y: the y-variable (bare name or "string"); defaults to the plot's mapped y if omitted
# - data: dataset (defaults to p$data)
# - x: x-variable to find a placement; defaults to the plot's mapped x or row index
# - where: where on the x-axis to place the ruler ("middle", "mean", "median")
# - color/size/...: passed to geom_segment
# Works for both numeric x (scatter) and categorical x (jitter).

# Examples
# gf_jitter(Thumb ~ Height, data = Fingers) %>%
#   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#   gf_sd_ruler()

# gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
#   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#   gf_sd_ruler()

# gf_jitter(Thumb ~ RaceEthnic, data = Fingers, width = .1) %>%
#   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#   gf_sd_ruler()


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
  # if categorical, convert to integer category positions: 1,2,3,...
  if (is.null(x)) {
    if (!is.null(p$mapping$x)) {
      x_name <- rlang::as_name(p$mapping$x)
      x_vals_raw <- data[[x_name]]
    } else {
      x_vals_raw <- seq_along(y_vals)
    }
  } else {
    x_name <- if (is.character(x)) x else deparse(substitute(x))
    x_vals_raw <- data[[x_name]]
  }
  
  # turn categorical x into numeric positions
  x_vals <- x_vals_raw
  if (!is.numeric(x_vals)) {
    if (is.factor(x_vals)) {
      # use existing factor levels order
      x_vals <- as.numeric(x_vals)
    } else {
      # character or other -> factor in order of appearance
      x_vals <- as.numeric(factor(x_vals, levels = unique(x_vals)))
    }
  }
  
  # compute placement
  # for jitter plots with K groups, this centers at (1+K)/2 by default
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
