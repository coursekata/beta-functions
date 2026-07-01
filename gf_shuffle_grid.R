# deps: ggplot2, ggformula, gridExtra, mosaic (for shuffle()), magrittr
#
# Build a grid of plots: every panel except one shows y randomly shuffled
# against x (breaking any real relationship); exactly one panel shows the
# real, unshuffled data. This is the classic "spot the real data" exercise
# used to build intuition for randomization/permutation tests.
#
#   gf_shuffle_grid(Height ~ Sex, data = Fingers, plot = "jitter")
#
# Works for both categorical x (draws group-mean segments via gf_lm_cat() +
# gf_coef() b0/b1 arrows) and continuous x (draws a regression line via
# gf_lm() + gf_coef() rise-over-run).
#
# Parameters:
#   formula    - y ~ x formula (single variable on each side)
#   data       - data frame
#   plot       - "jitter" (default) or "point"
#   nrow, ncol - grid dimensions (default 3x3; capped at 25 panels total)
#   real_data  - which panel (1-indexed, filled row-major like grid.arrange)
#                shows the real data. Defaults to a random position each call
#                so students can't memorize a fixed spot — pass an explicit
#                value to control it (e.g. for a worked example).
#   show_model - overlay gf_lm_cat() or gf_lm() group means / regression line (default TRUE)
#   show_coef  - overlay gf_coef() coefficient annotations (default FALSE).
#                Only meaningful when show_model = TRUE; set both explicitly to get coef without model.
#   reveal     - if TRUE, label the real-data panel "real data" so it's
#                identifiable (default FALSE — keeps it a blind exercise)
#   width      - jitter width (only used when plot = "jitter")
#   color      - overlay color
#   seed       - optional seed for reproducible shuffles + real_data position
#   ...        - passed through to gf_jitter() / gf_point()

gf_shuffle_grid <- function(formula, data,
                            plot = c("jitter", "point"),
                            nrow = 3, ncol = 3,
                            real_data = NULL,
                            show_model = TRUE,
                            show_coef = FALSE,
                            reveal = FALSE,
                            width = 0.1,
                            color = "purple",
                            seed = NULL,
                            ...) {

  plot  <- match.arg(plot)
  total <- nrow * ncol

  if (total > 25) {
    stop("gf_shuffle_grid() supports at most 25 panels (e.g. 5x5). Reduce nrow/ncol.")
  }
  if (total < 2) {
    stop("nrow * ncol must be at least 2 (one real panel plus at least one shuffle).")
  }

  if (!is.null(seed)) set.seed(seed)

  if (is.null(real_data)) {
    real_data <- sample(seq_len(total), 1)
  } else if (real_data < 1 || real_data > total) {
    stop("real_data must be between 1 and nrow*ncol (", total, ").")
  }

  y_name <- all.vars(formula[[2]])
  x_name <- all.vars(formula[[3]])
  if (length(y_name) != 1 || length(x_name) != 1) {
    stop("gf_shuffle_grid() expects a simple y ~ x formula with one variable on each side.")
  }

  is_cat <- is.factor(data[[x_name]]) || is.character(data[[x_name]])

  panels <- vector("list", total)
  for (i in seq_len(total)) {
    is_real <- (i == real_data)
    label   <- if (reveal && is_real) "real data" else as.character(i)
    panels[[i]] <- .shuffle_grid_panel(
      data = data, y_name = y_name, x_name = x_name,
      is_real = is_real, is_cat = is_cat,
      plot_type = plot, width = width, color = color,
      show_model = show_model, show_coef = show_coef, label = label, ...
    )
  }

  gridExtra::grid.arrange(grobs = panels, nrow = nrow, ncol = ncol)
}


# ── helper ───────────────────────────────────────────────────────────────────

.shuffle_grid_panel <- function(data, y_name, x_name, is_real, is_cat,
                                plot_type, width, color, show_model, show_coef, label, ...) {

  plot_data <- data
  if (is_real) {
    y_col <- y_name
  } else {
    y_col <- ".shuffled_y"
    plot_data[[y_col]] <- mosaic::shuffle(data[[y_name]])
  }

  gformula  <- stats::as.formula(paste0("`", y_col, "` ~ `", x_name, "`"))
  geom_fun  <- if (plot_type == "jitter") ggformula::gf_jitter else ggformula::gf_point
  geom_args <- list(gformula, data = plot_data, size = 0.75)
  if (plot_type == "jitter") geom_args$width <- width
  geom_args <- c(geom_args, list(...))

  p <- do.call(geom_fun, geom_args)

  if (show_model) {
    if (is_cat) {
      p <- p %>% gf_lm_cat(color = color, linewidth = 1)
    } else {
      p <- p %>% ggformula::gf_lm(color = color)
    }
  }
  if (show_coef) {
    p <- p %>% gf_coef(color = color)
  }

  p +
    ggplot2::labs(title = label, x = NULL, y = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9),
      axis.text  = ggplot2::element_text(size = 7)
    )
}
