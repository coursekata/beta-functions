library(glue)
library(rlang)
#
#' Add a model to a plot
#'
#' When teaching about regression it can be useful to visualize the data as a point plot with the
#' outcome on the y-axis and the explanatory variable on the x-axis. For regression models, this is
#' most easily achieved by calling [`ggformula::gf_lm()`], with empty models
#' [`ggformula::gf_hline()`] using the mean, and a more complicated call to
#' [`ggformula::gf_segment()`] for group models. This function simplifies this
#' by making a guess about what kind of model you are plotting (empty/null, regression, group) and
#' then making the appropriate plot layer for it.
#'
#' This function works with linear models that have a numeric outcome measure. It also supports
#' binomial logistic regression models fit with [`glm()`] when the outcome is numeric and coded 0/1.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model A linear model fit by either [`lm()`] or [`aov()`], or a binomial logistic regression
#'   model fit by [`glm()`] with a numeric 0/1 outcome.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'
#' @return a gg object (a plot layer) that can be added to a plot.
#'
#' @export
gf_model2 <- function(object, model, ...) {
  args <- rlang::list2(...)

  if (!inherits(object, c("gg", "ggplot"))) {
    abort("`gf_model2()` needs to be layered on top of a plot.")
  }

  info <- list(layer = list())
  info$model <- fortify_model(object, model)
  info$plot <- fortify_plot(object, info$model)

  info$layer$args <- rlang::list2(...)
  info$layer$args$object <- object

  # standardize user-defined aesthetics
  if (!is.null(info$layer$args$color)) {
    info$layer$args$colour <- info$layer$args$color
    info$layer$args$color <- NULL
  }

  missing_in_plot <- setdiff(info$model$terms, info$plot$variables)
  if (length(missing_in_plot) > 0) {
    abort(c(
      "The model you are trying to plot uses variables that do not exist in the plot",
      glue("plot: {collapse(unique(info$plot$variables))}"),
      glue("model: {collapse(info$model$terms)}"),
      glue("missing in plot: {collapse(missing_in_plot)}")
    ))
  }

  if (length(info$model$outcome) > 1) {
    abort(c(
      "There is only support for plotting models with one outcome variable at this time",
      glue("detected outcomes: {info$model$outcome}")
    ))
  }

  if (info$model$outcome %in% info$plot$axes == FALSE) {
    abort(c(
      "The model outcome variable must be represented on the plot as one of the axes",
      glue("model outcome: {info$model$outcome}"),
      glue("plot axes: {collapse(info$plot$axes)}")
    ))
  }

  if (!is.numeric(info$model$data[[info$model$outcome]])) {
    abort(c(
      "There is only support for plotting models with numeric outcome variables at this time",
      glue("detected outcome type: {class(info$model$data[[info$model$outcome]])}")
    ))
  }

  if (is_binomial_glm(info$model$fit)) {
    y <- info$model$data[[info$model$outcome]]
    if (!all(stats::na.omit(y) %in% c(0, 1))) {
      abort(c(
        "Binomial glm models must have an outcome coded as 0 and 1 to be plotted with `gf_model()`.",
        glue("model outcome: {info$model$outcome}")
      ))
    }
  }

  # if the plot has an aesthetic mapped, but the model doesn't have that variable, unset it
  not_in_model <- info$plot$variables[info$plot$variables %in% info$model$terms == FALSE]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(info$layer$args[[aesthetic]])) {
      info$layer$args[[aesthetic]] <- ggplot2::GeomLine$default_aes[[aesthetic]]
    }
  }

  # only allow mapped aesthetics that are predictors in the model
  for (arg_name in names(info$layer$args)) {
    if (is_formula(info$layer$args[[arg_name]])) {
      var_name <- sub("^~", "", quo_name(info$layer$args[[arg_name]]))
      if (var_name %in% info$model$predictors == FALSE) {
        info$layer$args[[arg_name]] <- NULL
      }
    }
  }

  # find grouping variable
  non_axis_predictor <- setdiff(info$model$predictors, info$plot$axes)
  if (length(non_axis_predictor) == 1) {
    info$layer$args$group <- name_to_frm(non_axis_predictor)
  } else if (length(non_axis_predictor) > 1) {
    abort("Not sure how to plot a model with multiple variables mapped to aesthetic properties.")
  }

  # determine what to plot with ------------
  if (
    length(info$model$predictors) == 0 ||
      (length(info$model$predictors) == 1 && info$model$predictors %in% info$plot$axes == FALSE)
  ) {
    if (info$plot$flipped) {
      info$layer$plotter <- ggformula::gf_vline
      info$layer$geom <- ggplot2::GeomVline
      info$layer$args$xintercept <- name_to_frm(info$model$outcome)
    } else {
      info$layer$plotter <- ggformula::gf_hline
      info$layer$geom <- ggplot2::GeomHline
      info$layer$args$yintercept <- name_to_frm(info$model$outcome)
    }
  } else {
    non_outcome_axis_data <- object$data[[info$plot$non_outcome_axis]]
    if (is.numeric(non_outcome_axis_data)) {
      info$layer$geom <- ggplot2::GeomLine
      info$layer$plotter <- ggformula::gf_line
    } else {
      info$layer$plotter <- ggformula::gf_errorbar
      info$layer$geom <- ggplot2::GeomErrorbar
      info$layer$args$width <- if_not_null(info$layer$args$width, .4)

      if (info$plot$flipped) {
        info$layer$args$xmin <- name_to_frm(info$model$outcome)
        info$layer$args$xmax <- info$layer$args$xmin
      } else {
        info$layer$args$ymin <- name_to_frm(info$model$outcome)
        info$layer$args$ymax <- info$layer$args$ymin
      }
    }
  }

  # translate dot size to linewidth if needed
  info$layer$args$linewidth <- if_not_null(
    info$layer$args$linewidth,
    if_not_null(info$layer$args$size, 1)
  )

  # re-map dynamic aesthetics from previous layers if they are predictors in the model
  remap <- info$plot$variables[info$plot$variables %in% info$model$predictors]
  remap <- remap[names(remap) %in% info$layer$geom$aesthetics()]
  remap <- remap[names(remap) %in% names(info$layer$args) == FALSE]
  info$layer$args[names(remap)] <- purrr::map(remap, name_to_frm)

  if ("size" %in% names(info$plot$aesthetics)) {
    info$layer$args$linewidth <- name_to_frm(info$plot$variables[["size"]])
  }

  if (
    is.null(info$layer$args$color) &&
      "color" %in% names(info$plot$aesthetics) == FALSE &&
      "fill" %in% names(info$plot$aesthetics)
  ) {
    info$layer$args$color <- name_to_frm(info$plot$variables[["fill"]])
  }

  # build data grid ----------------
  params <- list()
  for (term in c(info$model$predictors, info$plot$aesthetics)) {
    term_data <- object$data[[term]]
    if (term == info$model$outcome) {
      abort("How did you use the outcome as a predictor?")
    } else if (!is.numeric(term_data)) {
      if (is.logical(term_data)) {
        params[[term]] <- c(TRUE, FALSE)
      } else {
        params[[term]] <- levels(factor(term_data))
      }
    } else if (term %in% info$plot$axes) {
      rng <- range(term_data)
      len <- max(nrow(object$data), 80L)
      params[[term]] <- seq(rng[[1]], rng[[2]], length.out = len)
    } else {
      spread <- stats::sd(term_data, na.rm = TRUE)
      middle <- mean(term_data, na.rm = TRUE)
      params[[term]] <- c(middle - spread, middle, middle + spread)
    }
  }

  # predict y values using the grid
  grid <- expand.grid(if (length(params)) params else list(dummy = 1))

  predict_args <- list(
    object = info$model$fit,
    newdata = grid
  )

  if (is_binomial_glm(info$model$fit)) {
    predict_args$type <- "response"
  }

  grid[[info$model$outcome]] <- do.call(stats::predict, predict_args)

  info$layer$args$data <- grid

  do.call(info$layer$plotter, info$layer$args)
}

is_binomial_glm <- function(model) {
  inherits(model, "glm") &&
    family(model)$family == "binomial"
}
