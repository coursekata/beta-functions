# Temporary self-contained version of gf_model() with binomial glm support.
# Source this file into a notebook, then use gf_model2().

# This function checks for required packages when called. It does not call library()
# inside the function because that can alter the user's search path. Instead, it
# uses explicit package::function calls where needed.

gf_model2 <- function(object, model, ...) {
  required_packages <- c("ggplot2", "ggformula", "rlang", "purrr", "glue")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "gf_model2() requires these packages: ",
      paste(missing_packages, collapse = ", "),
      ". Please install them before using gf_model2().",
      call. = FALSE
    )
  }

  args <- rlang::list2(...)

  if (!inherits(object, c("gg", "ggplot"))) {
    rlang::abort("`gf_model2()` needs to be layered on top of a plot.")
  }

  info <- list(layer = list())
  info$model <- fortify_model2(object, model)
  info$plot <- fortify_plot2(object, info$model)

  info$layer$args <- rlang::list2(...)
  info$layer$args$object <- object

  if (!is.null(info$layer$args$color)) {
    info$layer$args$colour <- info$layer$args$color
    info$layer$args$color <- NULL
  }

  missing_in_plot <- setdiff(info$model$terms, info$plot$variables)
  if (length(missing_in_plot) > 0) {
    rlang::abort(c(
      "The model you are trying to plot uses variables that do not exist in the plot",
      glue::glue("plot: {collapse2(unique(info$plot$variables))}"),
      glue::glue("model: {collapse2(info$model$terms)}"),
      glue::glue("missing in plot: {collapse2(missing_in_plot)}")
    ))
  }

  if (length(info$model$outcome) > 1) {
    rlang::abort(c(
      "There is only support for plotting models with one outcome variable at this time",
      glue::glue("detected outcomes: {collapse2(info$model$outcome)}")
    ))
  }

  if (info$model$outcome %in% info$plot$axes == FALSE) {
    rlang::abort(c(
      "The model outcome variable must be represented on the plot as one of the axes",
      glue::glue("model outcome: {info$model$outcome}"),
      glue::glue("plot axes: {collapse2(info$plot$axes)}")
    ))
  }

  if (!is.numeric(info$model$data[[info$model$outcome]])) {
    rlang::abort(c(
      "There is only support for plotting models with numeric outcome variables at this time",
      glue::glue("detected outcome type: {class(info$model$data[[info$model$outcome]])}")
    ))
  }

  if (is_binomial_glm2(info$model$fit)) {
    y <- info$model$data[[info$model$outcome]]
    if (!all(stats::na.omit(y) %in% c(0, 1))) {
      rlang::abort(c(
        "Binomial glm models must have an outcome coded as 0 and 1 to be plotted with `gf_model2()`.",
        glue::glue("model outcome: {info$model$outcome}")
      ))
    }
  }

  not_in_model <- info$plot$variables[info$plot$variables %in% info$model$terms == FALSE]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(info$layer$args[[aesthetic]])) {
      info$layer$args[[aesthetic]] <- ggplot2::GeomLine$default_aes[[aesthetic]]
    }
  }

  for (arg_name in names(info$layer$args)) {
    if (rlang::is_formula(info$layer$args[[arg_name]])) {
      var_name <- sub("^~", "", rlang::quo_name(info$layer$args[[arg_name]]))
      if (var_name %in% info$model$predictors == FALSE) {
        info$layer$args[[arg_name]] <- NULL
      }
    }
  }

  non_axis_predictor <- setdiff(info$model$predictors, info$plot$axes)
  if (length(non_axis_predictor) == 1) {
    info$layer$args$group <- name_to_frm2(non_axis_predictor)
  } else if (length(non_axis_predictor) > 1) {
    rlang::abort("Not sure how to plot a model with multiple variables mapped to aesthetic properties.")
  }

  if (
    length(info$model$predictors) == 0 ||
      (length(info$model$predictors) == 1 && info$model$predictors %in% info$plot$axes == FALSE)
  ) {
    if (info$plot$flipped) {
      info$layer$plotter <- ggformula::gf_vline
      info$layer$geom <- ggplot2::GeomVline
      info$layer$args$xintercept <- name_to_frm2(info$model$outcome)
    } else {
      info$layer$plotter <- ggformula::gf_hline
      info$layer$geom <- ggplot2::GeomHline
      info$layer$args$yintercept <- name_to_frm2(info$model$outcome)
    }
  } else {
    non_outcome_axis_data <- object$data[[info$plot$non_outcome_axis]]
    if (is.numeric(non_outcome_axis_data)) {
      info$layer$geom <- ggplot2::GeomLine
      info$layer$plotter <- ggformula::gf_line
    } else {
      info$layer$plotter <- ggformula::gf_errorbar
      info$layer$geom <- ggplot2::GeomErrorbar
      info$layer$args$width <- if_not_null2(info$layer$args$width, .4)

      if (info$plot$flipped) {
        info$layer$args$xmin <- name_to_frm2(info$model$outcome)
        info$layer$args$xmax <- info$layer$args$xmin
      } else {
        info$layer$args$ymin <- name_to_frm2(info$model$outcome)
        info$layer$args$ymax <- info$layer$args$ymin
      }
    }
  }

  info$layer$args$linewidth <- if_not_null2(
    info$layer$args$linewidth,
    if_not_null2(info$layer$args$size, 1)
  )

  remap <- info$plot$variables[info$plot$variables %in% info$model$predictors]
  remap <- remap[names(remap) %in% info$layer$geom$aesthetics()]
  remap <- remap[names(remap) %in% names(info$layer$args) == FALSE]
  info$layer$args[names(remap)] <- purrr::map(remap, name_to_frm2)

  if ("size" %in% names(info$plot$aesthetics)) {
    info$layer$args$linewidth <- name_to_frm2(info$plot$variables[["size"]])
  }

  if (
    is.null(info$layer$args$color) &&
      "color" %in% names(info$plot$aesthetics) == FALSE &&
      "fill" %in% names(info$plot$aesthetics)
  ) {
    info$layer$args$color <- name_to_frm2(info$plot$variables[["fill"]])
  }

  params <- list()
  for (term in c(info$model$predictors, info$plot$aesthetics)) {
    term_data <- object$data[[term]]
    if (term == info$model$outcome) {
      rlang::abort("How did you use the outcome as a predictor?")
    } else if (!is.numeric(term_data)) {
      if (is.logical(term_data)) {
        params[[term]] <- c(TRUE, FALSE)
      } else {
        params[[term]] <- levels(factor(term_data))
      }
    } else if (term %in% info$plot$axes) {
      rng <- range(term_data, na.rm = TRUE)
      len <- max(nrow(object$data), 80L)
      params[[term]] <- seq(rng[[1]], rng[[2]], length.out = len)
    } else {
      spread <- stats::sd(term_data, na.rm = TRUE)
      middle <- mean(term_data, na.rm = TRUE)
      params[[term]] <- c(middle - spread, middle, middle + spread)
    }
  }

  grid <- expand.grid(if (length(params)) params else list(dummy = 1))

  predict_args <- list(
    object = info$model$fit,
    newdata = grid
  )

  if (is_binomial_glm2(info$model$fit)) {
    predict_args$type <- "response"
  }

  grid[[info$model$outcome]] <- do.call(stats::predict, predict_args)
  info$layer$args$data <- grid

  do.call(info$layer$plotter, info$layer$args)
}

is_binomial_glm2 <- function(model) {
  inherits(model, "glm") && stats::family(model)$family == "binomial"
}

fortify_model2 <- function(object, model) {
  formula <- stats::formula(model)
  data <- if (inherits(model, "lm")) model$model else object$data
  fit <- if (inherits(model, "lm")) model else stats::lm(formula, data = data)
  terms <- sort(names(fit$model))
  predictors <- sort(setdiff(terms, deparse(rlang::f_lhs(formula))))
  outcome <- setdiff(terms, predictors)

  list(
    formula = formula,
    data = data,
    fit = fit,
    terms = terms,
    predictors = predictors,
    outcome = outcome
  )
}

fortify_plot2 <- function(object, fortified_model) {
  mapping <- object$mapping
  aesthetics <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- object$facet$vars()
  variables <- sort(c(purrr::map_chr(mapping, rlang::quo_name), facet = facets))
  axes <- variables[names(variables) %in% aesthetics == FALSE & variables %in% facets == FALSE]
  outcome_axis <- axes[axes %in% fortified_model$outcome]
  non_outcome_axis <- axes[axes %in% outcome_axis == FALSE]
  flipped <- names(outcome_axis) == "x"

  list(
    mapping = mapping,
    variables = variables,
    aesthetics = variables[aesthetics],
    facets = facets,
    axes = axes,
    outcome_axis = outcome_axis,
    non_outcome_axis = non_outcome_axis,
    flipped = flipped,
    env = object$plot_env
  )
}

collapse2 <- function(x) glue::glue_collapse(x, sep = ", ")
name_to_frm2 <- function(x) stats::formula(paste0("~", x))
if_not_null2 <- function(x, other) if (!is.null(x)) x else other
