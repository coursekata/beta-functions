add_sd_ruler <- function(plot, outcome, 
                         legend_x = NULL, legend_y = NULL, 
                         ruler = "show", 
                         legend = "show") {
  outcome_quo <- rlang::enquo(outcome)

  # Try to evaluate outcome inside plot$data, otherwise from global env
  outcome_vals <- tryCatch(
    rlang::eval_tidy(outcome_quo, data = plot$data),
    error = function(e) rlang::eval_tidy(outcome_quo, data = rlang::caller_env())
  )

  m <- mean(outcome_vals, na.rm = TRUE)
  sd_val <- sd(outcome_vals, na.rm = TRUE)

  mapping <- plot$mapping
  outcome_name <- rlang::as_name(outcome_quo)

  # Determine if outcome is mapped to x or y
  is_x <- tryCatch(rlang::as_name(mapping$x) == outcome_name, error = function(e) FALSE)
  is_y <- tryCatch(rlang::as_name(mapping$y) == outcome_name, error = function(e) FALSE)

  # Fallback if uncertain
  if (!is_x && !is_y) {
    warning("Could not determine if outcome is on x or y axis. Assuming y-axis.")
    is_y <- TRUE
  }

  # Add SD ruler segment
  if (ruler == "show") {
    if (is_x) {
      segment_df <- data.frame(x = m, xend = m + sd_val, y = 0, yend = 0)
      plot <- plot +
        geom_segment(data = segment_df,
                     aes(x = x, xend = xend, y = y, yend = yend),
                     inherit.aes = FALSE,
                     color = "red", linewidth = 2)
    } else {
      segment_df <- data.frame(x = 0, xend = 0, y = m, yend = m + sd_val)
      plot <- plot +
        geom_segment(data = segment_df,
                     aes(x = x, xend = xend, y = y, yend = yend),
                     inherit.aes = FALSE,
                     color = "red", linewidth = 2)
    }
  }

  # Add SD legend if requested
  if (legend == "show" && !is.null(legend_x) && !is.null(legend_y)) {
    label_df <- data.frame(
      x = legend_x,
      y = legend_y,
      label = paste0("SD = ", round(sd_val, 2))
    )
    plot <- plot +
      geom_label(data = label_df,
                 aes(x = x, y = y, label = label),
                 inherit.aes = FALSE,
                 color = "red",
                 fill = NA,
                 label.size = 0.8,
                 label.r = unit(0.15, "lines"),
                 label.padding = unit(0.4, "lines"),
                 fontface = "bold",
                 size = 5)
  }

  return(plot)
}
