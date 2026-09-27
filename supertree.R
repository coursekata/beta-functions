# supertree()
#
# Display an rpart regression tree in a format designed to make the
# connection between decision trees and DATA = MODEL + ERROR explicit.
#
# supertree() does NOT fit a decision tree. Fit the model first with
# rpart(), then pass the fitted model to supertree().
#
# Example:
#
#   tree_model <- rpart(
#     passed ~ study_hours,
#     data = college_success
#   )
#
#   supertree(tree_model)
#
# Because `passed` is a numeric 0/1 variable, rpart() automatically fits
# a regression tree using squared error. At each terminal group:
#
#   - p-hat is the mean of the 0/1 outcome in that group and therefore
#     is also the proportion of cases with Y = 1.
#   - SSE is the sum of squared errors within that group.
#
# The MODEL summary reports:
#
#   - the number of terminal groups
#   - SST, the error from the empty model
#   - SSE, the error from the tree model
#   - Brier, the mean squared prediction error (SSE / n)
#   - PRE, the proportional reduction in error from SST to SSE
#
# The optional `depth` argument can be used to examine only the first
# levels of a larger tree:
#
#   supertree(tree_model, depth = 1)
#   supertree(tree_model, depth = 2)
#
# When depth is specified, groups at the displayed boundary are treated
# as terminal groups for purposes of calculating p-hat, SSE, Brier,
# and PRE. Thus supertree(model, depth = 1) shows the prediction
# function and error that would result if the tree stopped after its
# first split.
#
# Arguments:
#
#   model   An rpart model fit using the regression ("anova") method.
#
#   depth   Maximum depth of the tree to display. The root split is
#           depth = 1. The default (Inf) displays the complete tree.
#
#   digits  Number of decimal places used for p-hat, SST, SSE, Brier,
#           and PRE. Default = 2.
#
# An asterisk (*) identifies a terminal group in the tree being shown.
#
# NOTE: This version is designed for regression trees with quantitative
# predictors. Support for categorical splitting variables would require
# additional handling of rpart's categorical split representation.

supertree <- function(model, depth = Inf, digits = 2) {
  if (!inherits(model, "rpart")) {
    stop("supertree() requires a model created by rpart().")
  }

  if (model$method != "anova") {
    stop(paste0(
      "supertree() currently requires an rpart regression tree ",
      "(method = 'anova')."
    ))
  }

  if (!is.numeric(depth) || length(depth) != 1 || depth < 1) {
    stop("depth must be a positive number.")
  }

  frame <- model$frame
  node_numbers <- as.numeric(row.names(frame))

  node_depth <- function(node) {
    floor(log(node, base = 2))
  }

  node_info <- function(node) {
    i <- match(node, node_numbers)

    list(
      n = frame$n[i],
      prediction = frame$yval[i],
      sse = frame$dev[i],
      leaf = as.character(frame$var[i]) == "<leaf>"
    )
  }

  get_split <- function(node) {
    row_index <- match(node, node_numbers)

    if (as.character(frame$var[row_index]) == "<leaf>") {
      return(NULL)
    }

    internal_before <- which(
      seq_len(nrow(frame)) < row_index &
        as.character(frame$var) != "<leaf>"
    )

    start <- 1

    if (length(internal_before) > 0) {
      for (j in internal_before) {
        start <- start + 1 +
          frame$ncompete[j] +
          frame$nsurrogate[j]
      }
    }

    s <- model$splits[start, , drop = FALSE]

    list(
      variable = as.character(frame$var[row_index]),
      index = unname(s[1, "index"]),
      ncat = unname(s[1, "ncat"])
    )
  }

  fmt_p <- function(x) {
    sub("^0", "", formatC(x, format = "f", digits = digits))
  }

  fmt_error <- function(x) {
    formatC(x, format = "f", digits = digits)
  }

  fmt_pre <- function(x) {
    sub("^0", "", formatC(x, format = "f", digits = digits))
  }

  displayed_terminal <- function(node) {
    info <- node_info(node)
    info$leaf || node_depth(node) >= depth
  }

  rows <- list()

  add_row <- function(text, node = NA_real_, terminal = FALSE) {
    rows[[length(rows) + 1]] <<- list(
      text = text,
      node = node,
      terminal = terminal
    )
  }

  build_tree <- function(node,
                         prefix = "",
                         branch = NULL,
                         is_last = TRUE) {

    info <- node_info(node)

    connector <- if (is.null(branch)) {
      ""
    } else if (is_last) {
      "└── "
    } else {
      "├── "
    }

    branch_text <- if (is.null(branch)) {
      ""
    } else {
      paste0(
        branch,
        if (branch == "NO") " " else "",
        " (n = ",
        info$n,
        ") → "
      )
    }

    if (displayed_terminal(node)) {

      terminal_text <- paste0(
        prefix,
        connector,
        branch_text,
        "p\u0302 = ",
        fmt_p(info$prediction),
        ", SSE = ",
        fmt_error(info$sse),
        "  *"
      )

      add_row(
        text = terminal_text,
        node = node,
        terminal = TRUE
      )

      return(invisible(NULL))
    }

    split <- get_split(node)
    split_value <- format(split$index, trim = TRUE)

    if (split$ncat < 0) {
      rule <- paste0(
        split$variable,
        " < ",
        split_value,
        "?"
      )
    } else {
      rule <- paste0(
        split$variable,
        " >= ",
        split_value,
        "?"
      )
    }

    if (is.null(branch)) {
      add_row(rule)
    } else {
      add_row(
        paste0(
          prefix,
          connector,
          branch_text,
          rule
        )
      )
    }

    left_child <- node * 2
    right_child <- node * 2 + 1

    child_prefix <- if (is.null(branch)) {
      ""
    } else {
      paste0(
        prefix,
        if (is_last) "    " else "│   "
      )
    }

    if (left_child %in% node_numbers) {
      build_tree(
        left_child,
        prefix = child_prefix,
        branch = "YES",
        is_last = FALSE
      )
    }

    if (right_child %in% node_numbers) {
      build_tree(
        right_child,
        prefix = child_prefix,
        branch = "NO",
        is_last = TRUE
      )
    }
  }

  build_tree(1)

  model_formula <- paste(
    deparse(formula(model)),
    collapse = " "
  )

  root_n <- node_info(1)$n

  cat("\n")
  cat(
    "DECISION TREE: ",
    model_formula,
    "\n\n",
    sep = ""
  )

  cat("n = ", root_n, "\n", sep = "")

  for (row in rows) {
    cat(row$text, "\n", sep = "")
  }

  cat("\n* terminal group\n\n")

  shown_nodes <- node_numbers[
    vapply(
      node_numbers,
      node_depth,
      numeric(1)
    ) <= depth
  ]

  endpoint_nodes <- shown_nodes[
    vapply(
      shown_nodes,
      displayed_terminal,
      logical(1)
    )
  ]

  model_sse <- sum(
    vapply(
      endpoint_nodes,
      function(node) node_info(node)$sse,
      numeric(1)
    )
  )

  sst <- node_info(1)$sse
  brier <- model_sse / root_n
  pre <- (sst - model_sse) / sst

  cat("MODEL\n")
  cat(sprintf(
    "  %-17s %d\n",
    "Terminal groups:",
    length(endpoint_nodes)
  ))
  cat(sprintf(
    "  %-17s %s\n",
    "SST:",
    fmt_error(sst)
  ))
  cat(sprintf(
    "  %-17s %s\n",
    "SSE:",
    fmt_error(model_sse)
  ))
  cat(sprintf(
    "  %-17s %s\n",
    "Brier:",
    fmt_error(brier)
  ))
  cat(sprintf(
    "  %-17s %s\n",
    "PRE:",
    fmt_pre(pre)
  ))

  invisible(model)
}
