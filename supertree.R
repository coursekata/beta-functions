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
#   - PRE, the proportional reduction in error from SST to SSE
#
# The optional `depth` argument can be used to examine only the first
# levels of a larger tree:
#
#   supertree(tree_model, depth = 1)
#   supertree(tree_model, depth = 2)
#
# When depth is specified, groups at the displayed boundary are treated
# as terminal groups for purposes of calculating p-hat, SSE, and PRE.
# Thus supertree(model, depth = 1) shows the prediction function and
# error that would result if the tree stopped after its first split.
#
# Arguments:
#
#   model   An rpart model fit using the regression ("anova") method.
#
#   depth   Maximum depth of the tree to display. The root split is
#           depth = 1. The default (Inf) displays the complete tree.
#
#   digits  Number of decimal places used for p-hat, SST, SSE, and PRE.
#           Default = 2.
#
# An asterisk (*) identifies a terminal group in the tree being shown.
#
# NOTE: This version is designed for regression trees with quantitative
# predictors. Support for categorical splitting variables would require
# additional handling of rpart's categorical split representation.


supertree <- function(model, depth = Inf, digits = 2) {
  
  # ----- Check model -----
  
  if (!inherits(model, "rpart")) {
    stop("supertree() requires a model created by rpart().")
  }
  
  if (model$method != "anova") {
    stop(
      paste0(
        "supertree() currently requires an rpart regression tree ",
        "(method = 'anova')."
      )
    )
  }
  
  if (!is.numeric(depth) || length(depth) != 1 || depth < 1) {
    stop("depth must be a positive number.")
  }
  
  
  # ----- Basic tree information -----
  
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
  
  
  # ----- Find the primary split for a node -----
  
  # rpart stores information about splits separately from the node
  # information in model$frame. This helper finds the primary split
  # belonging to a particular internal node.
  
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
        
        start <- start +
          1 +
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
  
  
  # ----- Number formatting -----
  
  fmt_p <- function(x) {
    sub(
      "^0",
      "",
      formatC(x, format = "f", digits = digits)
    )
  }
  
  
  fmt_sse <- function(x) {
    formatC(x, format = "f", digits = digits)
  }
  
  
  fmt_pre <- function(x) {
    sub(
      "^0",
      "",
      formatC(x, format = "f", digits = digits)
    )
  }
  
  
  # ----- Determine which groups are terminal in the displayed tree -----
  
  # A group is terminal in the display if:
  #
  #   1. it is an actual terminal node in the fitted rpart model, OR
  #   2. it has reached the maximum depth requested by the user.
  
  displayed_terminal <- function(node) {
    
    info <- node_info(node)
    
    info$leaf || node_depth(node) >= depth
  }
  
  
  # ----- Build the tree one row at a time -----
  
  rows <- list()
  
  
  add_row <- function(left,
                      prediction = NA_real_,
                      sse = NA_real_,
                      terminal = FALSE) {
    
    rows[[length(rows) + 1]] <<- list(
      left = left,
      prediction = prediction,
      sse = sse,
      terminal = terminal
    )
  }
  
  
  build_tree <- function(node,
                         prefix = "",
                         branch = NULL,
                         is_last = TRUE) {
    
    info <- node_info(node)
    
    
    # Tree connector
    
    connector <- if (is.null(branch)) {
      ""
    } else if (is_last) {
      "└── "
    } else {
      "├── "
    }
    
    
    # Branch label and number of cases traveling down that branch
    
    branch_text <- if (is.null(branch)) {
      
      ""
      
    } else {
      
      paste0(
        branch,
        if (branch == "NO") " " else "",
        " (n = ",
        sprintf("%3d", info$n),
        ") → "
      )
    }
    
    
    # If this is a terminal group in the displayed tree,
    # store its prediction and SSE.
    
    if (displayed_terminal(node)) {
      
      add_row(
        left = paste0(
          prefix,
          connector,
          branch_text
        ),
        prediction = info$prediction,
        sse = info$sse,
        terminal = TRUE
      )
      
      return(invisible(NULL))
    }
    
    
    # Otherwise this is an internal decision node.
    
    split <- get_split(node)
    
    split_value <- format(
      split$index,
      trim = TRUE
    )
    
    
    # For a quantitative predictor, rpart's ncat sign indicates
    # the direction of the primary split.
    
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
    
    
    # Add the decision rule to the display.
    
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
    
    
    # Find child nodes.
    
    left_child <- node * 2
    right_child <- node * 2 + 1
    
    
    # Extend the tree-drawing prefix.
    
    child_prefix <- if (is.null(branch)) {
      
      ""
      
    } else {
      
      paste0(
        prefix,
        if (is_last) "    " else "│   "
      )
    }
    
    
    # Build left/YES branch.
    
    if (left_child %in% node_numbers) {
      
      build_tree(
        left_child,
        prefix = child_prefix,
        branch = "YES",
        is_last = FALSE
      )
    }
    
    
    # Build right/NO branch.
    
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
  
  
  # ----- Model formula -----
  
  model_formula <- paste(
    deparse(formula(model)),
    collapse = " "
  )
  
  
  # ----- Helpers for aligned Unicode output -----
  
  # Tree-drawing characters such as ├ and │ can cause problems with
  # ordinary character counts. type = "width" measures their displayed
  # width so that the p-hat and SSE columns line up correctly.
  
  display_width <- function(x) {
    nchar(x, type = "width")
  }
  
  
  pad_right <- function(x, width) {
    
    spaces_needed <- width - display_width(x)
    
    if (spaces_needed > 0) {
      paste0(x, strrep(" ", spaces_needed))
    } else {
      x
    }
  }
  
  
  # ----- Format terminal statistics -----
  
  prediction_strings <- vapply(
    rows,
    function(x) {
      
      if (is.na(x$prediction)) {
        ""
      } else {
        paste0(
          "p\u0302 = ",
          fmt_p(x$prediction)
        )
      }
    },
    character(1)
  )
  
  
  sse_strings <- vapply(
    rows,
    function(x) {
      
      if (is.na(x$sse)) {
        ""
      } else {
        paste0(
          "SSE = ",
          fmt_sse(x$sse)
        )
      }
    },
    character(1)
  )
  
  
  # All terminal statistics begin at the same horizontal position,
  # regardless of the depth of the terminal group.
  
  left_width <- max(
    vapply(
      rows,
      function(x) display_width(x$left),
      numeric(1)
    )
  ) + 4
  
  
  pred_width <- max(
    vapply(
      prediction_strings,
      display_width,
      numeric(1)
    )
  ) + 3
  
  
  sse_width <- max(
    vapply(
      sse_strings,
      display_width,
      numeric(1)
    )
  ) + 3
  
  
  # ----- Print tree -----
  
  root_n <- node_info(1)$n
  
  cat("\n")
  cat(
    "DECISION TREE: ",
    model_formula,
    "\n\n",
    sep = ""
  )
  
  cat(
    "n = ",
    root_n,
    "\n",
    sep = ""
  )
  
  
  for (i in seq_along(rows)) {
    
    row <- rows[[i]]
    
    if (is.na(row$prediction)) {
      
      # Internal decision rule
      
      cat(
        row$left,
        "\n",
        sep = ""
      )
      
    } else {
      
      # Terminal group
      
      cat(
        pad_right(
          row$left,
          left_width
        ),
        pad_right(
          prediction_strings[i],
          pred_width
        ),
        pad_right(
          sse_strings[i],
          sse_width
        ),
        if (row$terminal) "*" else "",
        "\n",
        sep = ""
      )
    }
  }
  
  
  cat("\n* terminal group\n\n")
  
  
  # ----- Calculate MODEL statistics -----
  
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
  
  
  # Total model SSE is the sum of SSE across the terminal groups.
  
  model_sse <- sum(
    vapply(
      endpoint_nodes,
      function(node) node_info(node)$sse,
      numeric(1)
    )
  )
  
  
  # The root node represents the empty model, so its SSE is SST.
  
  sst <- node_info(1)$sse
  
  
  # PRE = proportional reduction in error relative to the empty model.
  
  pre <- (sst - model_sse) / sst
  
  
  # ----- Print MODEL summary -----
  
  cat("MODEL\n")
  
  cat(
    sprintf(
      "  %-17s %d\n",
      "Terminal groups:",
      length(endpoint_nodes)
    )
  )
  
  cat(
    sprintf(
      "  %-17s %s\n",
      "SST:",
      fmt_sse(sst)
    )
  )
  
  cat(
    sprintf(
      "  %-17s %s\n",
      "SSE:",
      fmt_sse(model_sse)
    )
  )
  
  cat(
    sprintf(
      "  %-17s %s\n",
      "PRE:",
      fmt_pre(pre)
    )
  )
  
  
  invisible(model)
}
