# outer.R
# Shades the outer tails of a distribution (complement of middle())
# For use with coursekata package and ggformula histograms

#' Shade the outer tails of a distribution
#'
#' This function is used with gf_histogram's fill aesthetic to shade both
#' outer tails of a distribution. It is the complement of middle() - where
#' middle(x, .95) shades the central 95%, outer(x, .05) shades the outer 5%
#' (2.5% in each tail).
#'
#' @param x A numeric vector (typically a variable in a data frame)
#' @param prop The total proportion to shade in both tails combined.
#'        For example, prop = 0.05 shades 2.5% in each tail.
#'
#' @return A logical vector indicating which values fall in the outer tails
#'
#' @examples
#' library(coursekata)
#'
#' # Shade the outer 5% (2.5% in each tail)
#' gf_histogram(~Thumb, data = Fingers, fill = ~outer(Thumb, .05))
#'
#' # Shade the outer 10% (5% in each tail)
#' gf_histogram(~Thumb, data = Fingers, fill = ~outer(Thumb, .10))
#'
#' @export
outer <- function(x, prop) {
  if (!is.numeric(prop) || prop <= 0 || prop >= 1) {
    stop("prop must be a number between 0 and 1")
  }
  
  # Calculate the cutoffs for each tail
  # prop is the total in both tails, so each tail gets prop/2
  tail_prop <- prop / 2
  
  x_clean <- x[!is.na(x)]
  x_sorted <- sort(x_clean)
  n <- length(x_sorted)
  
  # Lower cutoff: values below this are in the lower tail
  lower_idx <- floor(tail_prop * n) + 1
  lower_idx <- max(1, min(n, lower_idx))
  cutoff_lower <- x_sorted[lower_idx]
  
  # Upper cutoff: values above this are in the upper tail
  upper_idx <- ceiling((1 - tail_prop) * n)
  upper_idx <- max(1, min(n, upper_idx))
  cutoff_upper <- x_sorted[upper_idx]
  
  # Return TRUE for values in either tail (outer regions)
  x <= cutoff_lower | x >= cutoff_upper
}
