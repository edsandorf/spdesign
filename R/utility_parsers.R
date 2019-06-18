#' Split utility
#'
#' Takes the utility functions and splits them into workable chunks
#'
#' @param V A list of utilities
#'
#' @return A list of matrices where each list element refers to an alternative.
#' The matrix has rows equal to the number of parameters for each alternative
#' and columns equal to param and attributes/interation terms

split_utility <- function(v) {
  v <- str_trim(unlist(str_split(v, pattern = "(?s)\\+")))
  v_mat <- str_split(v, pattern = "(?s)\\*", simplify = TRUE)
  rows <- nrow(v_mat)
  cols <- ncol(v_mat)
  matrix(str_trim(v_mat, side = "both"), nrow = rows, ncol = cols)
}

#' Parse the utility functions
#'
#' This function parses the utility expressions and extracts the correct number
#' of attributes and levels as well as coefficients and priors.
#'
#' @param V A list of utility functions
#'
#' @examples
#' utility_funcs <- list(
#'   alt_1 = "beta_1 | c(0.1) * x_1 | c(0, 1) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8) * x_3 | c(2, 5)",
#'   alt_2 = "beta_1 * x_1 + beta_2 * x_2"
#' )
#'
#'

parse_utility <- function(V) {
  if (!is.list(V)) {
    stop("'utility_funcs' must be a list")
  }

  if (length(V) < 2) {
    stop("You must specify a minimum of 2 alternatives")
  }

  J <- length(V)

  # Split on plus and allow for line breaks - each list element is a vector
  # of length equal to the number of parameters
  V <- lapply(V, function(v) split_utility(v))


}
