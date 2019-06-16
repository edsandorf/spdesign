#' Parse the utility functions
#'
#' This function parses the utility expressions and extracts the correct number
#' of attributes and levels as well as coefficients and priors.
#'
#' @param V A list of utility functions
#'
#' @examples
#' utility_funcs <- list(
#'   alt_1 = "beta_1[0.1] * x_1[0, 1] + beta_2[-0.1] * x_2[2, 4, 6, 8]",
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

  lapply(V, function(v) {
    str_trim(str_split(v, pattern = "\\+", simplify = TRUE))
  })
  str_split(v, pattern = "[\\d]")
  str_split(v, pattern = "[\\b]")
  str_extract_all(v, "\\w")


  # Extracts the square bracket
  pattern <- "(\\[.*?\\])"
  str_extract(v, pattern)
  str_split(v, pattern)


  # Removes all the brackets
  pattern <- "\\[|\\]"
  str_replace_all(tmp, pattern, "")
  str_split(tmp, pattern)

  # Returns the start and end position of the brackets in a matrix!
  pattern <- "(\\[.*?\\])"
  str_locate_all(v, pattern)
#http://uc-r.github.io/regex

}

#'
#'
#' @return A list of strings with length equal to the number of coefficients
#'

parse_alt <- function(v) {

}

#'
#'
#'
#'

parse_coef <- function(b) {

}
