#' Extract the name argument(s)
#'
#' Extract the name argument(s) of the supplied string.
#'
#' @param string A character string
#' @param simplify If TRUE return as a character vector
#'
#' @return A list
#'
#' @noRd
extract_name_args <- function(string, simplify = FALSE) {
  # (?<=(^|\\+|\\*)) - A positive look behind for the start of the string, '+' or '*'
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\||\\*\\+|$)) - A positive look ahead for |, '*', '+', or the end of the string
  expr <- "(?<=(^|\\+|\\*)).*?(?=(\\||\\*\\+|$))"
  s <- str_extract_all(string, expr)
  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract the value argument(s)
#'
#' Extracts the value argument(s) of the supplied string. The value argument
#' is defined as the characters between | and '*', '+' and $ in the supplied
#' string.
#'
#' @param string A character string
#' @param simplify If TRUE return as a vector. Default is FALSE.
#'
#' @return A list
#'
#' @noRd
extract_value_args <- function(string, simplify = FALSE) {
  # (?<=(\\|)) - A positive look behind for '|'
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\*|\\+|$)) - A positive look ahead for '*', '+', or the end of the string
  expr <- "(?<=(\\|)).*?(?=(\\*|\\+|$))"
  s <- str_extract_all(string, expr)
  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract numeric
#'
#' Extracts the numerical values from a string. The function is a wrapper around
#' \code{\link{str_extract_all}}.The function is only intended for internal use
#' to avoid code-duplication.
#'
#' @param string A character string
#' @param simplify If TRUE return as a vector. Default is FALSE
#'
#' @return Returns a list containing all the numerical values in the supplied
#' vector. Returns a single vector if simplify = TRUE
#'
#' @noRd
extract_numeric <- function(string, simplify = FALSE) {
  expr <-"[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
  s <- lapply(str_extract_all(string, expr), function(s) {as.numeric(s)})
  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract distributions
#'
#' This function will locate and extract the the distributions for Bayesian
#' priors and random parameters as specified in the design. The output is used
#' to create the matrix of correct draws for priors and parameters.
#'
#' @param string A single character string or list of character strings with a
#' single or multiple utility functions
#'
#' @return A list of length equal to the number of distributions in the order
#' that they appear in the utility functions going from top left to bottom right
#' across the list.
#'
#' @noRd
extract_distribution <- function(string) {
  # Extract the distributions and return as a vector.
  unlist(str_extract_all(string, "(N|LN|TR|U)(?=\\()"))
}
