#' Remove all white spaces
#'
#' Takes a string as an input and removes all whitespaces in the string
#'
#' @param x A character string
#'
#' @return A character vector with no white spaces
#'
#' @examples
#' x <- "  test | this    "
#' remove_whitespace(x)
#'
remove_whitespace <- function(x) {
  if (!is.character(x)) stop("Input must be a character vector")
  gsub("\\s", "", x, perl = TRUE)
}

#' Extract names
#'
#' Extracts the name of a parameter or attribute by removing everything after
#' and including |.
#'
#' @param x A character vector of the form name | parameter(s)
#'
#' @return A character vector. In most cases the character vector will be of
#' length 1, but in the case of interaction terms, the character vector will
#' be of length equal to the number of terms.
#'
#' @examples
#' x <- "x_1|c(0,1)"
#' extract_names(x)
#'
#' x <- c("x_2|c(2,4,6,8)", "x_3|c(2,5)")
#' extract_names(x)
#'
extract_names <- function(x) {
  # Extract the name(s)
  str_tmp <- sub("\\|.*", "", x, perl = TRUE)

  # If we have duplicate names
  if (any(duplicated(str_tmp))) {
    stop("There are duplicate elements when extracting names from the string. Check that the interaction term is not with itself. This should be considered changed to a warning if there are valid edge cases I have not thought about.")
  }

  # The substitute expression will return an empty string if nothing is present prior to |
  if (length(str_tmp) == 1 && str_tmp == "") {
    stop("There is no name to retrieve. Check that you have specified a parameter/attribute and not just priors/attribute levels.")
  }

  # If it is bayesian, then we need to  return a vector of correct parameter names
  # Return
  str_tmp
}

#' Extract the values
#'
#' @inheritParams extract_names

extract_values <- function(x) {
  # Remove the attribute or parameter name
  str_tmp <- sub(".*\\|", "", x, perl = TRUE)

  # Return the numeric value of the parameters as a single number or vector
  as.numeric(unlist(regmatches(str_tmp, gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", str_tmp))))
}


#' Extracts the parameters
#'
#' Wrapping around extract values and extract names
#'
#' @inheritParams extract_names
#'
#' @return A named vector of parameters with associated attributes for random
#' parameters and distributions
#'
extract_params <- function(x) {
  # Check input
  if (!is.character(x)) stop("Input must be a character vector")

  # Extract the names
  beta_names <- extract_names(x)
  if (attr(gregexpr("(N|LN|U|T)", x, perl = TRUE)[[1]], "match.length") > 0) {
    beta_names <- paste0(c("mu_", "sigma_"), beta_names)

    if (length(attr(gregexpr("(N|LN|U|T)", x, perl = TRUE)[[1]], "match.length")) > 1){
      stop("The functionality for specifying Bayesian priors for parameters in the RPL model has not been implemented yet.")
    }
  }

  beta_values <- extract_values(x)

  # Return
  names(beta_values) <- beta_names
  beta_values
}

#'
#'
#' Extracts the attributes and levels of the utility functions
#'
#' @param x A character string
#'
extract_attrs <- function(x) {
  if (!is.character(x)) stop("Input must be a character vector")

  # Extract the names
  attrs_names <- extract_names(x)

  # Attribute values
  attrs_values <- extract_values(x)



}





#' Split utility
#'
#' Takes the list of utility functions and splits them on + and *.
#'
#' @inheritParams parse_utility
#'
#' @return A list of matrices where each list element refers to an alternative.
#' The matrix has rows equal to the number of parameters for each alternative
#' and columns equal to param and attributes/interation terms

split_utility <- function(V) {
  v <- str_trim(unlist(str_split(V, pattern = "(?s)\\+")))
  v_mat <- str_split(v, pattern = "(?s)\\*", simplify = TRUE)
  rows <- nrow(v_mat)
  cols <- ncol(v_mat)
  matrix(str_trim(v_mat, side = "both"), nrow = rows, ncol = cols)
}

#' Extract the parameters
#'
#' Extracts the parameters from the list of matrices created by
#'
#' @inheritParams parse_utility
#' @param bayesian If TRUE use Bayesian priors for the parameters
#'
#' @return A named vector of parameters

extract_param <- function(V, bayesian) {
  p <- V[, 1L]
  param_names <- str_trim(str_extract(p, pattern = "(?s)([^\\|]+)"), side = "both")

  if (bayesian) {

  } else {

  }

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
