#' Remove all white spaces
#'
#' Takes a string as an input and removes all whitespaces in the string
#'
#' @param string A character string
#'
#' @return A character vector with no white spaces
#'
remove_whitespace <- function(string) {
  if (!is.character(string)) stop("Input must be a character vector")
  str_replace_all(string, "\\s", "")
}

#' Extract the name argument(s)
#'
#' Extract the name argument(s) of the supplied string.
#'
#' @param string A character string
#' @param simplify If TRUE return as a character vector
#'
#' @return A list
#'
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
extract_numeric <- function(string, simplify = FALSE) {
  expr <-"[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
  s <- lapply(str_extract_all(string, expr), function(s) {as.numeric(s)})
  if (simplify) {
    unlist(s)
  } else {
    s
  }
}














#' Split a string on the supplied pattern
#'
#' A simple wrapper around \code{\link{strsplit}}.
#'
#' @param string A character string to be split
#' @param pattern A character string pattern. The pattern should conform to
#' perl style regex. See \code{\link{strsplit}} for details.
#' @param simplify A boolean indicating whether the output should be simplified
#' from a list to a vector. The default is FALSE and a list of character vectors
#' is returned. This is the default behavior for \code{\link{strsplit}} and the
#' default for this wrapper function.
#'
#' @return A list of character vectors.
split_string <- function(string, pattern, simplify = TRUE) {
  if (!is.character(string)) stop("The supplied 'string' must be a character vector.")
  if (!is.character(pattern)) stop("The supplied 'pattern' must be a character vector.")

  s <- strsplit(string, pattern, perl = TRUE)
  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Split utility
#'
#' Takes a single utility function and splits it into pieces that can be used by
#' the utility parser to create the named vector of priors and the named list
#' of attributes and levels
#'
#' @param x A character string containing a single utility expression
#'
#' @return A list of character vectors where each list element contains the
#' parameter/value or attribute/value combinations. The first element of each
#' vector is always the parameter and subsequent elements are attributes or
#' interaction terms.
#'

split_utility <- function(x) {
  # Trim away the white spaces
  x <- remove_whitespace(x)

  # Split the utility on the '+' sign and return a character vector
  x <- split_string(x, "(?s)\\+", simplify = TRUE)

  # Split the utility on the '*' sign and return a list of character vectors
  x <- split_string(x, "(?s)\\*", simplify = FALSE)
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




#' Extracts the parameters
#'
#' Wrapping around extract values and extract names
#'
#' @param x A character string
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

  beta_values <- extract_numeric(x)

  # Return
  names(beta_values) <- beta_names
  beta_values
}





#' Parse the utility functions
#'
#' The function parses the list of utilities to extract the relevant parameters
#' used to create the design.
#'
#' @param U A named list of utility expressions
#'
#' @return An object containing the outputs from the parsed utility functions, such
#' as the named vector of priors, the named list of attributes and their levels,
#' restrictions implied by the utility specifications
#'
#' @examples
#' U <- list(
#'   alt_1 = "beta_1 | c(0.1, 0.5) * x_1 | c(0, 1, 2) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8)",
#'   alt_2 = "beta_1 * x_1 + beta_2 * x_2"
#' )
#'
#'
#' @export

parse_utility <- function(U) {
  # Run a set of checks
  if (!is.list(U)) stop("'U' has to be a named list of utility functions. Please see the manual.")
  if (length(U) < 2) stop("'U' has to contain at least 2 utility functions. Please see the manual.")

  J <- length(U)

  # Split the utility for further processing





}


#' Extracts the utility components
#'
#' This function acts as a wrapper around \code{\link{extract_utility_function}},
#' \code{\link{extract_params}} and \code{\link{extract_attrs}}, and will parse
#' a single utility function. The function is only meant for internal use and is
#' called from within \code{\link{parse_utility}}.
#'
#' @param string A character string formatted as a utility function
#'
#' @return A named list with the utility expression, the trimmed utility without
#' attribute and parameter information, a named vector with priors and a named
#' list with attributes and levels.
extract_utility_components <- function(string) {
  list(
    input = string,
    u = extract_utility_function(string),
    params = extract_params(string),
    attrs = extract_attrs(string)
  )
}

#' Extract utility function
#'
#' @param string A character string
#'
extract_utility_function <- function(string) {
  # Extract the utility
  # (^|\\*|\\+) - Start the match at the beginning of the string, '*' or '+'
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\||\\Z)) - A positive look ahead for | or the end of the string
  expr <- "(^|\\*|\\+).*?(?=(\\||$))"
  string_u <- str_extract_all(string, expr, simplify = TRUE)
  string_u <- str_c(string_u, collapse = "")
  string_u <- str_trim(string_u, side = "both")


  # Return a list of
  string_u
}

#' Extract the parameters and priors
#'
#' @param string A character string
#'
#'
extract_params <- function(string) {
  # (?<=(^|\\+)) - A positive look behind for the start of the string or '+'
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\||\\*\\+|$)) - A positive look ahead for | or the end of the string
  expr <- "(?<=(^|\\+)).*?(?=(\\||\\*\\+|$))"
  names_b <- str_extract_all(string, expr, simplify = TRUE)

  # This is where we need to check and do further processing for .dummy coding #
  # If dummy, need to place a check for multiple values below #

  names_b <- str_trim(names_b, side = "both") # Drops dimensions and returns vector

  # Use the parameter name as the look behind and stop at the first '*', '+' or
  # end of line
  expr <- paste0("(?<=(", names_b, ")).*?(?=(\\*|\\+|$))")
  values_b <- str_extract(string, expr)

  # This is where we need to check and do further processing for Bayesian and RPL #

  values_b <- extract_numeric(values_b)

  # Return the values
  names(values_b) <- names_b
  values_b
}


#' Extract the attributes and levels
#'
#' @param string A character string
#'
extract_attrs <- function(string) {
  # Extract the attributes
  # (?<=(\\*)) - A positive look behind for '*'
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\||\\+\\Z)) - A positive look ahead for |, '+', or the end of the string
  expr <- "(?<=(\\*)).*?(?=(\\||\\*\\+|$))"
  names_attrs <- str_extract_all(string, expr, simplify = TRUE)
  names_attrs <- str_trim(names_attrs, side = "both")

  # Extract the levels
  expr <- paste0("(?<=(", names_attrs, ")).*?(?=(\\*|\\+|$))")
  levels_attrs <- str_extract(string, expr)
  levels_attrs <- lapply(levels_attrs, function(x) extract_numeric(x))

  # Return the values
  names(levels_attrs) <- names_attrs
  levels_attrs
}












#' Extract Bayesian priors
#'
#'
#' @param string A character string
#'
extract_bayesian_prior <- function(string) {
  extract_value_args(string)

  extract_name_args(string)


  # Uses PCRE regex because ICU doesn't support the reset operator \K
  # See https://stackoverflow.com/questions/65235254/splitting-a-string-with-nested-parentheses-at-only-the-top-level-where-level-i/65238265#65238265
  # and the great explanation from Wiktor Stribizew.
  expr <- "(?:\\G(?!^)\\s*,\\s*|^(N|LN|T|U)\\()\\K(?:\\d+|\\w+(\\([^()]*(?:(?1)[^()]*)*\\)))(?=\\s*,|\\)$)"
  regmatches(string, gregexpr(expr, string, perl=TRUE))


}

#' Extract dummy coded
#'
#' The function extracts and returns parameters attributes and levels using the
#' '.dummy' syntax.
#'
#' @param string A character string
#'
#' @return A list with the original input string, a named parameter vector of priors,
#' and a list of dummy coded attributes and their levels.
#'
extract_dummy <- function(string) {
  # Extract the parameters and levels - This is gonna fail immediately if we have a Bayesian prior!!!
  values <- extract_numeric(extract_value_args(string, simplify = TRUE))

  # Check that the correct number of priors and attributes are present
  if (Reduce("-", lapply(values, length)) != -1) {
    stop("When using dummy coding, the number of priors specified must be equal to the number of levels - 1. Please check your specification.")
  }

  # Extract the names of the parameters and levels, and create an addon for the recoded variables
  name_values <- str_trim(extract_name_args(string, simplify = TRUE), side = "both")
  name_values <- str_replace_all(name_values, "\\.dummy", "")
  name_addon <- paste("lvl", seq(2, length(values) + 1), sep = "_")

  # Get the parameters
  params <- values[[1]]
  names(params) <- paste(name_values[1], name_addon, sep = "_")

  # Get the re-coded attributes and dummy-levels
  attrs <- lapply(seq_along(params), function(not_in_use) c(0, 1))
  names(attrs) <- paste(name_values[2], name_addon, sep = "_")

  # Print message
  message(blue$bold(symbol$info), " When using dummy coding the first level is always dropped. The restriction that neither dummy can have a value of 1 at the same time is imposed.")

  # Return a list with new params and attrs
  list(
    input = string,
    params = params,
    attrs = attrs
  )
}
