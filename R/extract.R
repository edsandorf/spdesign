#' Extract all names
#'
#' Extracts all parameter and attribute names from the utility function.
#' This is a wrapper around \code{\link[stringr]{str_extract_all}} with a specified
#' boundary. The function also calls \code{\link{remove_all_brackets}} to
#' ensure that if a word is used inside a square bracket, e.g. seq, it is not
#' extracted.
#'
#' Note that we are not matching spaces nor the interaction operator I(). This
#' is to avoid I being identified as its own (unspecified) attribute.
#'
#' @param string A character string
#' @param simplify If TRUE return as a vector. Default is FALSE.
#'
#' @return A list or vector with all names
extract_all_names <- function(string, simplify = FALSE) {
  s <- str_extract_all(remove_all_brackets(string), "\\b[^(\\s|I\\()]\\w*\\b")
  s <- lapply(s, unique)

  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract parameter names
#'
#' Extracts all words starting with "b_". Leverages the fact that all parameters
#' has to start with "b_".
#'
#' @inheritParams extract_all_names
#'
#' @return A list or vector with the parameter names.
extract_param_names <- function(string, simplify = FALSE) {
  expr <- "b_.*?\\b"
  s <- str_extract_all(string, expr)

  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract attribute names
#'
#' Extracts attribute names. It is a wrapper around
#' \code{\link{extract_all_names}} and \code{\link{extract_param_names}}.
#'
#' @inheritParams extract_all_names
#'
#' @return A Vector or string wtih attribute names
extract_attribute_names <- function(string, simplify = FALSE) {
  all_names <- extract_all_names(string, TRUE)
  param_names <- extract_param_names(string, TRUE)
  idx <- all_names %in% param_names
  all_names[!idx]
}

#' Extract the value argument(s)
#'
#' Extracts the value argument(s) of the supplied string. The value argument
#' is defined as the characters between [] string.
#'
#' @inheritParams extract_all_names
#'
#' @return A vector or list with the extracted value arguments
extract_values <- function(string, simplify = FALSE) {
  # (?<=(\\|)) - A positive look behind for '['
  # .*? - Non-greedy capture between the start and end of the match
  # (?=(\\*|\\+|$)) - A positive look ahead for ']'
  expr <- "(?<=\\[).*?(?=\\])"
  s <- str_extract_all(string, expr)

  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract unparsed named values of the utilitiy function
#'
#' If the utility function contains parameters that are dummy coded, the dummy
#' coding is handled here. By expanding the dummy coding prior to parsing we can
#' directly consider Bayesian priors for each level.
#'
#' @inheritParams extract_all_names
#'
#' @return A named list of parameter and attribute values. Each list element is
#'   named and contains a numeric value or expression to be parsed
extract_unparsed_values <- function(string) {
  # Extracting the specified parameters and attributes
  string_elements <- extract_specified(string, TRUE)

  values <- extract_values(string_elements)
  names(values) <- remove_all_brackets(string_elements)

  if (contains_dummies(string)) {

    # This is the expression that handles the prior distributions
    expr <- "-?(\\d+\\.?\\d*|(normal|uniform|lognormal|triangular)_p\\(.*?\\))"

    for (dummy in names(values)[str_detect(names(values), "b_.*_dummy")]) {
      expanded <- as.list(unlist(str_extract_all(values[dummy], expr)))

      # Get the corresponding attribute by remove "b_" and "_dummy"
      # grep(str_extract(names(values[i]), "(?<=b_).*(?=_dummy)")

      names(expanded) <- paste0(str_extract(dummy, "^.*(?=_dummy$)"), seq_along(expanded) + 1)

      # Order doesn´t matter, so we can do
      values <- c(values[names(values) != dummy], expanded)
      # values <- c(values[-i], expanded)
    }
  }

  return(
    values
  )
}

#' Extracts the named values of the utility function
#'
#' The function extracts the named values of the supplied utility function.
#'
#'
#' @inheritParams extract_all_names
#'
#' @return A named list of parameter and attribute values. Each list element is
#' named and can contain a single prior, a list with a mean and sd, or a vector
#' with attribute levels
extract_named_values <- function(string) {

  values <- extract_unparsed_values(string)

  values <- lapply(values, function(x) {
    eval(parse(text = x))
  })

  return(
    values
  )
}

#' Extract specified
#'
#' Only extract parameters and attributes with specified priors and levels. This
#' is very useful to test whether parameters or attributes are specified
#' multiple times
#'
#' @inheritParams extract_all_names
extract_specified <- function(string, simplify = FALSE) {
  # [^\\s\\+\\-\\*\\/] - Negative group not match one in the group.
  # (\\(.*?\\))? - Optional to match the parenthesis with level occurrences
  expr <- "[^\\s\\+\\-\\*\\/]*?\\[.*?\\](\\(.*?\\))?"
  s <- str_extract_all(remove_whitespace(string), expr)

  if (simplify) {
    unlist(s)
  } else {
    s
  }
}

#' Extract the frequency of levels
#'
#' The function extracts how many times each level of an attribute should
#' occur within the design when attribute level balance is not enforced.
#' Note that it extracts the parentheses AFTER the end of the square brackets.
#' Specifying round brackets without the square brackets are syntactically
#' invalid and therefore we want the code to fail in this case.
#'
#' @inheritParams extract_all_names
extract_level_occurrence <- function(string, simplify = FALSE) {
  expr <- "(?<=\\])\\(.*?\\)"
  s <- str_extract_all(string, expr)

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
#' IMPORTANT: The function will silently drop duplicates.
#'
#' @param string A single character string or list of character strings with a
#' single or multiple utility functions
#' @param type A string indicating the type: prior or param
#'
#' @return A named vector of priors or parameters where the type of distribution
#' is given by a character letter: "normal", "lognormal", "uniform" or
#' "triangular"
extract_distribution <- function(string, type) {
  b <- switch(
    type,
    prior = extract_prior_distribution(string),
    param = extract_param_distribution(string)
  )

  # If no distribution is found, return NA
  if (length(b) == 0) {
    return(NA)
  }

  expr <- "(?<=\\[)(normal|lognormal|uniform|triangular)"
  distribution <- unlist(str_extract_all(b, expr))
  b_names <- extract_param_names(b)
  names(distribution) <- b_names

  # Remove duplicate values
  distribution[!duplicated(b_names)]
}

#' Extract the prior distribution
#'
#' @inheritParams extract_distribution
extract_prior_distribution <- function(string) {
  expr <- "\\b\\w*\\[(normal_p|uniform_p|lognormal_p|triangular_p)\\(.*?\\]"
  unlist(str_extract_all(string, expr))
}

#' Extract the parameter distribution
#'
#' @inheritParams extract_distribution
extract_param_distribution <- function(string) {
  expr <- "\\b\\w*\\[(normal|uniform|lognormal|triangular)\\(.*?\\]"
  unlist(str_extract_all(string, expr))
}
