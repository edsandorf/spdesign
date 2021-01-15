#' Parse the utility functions
#'
#' The function parses the list of utilities to extract the relevant parameters
#' used to create the design.
#'
#' @param V A list of utility functions
#'
#' @return An object containing the outputs from the parsed utility functions, such
#' as the named vector of priors, the named list of attributes and their levels,
#' restrictions implied by the utility specifications
#'
parse_utility <- function(V) {
  # Run a set of checks
  if (!is.list(V)) stop("'U' has to be a named list of utility functions. Please see the manual.")
  if (length(V) < 2) stop("'U' has to contain at least 2 utility functions. Please see the manual.")

  J <- length(V)

  # Extract all the named values from the utility expressions
  parsed_V <- extract_named_values(V)

  # Check that all parameters and attributes are specified with values at least once!
  all_names <- unique(remove_whitespace(extract_all_names(V, TRUE)))
  value_names <- names(parsed_V)
  if (!all(all_names %in% value_names)) {
    missing_idx <- which((all_names %in% value_names) == FALSE)
    missing_values <- paste0("'", all_names[missing_idx], "'", collapse = " ")
    stop(paste0(missing_values, " does not have a specified prior or levels. Please make sure that all elements of the utility functions have been specified with a prior or level once."))
  }

  # Check whether a parameter or attribute is specified more than once
  if (any(duplicated(value_names))) {
    duplicate_idx <- duplicated(value_names)
    duplicate_values <- paste0("'", value_names[duplicate_idx], "'", collapse = " ")
    warning(paste0(duplicate_values, " are specified with priors or levels more than once. Only the first occurance of the value is used. If you intended to use different levels for different attributes in each utility function, please specify alternative specific attributes.\n "))
    parsed_V <- parsed_V[!duplicate_idx]
    value_names <- value_names[!duplicate_idx]
  }

  # Split into parameters and attributes and return as a list
  param_idx <- value_names %in% grep("b_", value_names, value = TRUE)
  list(
    param = parsed_V[param_idx],
    attrs = parsed_V[!param_idx]
  )
}
