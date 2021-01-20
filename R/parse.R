#' Parse the utility functions
#'
#' The function parses the list of utilities to extract the relevant parameters
#' used to create the design.
#'
#' @param V A list of utility functions
#'
#' @return
#' A list with three elements:
#' 1) The returned cleaned expression of the utility function is ready to be evaluated
#' in the design context
#' 2) A list of parameters
#' 3) A list of attributes entering each alternative. For example, for two
#' alternatives, it will return a list of length, with each list element
#' containing the list of attributes entering that alternative's utility function.
#' The purpose is to correctly handle alternative specific variables and return
#' a list that can be directly passed to create_full_factorial.
#'
#'@export
parse_utility <- function(V) {
  # Extract useful information ----
  all_names <- unique(remove_whitespace(extract_all_names(V, TRUE)))
  values <- extract_named_values(V)
  value_names <- names(values)

  # Run checks on the utility functions ----
  if (!is.list(V)) stop("'V' has to be a named list of utility functions. Please see the manual.")
  if (length(V) < 2) stop("'V' has to contain at least 2 utility functions. Please see the manual.")
  if (!all(do.call(c, lapply(V, is_balanced, "[", "]")))) {
    stop("There is an uneven number of opening and closing brackets in the utility functions. Make sure that all opening brackets only have one closing bracket.")
  }
  if (!all(do.call(c, lapply(V, is_balanced, "(", ")")))) {
    stop("There is an uneven number of opening and closing parentheses in the utility functions. Make sure that all opening parenthesis only have one closing parenthesis.")
  }

  # Dummy coding ----
  if (any(str_detect(V, "_dummy"))) {

  }

  # Check that all parameters and attributes are specified with values at least once!

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
    values <- values[!duplicate_idx]
    value_names <- value_names[!duplicate_idx]
  }

  # Check that a parameter only has one prior, issue warning and use first. Suggest `_dummy`

  # Prepare attributes so that they can be directly used to create the full factorial
  param_idx <- value_names %in% grep("b_", value_names, value = TRUE)
  cleaned_V <- lapply(V, clean_utility)
  attrs <- values[!param_idx]
  # This next step may not be necessary, but depends on how the Modified Federov or RSC algorithm works
  # attrs <- lapply(cleaned_V, function(v) {
  #   attrs[extract_attribute_names(v, TRUE)]
  # })

  # Return a list with cleaned V, parameters and attributes
  list(
    V = cleaned_V,
    param = values[param_idx],
    attrs = attrs
  )
}
