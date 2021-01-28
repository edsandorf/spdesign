#' Parse the utility functions
#'
#' The function parses the list of utilities to extract the relevant parameters
#' used to create the design.
#'
#' @param utility A list of utility functions
#'
#' @return
#' A list with three elements:
#' 1) The returned cleaned expression of the utility function is ready to be
#' evaluated in the design context
#' 2) A list of parameters
#' 3) A list of attributes entering each alternative. For example, for two
#' alternatives, it will return a list of length, with each list element
#' containing the list of attributes entering that alternative's utility
#' function. The purpose is to correctly handle alternative specific variables
#' and return a list that can be directly passed to create_full_factorial.
#'
#'@export
parse_utility <- function(utility) {
  # Extract useful information ----
  all_names <- unique(remove_whitespace(extract_all_names(utility, TRUE)))
  values <- extract_named_values(utility)
  value_names <- names(values)


  # Dummy coding ----
  if (any(str_detect(utility, "_dummy"))) {

  }

  if (!all(all_names %in% value_names)) {
    missing_idx <- which((all_names %in% value_names) == FALSE)
    missing_values <- paste0("'", all_names[missing_idx], "'", collapse = " ")
    stop(
      paste0(
        missing_values,
        " does not have a specified prior or levels. Please make sure that all
        elements of the utility functions have been specified with a prior or
        level once."
      )
    )
  }

  # Check whether a parameter or attribute is specified more than once
  if (any(duplicated(value_names))) {
    duplicate_idx <- duplicated(value_names)
    duplicate_values <- paste0(
      "'",
      value_names[duplicate_idx],
      "'",
      collapse = " "
    )
    warning(
      paste0(
        duplicate_values,
        " are specified with priors or levels more than once. Only the first
        occurance of the value is used. If you intended to use different levels
        for different attributes in each utility function, please specify
        alternative specific attributes.\n "
      )
    )
    values <- values[!duplicate_idx]
    value_names <- value_names[!duplicate_idx]
  }

  # Prepare attributes
  param_idx <- value_names %in% grep("b_", value_names, value = TRUE)
  cleaned_utility <- lapply(seq_along(utility), function(j) {
    v <- str_replace_all(remove_square_bracket(utility[[j]]), "\\s+", " ")
    attribute_names <- extract_attribute_names(v)
    for (i in seq_along(attribute_names)) {
      v <- str_replace_all(
        v,
        attribute_names[i],
        paste(attribute_names[i], j, sep = "_")
      )
    }
    # Return v
    v
  })
  # Restore names that were dropped when cleaning utility
  names(cleaned_utility) <- names(utility)
  attrs <- values[!param_idx]
  # This next step may not be necessary
  # attrs <- lapply(cleaned_utility, function(v) {
  #   attrs[extract_attribute_names(v, TRUE)]
  # })

  # Return a list with cleaned utility, parameters and attributes
  list(
    utility = cleaned_utility,
    param = values[param_idx],
    attrs = attrs
  )
}
