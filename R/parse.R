#' Parse the utility functions
#'
#' Parse the list of utility functions to extract the relevant priors, attribute
#' levels and other information.
#'
#' The function is exported, but should not be used.

#' @inheritParams generate_design
#'
#' @return A parsed utility expression of class 'utility'. The object is a list
#' with the following list-elements: A cleaned utility expression, a utility
#' formula object, a named vector of priors, a named list of attribute levels in
#' the wide format, a vector of generic attribute names, and an expanded list of
#' attribute levels with attribute level occurences.
#'
#' @export
parse_utility <- function(utility, tasks) {
  # Extract prior and attribute values
  prior_and_attr_values <- extract_named_values(utility)
  prior_and_attr_names <- names(prior_and_attr_values)

  # Check that all priors and attributes have associated values
  all_prior_and_attr_names <- unique(
    remove_whitespace(
      extract_all_names(utility, TRUE)
    )
  )

  if (!all(all_prior_and_attr_names %in% prior_and_attr_names)) {
    idx_missing <- which((all_prior_and_attr_names %in% prior_and_attr_names) == FALSE)
    missing_values <- paste0("'",
                             all_prior_and_attr_names[idx_missing],
                             "'",
                             collapse = " ")

    stop(
      paste0(
        missing_values,
        " does not have a specified prior or levels. Please make sure that all
        elements of the utility functions have been specified with a prior or
        level once."
      )
    )
  }

  # Check for duplicate specifications of attribute levels or priors
  if (any(duplicated(prior_and_attr_names))) {
    idx_duplicates <- duplicated(prior_and_attr_names)

    duplicate_values <- paste0(
      "'",
      prior_and_attr_names[idx_duplicates],
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

    prior_and_attr_values <- prior_and_attr_values[!idx_duplicates]
    prior_and_attr_names <- prior_and_attr_names[!idx_duplicates]
  }

  # Prepare priors
  idx_prior <- prior_and_attr_names %in% grep("b_",
                                              prior_and_attr_names,
                                              value = TRUE)

  prior_values <- prior_and_attr_values[idx_prior]

  # Check degrees of freedom
  if ((tasks * (length(utility) - 1)) < length(prior_values)) {
    stop(
      "The design is too small to identify all main effects. Not enough degrees
      of freedom. This will be turned into a warning later when we have decided
      how to proceed."
    )
  }

  # Get a clean utility expression
  utility_clean <- lapply(seq_along(utility), function(j, utility) {
    # Remove all brackets and replace multiple spaces with a single space
    v_j <- str_replace_all(
      remove_all_brackets(utility[[j]]),
      "\\s+",
      " "
    )

    attribute_names <- extract_attribute_names(v_j)
    for (i in seq_along(attribute_names)) {
      v_j <- str_replace_all(
        v_j,
        paste0("\\b", attribute_names[i]),
        paste(names(utility[j]), attribute_names[i], sep = "_")
      )
    }

    return(
      v_j
    )
  }, utility)

  # Restore the names that were dropped when cleaning utility
  names(utility_clean) <- names(utility)

  # Define a utility formula such that we can create the correct model.matrix
  utility_formula <- lapply(utility_clean, function(u, prior_names) {
    for (x in prior_names) {
      u <- remove_prior(x, u)
    }

    return(
      as.formula(paste0("~ 0 +", u))
    )

  }, prior_names = names(prior_values))

  # Prepare the attribute levels
  attribute_levels <- prior_and_attr_values[!idx_prior]
  attribute_names <- names(attribute_levels)

  # Expand the list of attributes to the wide format
  attribute_levels <- lapply(seq_along(utility), function(j,
                                                          attribute_levels,
                                                          attribute_names) {
    # Set all initial levels to 0 because we need square matrices but 0 values
    # for the alternative specific attributes in alternatives where they are not
    # included
    attribute_levels_tmp <- lapply(seq_along(attribute_names), function(i) {
      return(0)
    })

    names(attribute_levels_tmp) <- attribute_names

    # Replace with the attribute levels of the alternative. This considers
    # alternative specific attributes
    attribute_names <- extract_attribute_names(utility[[j]])
    attribute_levels_tmp[attribute_names] <- attribute_levels[attribute_names]

    # Add names and return
    names(attribute_levels_tmp) <- paste(
      names(utility[j]),
      attribute_names,
      sep = "_"
    )

    return(
      attribute_levels_tmp
    )
  }, attribute_levels, attribute_names)

  # Reduce to a single list of attribute levels
  attribute_levels <- do.call(c, attribute_levels)

  # Attribute level occurrence
  attribute_level_occurrence <- get_level_occurrence(
    utility,
    attribute_levels,
    tasks
  )

  # Create the utility object, assign the class and return
  utility <- list(
    utility_clean = utility_clean,
    utility_formula = utility_formula,
    prior_values = prior_values,
    attribute_levels = attribute_levels,
    attribute_names = attribute_names,
    attribute_level_occurrence = attribute_level_occurrence
  )

  class(utility) <- "utility"
  return(
    utility
  )
}
