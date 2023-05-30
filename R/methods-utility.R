#' Generic for getting the attributes and levels from the utility
#'
#' @param x An object of class utility
#'
#' @export
attribute_levels <- function(x) {
  # Extract all named values from the utility expression returned as a list
  named_values <- extract_named_values(x)

  # Create the index and filter
  idx <- !(names(named_values) %in% grep("b_", names(named_values), value = TRUE))

  return(
    named_values[idx]
  )
}

#' Expand the list of attributes and levels to the "wide" format
#'
#' Expands the attributes and levels to the wide format. The nested list is
#' padded with zeros where alternative specific attributes are present to ensure
#' that we can work with square matrices.
#'
#' @inheritParams attribute_levels
#'
#' @return A named vector
#'
#' @export
expand_attribute_levels <- function(x) {
  # Extract the specified attribute levels
  named_attributes <- attribute_levels(x)

  expanded_attributes <- lapply(seq_along(x), function(j) {
    # Create a full length list with 0-elements
    named_attributes_all <- lapply(seq_along(named_attributes), function(y) return(0))
    names(named_attributes_all) <- names(named_attributes)

    # Replace the attribute levels present in the alternative
    attribute_names_j <- extract_attribute_names(x[[j]])
    named_attributes_all[attribute_names_j] <- named_attributes[attribute_names_j]

    # Add alternative to the attribute names
    names(named_attributes_all) <- paste(names(x[j]), names(named_attributes), sep = "_")

    return(
      named_attributes_all
    )
  })

  # Return a single vector
  return(
    do.call(c, expanded_attributes)
  )
}

#' Cleans the utility expression
#'
#' The function cleans the utility expression by removing extra white spaces,
#' removes brackets and other information to return a clean, easy-to-read
#' expression
#'
#' @inheritParams attribute_levels
#'
#' @return A cleaned utility function as a list
#'
#' @export
clean_utility <- function(x) {

  utility <- lapply(seq_along(x), function(j) {
    v_j <- remove_all_brackets(x[[j]])
    v_j <- str_replace_all(v_j, "\\s+", " ")

    attribute_names <- extract_attribute_names(v_j)
    for (i in seq_along(attribute_names)) {
      v_j <- str_replace_all(v_j,
                             paste0("\\b", attribute_names[[i]]),
                             paste(names(x[j]), attribute_names[i], sep = "_"))
    }

    return(v_j)
  })

  names(utility) <- names(x)

  return(utility)
}

#' Create formulas from the utility functions
#'
#' Create formulas from the utility functions such that we can create correct
#' model matrices
#'
#' @inheritParams attribute_levels
#'
#' @return A list of formula expressions for the utility functions
#'
#' @export
utility_formula <- function(x) {
  names_priors <- names(priors(x))

  # Remove the prior from the cleaned utility expression
  return(
    lapply(clean_utility(x), function(v) {
      # Using a loop to iteratively overwrite v
      for (p in names_priors) {
        v <- remove_prior(p, v)
      }

      return(
        as.formula(paste0("~ 0 + ", v))
      )
    })
  )
}


#' Generic for extracting the vector of priors
#'
#' @param x An object of class 'utility' or 'spdesign'
#'
#' @return A list of named priors used in the optimization
#'
#' @export
priors <- function(x) {
  # Check the class of 'x' to return early if 'spdesign'
  if ("spdesign" %in% class(x)) {
    return(
      x[["priors"]]
    )
  }

  # Extract all named values from the utility expression returned as a list
  named_values <- extract_named_values(x)

  # Create the index and filter
  idx <- names(named_values) %in% grep("b_", names(named_values), value = TRUE)

  return(
    named_values[idx]
  )
}

#' Generic for extracting the attribute occurrence
#'
#' The function is called from within \code{\link{parse_utility}} with the
#' purpose of getting the list of attributes, levels and occurrences within the
#' design. This is necessary to impose attribute level balance or near attribute
#' level balance. This is also necessary to correctly populate the candidate
#' matrix used for the RSC algorithm.
#'
#' @inheritParams priors
#' @param rows Number of rows in the design
#'
#' @return A named list of lists where the outer list is for the attributes and
#' the inner list, the levels of each attribute and the number or range of times
#' they can occur
#'
#' @export
occurrences <- function(x, rows = NULL) {
  # Check the class of 'x' to return early if 'spdesign'
  if ("spdesign" %in% class(x)) {
    return(
      x[["occurrence"]]
    )
  }

  # Extract level occurrence
  attribute_lvls <- expand_attribute_levels(x)
  n_lvls <- lapply(attribute_lvls, length)

  # Set the default level occurrence assuming utility balance.
  level_occurrences <- lapply(n_lvls, function(n) {
    # If unbalanced and unspecified, set minimum range for occurrence
    if (rows %% n == 0) {
      lvl <- rows / n

    } else {
      minimum <- floor(rows / n)
      lvl <- minimum:(minimum + 1)

    }

    lvls <- lapply(seq_len(n), function(i) lvl)
    names(lvls) <- paste0("lvl", seq_along(lvls))

    return(lvls)
  })

  # Create a list to populate with occurrences
  specified_values <- extract_specified(x, simplify = TRUE)
  idx <- str_detect(specified_values, "(?<=\\])\\(.*?\\)")
  occurrences <- vector(mode = "list", sum(idx))
  names(occurrences) <- extract_attribute_names(specified_values[idx])

  # Transform the level occurrences to a list and if a single value, expand.
  for (i in seq_along(occurrences)) {
    occurrence <- extract_level_occurrence(specified_values[idx][[i]])
    lvls <- eval(parse(text = paste0("list", occurrence)))

    # Check if it only occurs once and repeat equal to the number of levels
    if (length(lvls) == 1) {
      # DANGER HERE: We are only using the first match. This will mean that
      # different levels for the same attribute in different alternatives won't
      # work.
      lvls <- rep(lvls, length(attribute_lvls[[grep(paste0("\\b.*?", names(occurrences)[[i]]), names(attribute_lvls))[[1]]]]))
    }

    names(lvls) <- paste0("lvl", seq_along(lvls))
    occurrences[[i]] <- lvls

  }

  # Expand to the wide format
  occurrences <- lapply(seq_along(occurrences), function(i) {
    occurrences_j <- lapply(seq_along(x), function(j) {
      if (str_detect(x[[j]], names(occurrences[i]))) {
        occurrences_tmp <- occurrences[i]
        names(occurrences_tmp) <- paste(names(x[j]), names(occurrences[i]), sep = "_")

        return(occurrences_tmp)

      }
    })

    return(
      do.call(c, occurrences_j)
    )
  })

  occurrences <- do.call(c, occurrences)

  # Replace the default values with the specified level occurrence
  level_occurrences[names(occurrences)] <- occurrences

  return(
    level_occurrences
  )

}


# Checks within the old level-occurrence function
# # Test that occurrence is specified once or equal to the number of levels
# n_lvl_occurrence <- lapply(level_occurrence, length)
# length_diff <- mapply(
#   "-",
#   n_attr_lvls,
#   n_lvl_occurrence,
#   SIMPLIFY = TRUE
# )
#
# # Check that the attribute level is only specified equal to the
# if (any(length_diff != 0)) {
#   stop(
#     "The number of level occurrences specified can only be of length one or
#         equal to the number of levels for the attribute"
#   )
# }
#
# tmp <- lapply(level_occurrence, function(x) {
#   sum_min <- Reduce("+", lapply(x, min))
#   sum_max <- Reduce("+", lapply(x, max))
#   !(candidate_rows %in% sum_min:sum_max)
# })
#
# if (any(do.call(c, tmp))) {
#   stop(
#     "The specified ranges for the attribute level occurrence or the number of
#       times an attribute level should occur does not allow you to find a design.
#       The number of rows in the design candidate must be either equal to the
#       sum of individual occurrences of an attribute level or contained in the
#       range of the sum of the minimum number of occurrences and the sum of the
#       maximum number of level occurrences."
#   )
# }
#
# # Implicitly return the list of level occurrences ----
# level_occurrence
