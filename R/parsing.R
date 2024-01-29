#' Generic for getting the attributes and levels from the utility function
#'
#' @param x An object of class utility
#'
#' @return A named list of attribute levels
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

#' Generic for getting the attribute names
#'
#' @inheritParams attribute_levels
#'
#' @return A character vector of attribute names
#'
#' @export
attribute_names <- function(x) {
  return(names(attribute_levels(x)))
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
#' expression.
#'
#' We can also use the side-effect of the function on a list of utility
#' expressions that do not contain brackets to return a an updated utility
#' expression with alternative specific attribute names.
#'
#' Warning: The function does not check if the utility expression *is* clean,
#' which means that running the function multiple times will result in
#' duplicate alternative names for the attributes. You need to pay particular
#' attention to this fact when using the formula \code{\link{update_utility}}
#' because this function calls \code{clean_utility}.
#'
#' @inheritParams attribute_levels
#'
#' @return A cleaned utility function as a list
#'
#' @export
clean_utility <- function(x) {
  # Create a cleaned utility expression
  v <- as.list(as.list(str_replace_all(remove_all_brackets(x), "\\s+", " ")))
  names(v) <- names(x)

  utility <- lapply(seq_along(v), function(j) {
    v_j <- v[[j]]

    attribute_names <- extract_attribute_names(v_j)
    for (i in seq_along(attribute_names)) {
      v_j <- str_replace_all(v_j,
                             paste0("\\b", attribute_names[[i]]),
                             paste(names(v[j]), attribute_names[i], sep = "_"))
    }

    return(v_j)
  })

  names(utility) <- names(x)

  return(utility)
}

#' Update the utility function
#'
#' Updates the utility function to consider dummy coded attributes. It will
#' expand the dummy-coding to K-1 dropping the lowest level. This is consistent
#' with standard practice.
#'
#' The function is called prior to evaluating designs if dummy-coded attributes
#' are present in the utility function. This is because the utility function
#' is evaluated in the context of the design environment and must be added there
#'
#' Important to note about the naming of the expanded priors and attributes:
#' The names for the attributes will be attached with the level of the factor,
#' whereas the prior will be named corresponding to the level, e.g., 2, 3, 4.
#' This is simply the result of the difference between how it's extracted from
#' the utility functions and how model.matrix creates names.
#'
#' @inheritParams attribute_levels
#'
#' @return An updated cleaned utility expression
#'
#' @export
update_utility <- function(x) {
  # Create a cleaned utility expression
  v <- as.list(as.list(str_replace_all(remove_all_brackets(x), "\\s+", " ")))

  # Extract the utility components and subset them to
  utility_components <- unlist(str_split(paste(unlist(x), collapse = " + "), "\\+"))
  expr_spec <- "[^\\s\\+\\-\\*\\/]*?\\[.*?\\](\\(.*?\\))?"
  expr_dumm <- "_dummy\\["
  utility_components <- utility_components[str_detect(utility_components, expr_spec) & str_detect(utility_components, expr_dumm)]

  # Expand the utility components
  for (u in utility_components) {
    lvls <- unlist(attribute_levels(u))[-1]
    prior <- str_extract(extract_param_names(u, TRUE), "^.*(?=(\\_dummy))")
    attr <- extract_attribute_names(u, TRUE)

    v_pattern <- str_trim(str_replace_all(remove_all_brackets(u), "\\s+", " "))
    v_pattern <- str_replace(v_pattern, "\\*", "\\\\*")
    v_replacement <- paste(paste(paste0(prior, 2:(length(lvls) + 1)), paste0(attr, lvls), sep = " * "), collapse = " + ")

    v <- str_replace_all(v, v_pattern, v_replacement)
  }

  v <- as.list(v)
  names(v) <- names(x)

  return(
    clean_utility(v)
  )

}

#' Create formulas from the utility functions
#'
#' Create formulas from the utility functions such that we can create correct
#' model matrices.
#'
#' Note that this function should be used on a cleaned utility expression and
#' **not** an updated utility expression. This is because we are converting
#' dummy coded attributes to factors prior to calling \code{\link{model.matrix}}.
#' This ensures that dummy coded attributes are correctly returned with the
#' model matrix.
#'
#' @inheritParams attribute_levels
#'
#' @return A list of formula expressions for the utility functions
#'
#' @export
utility_formula <- function(x) {
  names_priors <- unique(extract_param_names(x, TRUE))

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


#' Find the position of the dummy coded attributes
#'
#' The function will find the position of the dummy coded attributes in the
#' candidate set (in the case of the Modified Federov or Random algorithms) or
#' the design candidate (in the case of the RSC algorithm). This will let us
#' know which columns to coerce to factors prior to defining x_j.
#'
#' @inheritParams attribute_levels
#'
#' @return A boolean vector matching the expanded utility expression
dummy_names <- function(x) {
  # Get the elements assuming that the function is additive
  elements <- unlist(
    str_split(paste(unlist(clean_utility(x)), collapse = " + "), "\\+")
  )

  pos <- str_detect(elements, "_dummy\\s")


  return(
    remove_whitespace(str_extract(elements[pos], "(?<=(\\*|\\+)).*\\b"))
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

#' Extract or set attribute level occurrences
#'
#' This function will set the range of attribute level occurrences equal to to
#' the size of the design. This is equivalent to fully letting go of attribute
#' level balance. Letting go of attribute level balance is the default behavior
#' for the Modified Federov algorithm and the Random algorithm.
#'
#' If restrictions are placed on attribute level occurrence in the utility
#' function, then this function will extract these and add them to the output.
#'
#' Notice that specifying restrictions in the utility function only matters for
#' the Modified Federov and Random algorithms and will in general result in a
#' less efficient design.
#'
#' @inheritParams priors
#' @param rows Number of rows in the design
#'
#' @return A named list of lists where the outer list is for the attributes and
#' the inner list, the levels of each attribute and the number or range of times
#' they can occur
#'
#' @export
occurrences <- function(x, rows) {
  # Extract level occurrence
  attribute_lvls <- expand_attribute_levels(x)
  n_lvls <- lapply(attribute_lvls, length)

  # Set the default level occurrence such that each level can occur a minimum of
  # 0 times and a maximum of row number of times.
  level_occurrences <- lapply(n_lvls, function(n) {
    # If unbalanced and unspecified, set minimum range for occurrence
    # if (rows %% n == 0) {
    #   lvl <- rows / n
    #
    # } else {
    #   minimum <- floor(rows / n)
    #   lvl <- minimum:(minimum + 1)
    #
    # }

    lvls <- lapply(seq_len(n), function(i) c(0, seq_len(rows)))
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

#' Find minimum level occurrences
#'
#' Find minimium level occurrences. This is useful to ensure/approximate attribute
#' level balance in designs using the Modified Federov Algorithm or the
#' Random design algorithms.
#'
#' @inheritParams occurrences
#'
#' @return A list of minimum level occurrences for the attribute levels
min_lvl_occurrence <- function(x, rows) {
  return(
    lapply(expand_attribute_levels(x), function(y) floor(rows / length(y)))
  )
}

#' Find the number of levels
#'
#' Find the number of levels for each attribute
#'
#' @inheritParams occurrences
#'
#' @return A list with the number of levels for each attribute
nlvls <- function(x) {
  return(
    lapply(expand_attribute_levels(x), length)
  )
}

#' Attribute level occurrence lookup tables
#'
#' Creates a list of lookup tables for attribute level occurrence.
#'
#' @inheritParams generate_design
#' @param level_balance Boolean equal to TRUE if level balance. This is not used
#'
#' @return A list the length of the expanced attribute levels. Each list element
#' is a lookup table where the names of the table is the attribute level and the
#' element the number of times the minimum number of times the level occurs.
lvl_occurrences <- function(utility, rows, level_balance) {
  return(
    mapply(function(x, y, level_balance) {
      # Check if we are enforcing level balance.
      if (level_balance) {
        z <- rep(x, length(y))

      } else {
        z <- rep(1, length(y))

      }

      names(z) <- y
      return(z)
    }, min_lvl_occurrence(utility, rows), expand_attribute_levels(utility), level_balance, SIMPLIFY = FALSE)
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
