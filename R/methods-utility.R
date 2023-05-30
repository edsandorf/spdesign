#' S3 Generic for printing the utility object
#'
#' @param x A 'utility' object
#' @param ... Other parameters passed to print
#'
#' @method print utility
#'
#' @export
print.utility <- function(x, ...) {
  cat("---------------------------------------------------------------------\n")

  cat("\n\n")
  cat("---------------------------------------------------------------------\n")
}


#' Generic for getting the attributes and levels from the utility
#'
#'
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
#' @return A nested list where the top level is the alternative and the nested
#' levels are the attributes and levels.
expand_attribute_levels <- function() {

}

#' Cleans the utility expression
#'
#' The function cleans the utility expression by removing extra white spaces,
#' removes brackets and other information to return a clean, easy-to-read
#' expression
#'
#' @param x An object of class utility
#'
#' @return A cleaned utility function as a list
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
#' @return A list of formula expressions for the utility functions
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
#' @param ... Additional parameters passed to the function. These are really
#' only used internally on objects of class 'utility'ß
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
#' @inheritParams priors
#'
#'
#' @export
occurrences <- function(x, ...) {
  # Check the class of 'x' to return early if 'spdesign'
  if ("spdesign" %in% class(x)) {
    return(
      x[["occurrence"]]
    )
  }

  # Extract level occurrence

}
