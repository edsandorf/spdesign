#' Get the cleaned utility functions
#'
#' @param object A utility function of class 'utility'
#' @param ... Additional parameters passed to the function
#'
#' @return A list of cleaned utility expressions
get_utility_clean <- function(object, ...) {
  return(
    object[["utility"]]
  )
}

#' Get the utility formula
#'
#' @inheritParams get_utility_clean
#'
#' @return A list with the utility formula
get_utility_formula <- function(object, ...) {
  return(
    object[["utility_formula"]]
  )
}

#' Get the prior values
#'
#' @inheritParams get_utility_clean
#'
#' @return A list with the prior values
get_prior_values <- function(object, ...) {
  return(
    object[["prior_values"]]
  )
}

#' Get the attribute levels
#'
#' @inheritParams get_utility_clean
#'
#' @return A list with the attribute levels
get_attribute_levels <- function(object, ...) {
  return(
    object[["attribute_levels"]]
  )
}

#' Get the attribute names
#'
#' @inheritParams get_utility_clean
#'
#' @return A list with the attribute names
get_attribute_names <- function(object, ...) {
  return(
    object[["attribute_names"]]
  )
}

#' Get the attribute level occurrence
#'
#' @inheritParams get_utility_clean
#'
#' @return A list with the attribute level occurrences
get_attribute_level_occurrence <- function(object, ...) {
  return(
    object[["attribute_level_occurrence"]]
  )
}
