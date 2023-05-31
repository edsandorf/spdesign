#' Get the cleaned utility functions
#'
#' @param object A utility function of class 'utility'
#' @param ... Additional parameters passed to the function
#'
#' @return A list of cleaned utility expressions
get_utility <- function(object, ...) {
  return(
    object[["utility"]]
  )
}

#' Get the utility formula
#'
#' @inheritParams get_utility
#'
#' @return A list with the utility formula
get_formula <- function(object, ...) {
  return(
    object[["formula"]]
  )
}


#' Get the attribute levels
#'
#' @inheritParams get_utility
#'
#' @return A list with the attribute levels
get_attribute_levels <- function(object, ...) {
  return(
    object[["levels"]]
  )
}

#' Get the attribute names
#'
#' @inheritParams get_utility
#'
#' @return A list with the attribute names
get_attribute_names <- function(object, ...) {
  return(
    object[["names"]]
  )
}

#' Get the attribute level occurrence
#'
#' @inheritParams get_utility
#'
#' @return A list with the attribute level occurrences
get_attribute_level_occurrence <- function(object, ...) {
  return(
    object[["occurrence"]]
  )
}
