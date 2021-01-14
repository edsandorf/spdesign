#' Remove all white spaces
#'
#' Takes a string as an input and removes all whitespaces in the string
#'
#' @param string A character string
#'
#' @return A character vector with no white spaces
#'
#' @noRd
remove_whitespace <- function(string) {
  str_replace_all(string, "\\s", "")
}

#' Removes the square bracket containing parameters and levels
#'
#' Takes a utility function (string) as an input and removes all prior and level
#' specifications. Effectively everything between [] including [].
#'
#' @return A string
#'
#' @noRd
remove_square_bracket <- function(string) {
  str_replace_all(string, "\\[.*?\\]", "")
}
