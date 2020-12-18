#' Remove all white spaces
#'
#' Takes a string as an input and removes all whitespaces in the string
#'
#' @param string A character string
#'
#' @return A character vector with no white spaces
remove_whitespace <- function(string) {
  if (!is.character(string)) stop("Input must be a character vector")
  str_replace_all(string, "\\s", "")
}
