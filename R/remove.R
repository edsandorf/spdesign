#' Remove all white spaces
#'
#' Takes a string as an input and removes all whitespaces in the string
#'
#' @param string A character string
#'
#' @return A character vector with no white spaces
remove_whitespace <- function(string) {
  str_replace_all(string, "\\s", "")
}

#' Removes all brackets
#'
#' Takes a string as input and removes everything between square and round
#' brackets. The function wraps around \code{\link{remove_square_brackets}} and
#' \code{\link{remove_round_brackets}}. To avoid problems, we first remove
#' square brackets.
#'
#' @param string A character string
#'
#' @return A string without brackets
remove_all_brackets <- function(string) {
  # Remove square brackets before round brackets
  remove_round_brackets(
    remove_square_brackets(
      string
    )
  )
}

#' Remove square bracket
#'
#' Removes everything between (and including) square brackets
#'
#' @inheritParams remove_all_brackets
remove_square_brackets <- function(string) {
  str_replace_all(string, "\\[.*?\\]", "")
}

#' Remove round bracket
#'
#' Removes everything between (and including) round brackets. We negating
#' matches with I(), since this is R's interaction operator.
#'
#' (?<!I) - A negative lookbehind for I
#'
#' @inheritParams remove_all_brackets
remove_round_brackets <- function(string) {
  str_replace_all(string, "(?<!I)\\(.*?\\)", "")
}

#' Removes the parameter from the utility string
#'
#' @param prior A string with the parameter name
#' @param string A string to remove param from
remove_prior <- function(prior, string) {
  str_replace_all(string, paste0(prior, "(\\s)*?(\\*|\\/|\\+|\\-)"), "")
}
