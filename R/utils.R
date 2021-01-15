#' Tests whether a utility function is balanced
#'
#' Tests whether there is an equal number of opening and closing brackets in
#' the utility functions.
#'
#' @param string A character string
#' @param open An opening bracket ( [ or <
#' @param clos A closing bracket ) ] or >
#'
#' @noRd

is_balanced <- function(string, open, close) {
  opened <- str_count(string, paste0("\\", open))
  closed <- str_count(string, paste0("\\", close))
  if (opened != closed) {
    FALSE
  } else {
    TRUE
  }
}
