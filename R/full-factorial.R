#' Generate the full factorial
#'
#' This function is deprecated and will be removed in a future version. Use
#' \code{\link{build_candidate_set}} to build a candidate set from the utility
#' functions, or \code{\link{expand.grid}} to create the full factorial of a
#' list of attributes with levels that are different from those in the utility
#' functions.
#'
#' The function is a wrapper around \code{\link{expand.grid}} and generates the
#' full factorial given the supplied attributes.
#'
#' @param attrs A named list of attributes and their levels
#'
#' @return A data frame containing the full factorial
#'
#' @examples
#' attrs <- list(
#'   a1 = 1:5,
#'   a2 = c(0, 1)
#' )
#'
#' # Instead of full_factorial(attrs)
#' expand.grid(attrs)
#'
#' @export
full_factorial <- function(attrs) {
  .Deprecated(
    "build_candidate_set",
    msg = paste(
      "full_factorial() is deprecated. Use build_candidate_set() to build a",
      "candidate set from the utility functions, or expand.grid() for a list",
      "of attributes."
    )
  )

  expand.grid(attrs)
}
