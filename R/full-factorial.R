#' Generate the full factorial
#'
#' \code{full_factorial} is a wrapper around \code{\link{expand.grid}} to generate
#' the full factorial design implied by the specified utility functions and options
#' in \code{design_opt}. The full factorial is often used as a starting point to
#' generate a candidate set and will include unrealistic, identical and completely
#' dominated alternatives. When used as a starting point, it is up to the user
#' to ensure that all combinations of attributes and levels are permitted.
#'
#' The main use of the function is within \code{\link{design}} and appropriate
#' restrictions are then put in place. In addition, any restrictions specified by
#' the user are also applied.
#'
#' @param U A parsed utility object
#' @param design_opt The list of design options
#'
#' @export
full_factorial <- function(U, design_opt) {
  # Check that U is a parsed utility object
}
