#' Generate the full factorial
#'
#' The function is a wrapper around \code{\link{expand.grid}} and
#' generates the full factorial given the supplied attributes. The attributes
#' can either be specified directly by the user or extracted from the list
#' of utility functions using.
#'
#' The full factorial is often used as the starting point to generate a
#' candidate set. Note that the full factorial will include unrealistic and
#' completely dominated alternatives. It is therefore advised to use a subset
#' of the full factorial as a candidate set. The user can call
#' \code{full_factorial} and create a subset that is passed to
#' \code{\link{generate_design}} using the `candidate_set` parameter, or supply
#' a set of restrictions through the `restrictions` argument.
#'
#' @param attrs A named list of attributes and their levels
#'
#' @return A matrix containing the full factorial
#'
#' @examples
#' opts <- list(
#'   level_balance = FALSE,
#'   tasks = 10
#' )
#' attrs <- list(
#'   a1 = 1:5,
#'   a2 = c(0, 1)
#' )
#'
#' full_factorial(attrs)
#'
#' V <- list(
#'   alt1 = "b_a1[0.1] * a1[1:5] + b_a2[-2] * a2[c(0, 1)]",
#'   alt2 = "b_a1      * a1      + b_a2     * a2"
#' )
#'
#' attrs <- expand_attribute_levels(V)
#' full_factorial(attrs)
#'
#' @export
full_factorial <- function(attrs) {
  expand.grid(attrs)
}
