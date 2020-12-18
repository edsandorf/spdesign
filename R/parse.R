#' Parse the utility functions
#'
#' The function parses the list of utilities to extract the relevant parameters
#' used to create the design.
#'
#' @param V A list of utility functions
#'
#' @return An object containing the outputs from the parsed utility functions, such
#' as the named vector of priors, the named list of attributes and their levels,
#' restrictions implied by the utility specifications
#'
#' @examples
#' U <- list(
#'   alt_1 = "beta_1 | c(0.1, 0.5) * x_1 | c(0, 1, 2) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8)",
#'   alt_2 = "beta_1 * x_1 + beta_2 * x_2"
#' )
#'
#'
#' @export
parse_utility <- function(V) {
  # Run a set of checks
  if (!is.list(V)) stop("'U' has to be a named list of utility functions. Please see the manual.")
  if (length(V) < 2) stop("'U' has to contain at least 2 utility functions. Please see the manual.")

  J <- length(V)

  # Split the utility for further processing





}


