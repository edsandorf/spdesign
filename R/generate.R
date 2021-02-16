#' Generate the full factorial
#'
#' \code{make_full_factorial} is a wrapper around \code{\link{expand.grid}} and
#' generates the full factorial given the supplied attributes. The attributes
#' can either be specified directly by the user or extracted from the list
#' of utility functions using \code{\link{parse_utility}}.
#'
#' The full factorial is often used as the starting point to generate a
#' candidate set. Note that the full factorial will include unrealistic and
#' completely dominated alternatives. It is therefore advised to use a subset
#' of the full factorial as a candidate set. The user can call
#' \code{generate_full_factorial} and create a subset that is passed to
#' \code{\link{generate_design}} using the `candidate_set` parameter, or supply
#' a set of restrictions through the list of design options `design_opt`.
#'
#' The function is mainly used inside \code{\link{generate_design}} and
#'  appropriate restrictions are then put in place.
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
#' generate_full_factorial(attrs)
#'
#' V <- list(
#'   alt1 = "b_a1[0.1] * a1[1:5] + b_a2[-2] * a2[c(0, 1)]",
#'   alt2 = "b_a1      * a1      + b_a2     * a2"
#' )
#'
#' attrs <- parse_utility(V, opts)$attrs
#' generate_full_factorial(attrs)
#'
#' @export
generate_full_factorial <- function(attrs) {
  expand.grid(attrs)
}

#' Generates a candidate for the RSC algorithm
#'
#' The function will populate the design candidate matrix used for the RSC
#' algorithm.
#'
#' To ensure attribute level balance or that the number of times an attribute
#' level occurs is within the range specified in the list level_occurrence,
#' we sample from the list of occurrences subject to the constraint that the
#' sum has to be equal to the number of rows, i.e. choice tasks. While this is
#' a theoretical bottle neck for very large designs, it is still sufficiently
#' fast. It is only run once every 10 000 iterations of the RSC algorithm.
#'
#' @param parsed_utility A named list of the parsed utility expression
#' @param opts A list of options
#'
#' @return A matrix with rows equal to the number of choice tasks and columns
#' equal to the number of attributes in the 'wide' format
generate_rsc_candidate <- function(parsed_utility, opts) {
  # Define attributes and level occurrence locally
  attrs <- parsed_utility[["attrs"]]
  level_occurrence <- parsed_utility[["level_occurrence"]]

  # Loop over attributes
  design_candidate <- matrix(0, nrow = opts$tasks, ncol = length(attrs))
  colnames(design_candidate) <- names(attrs)

  for (i in seq_along(attrs)) {
    # Determine how many times to sample each level suject to summing to the
    # number of tasks
    constraints <- level_occurrence[[i]]
    sum_implied_tasks <- 1
    while (sum_implied_tasks != opts$tasks) {
      sample_level <- lapply(constraints, sample, 1L)
      sum_implied_tasks <- do.call(sum, sample_level)
    }

    # Sample each attribute level according to occurrence
    levels_tmp <- do.call(
      c,
      lapply(seq_along(sample_level), function(k) {
        rep(attrs[[i]][[k]], sample_level[[k]])
      })
    )

    design_candidate[, i] <- shuffle(levels_tmp)
  }

  design_candidate
}
