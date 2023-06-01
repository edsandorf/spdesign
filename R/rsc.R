#' Make a design candidate based on the rsc algorithm
#'
#'
#'
#' Depending on the setting the function calls a combination of
#' \code{\link{relabel}}, \code{\link{swap}} and \code{\link{cycle}}
#' to create new design candidates. The code is intentionally written modular
#' to allow for all special cases of the algorithm.
#'
#' @inheritParams federov
#'
rsc <- function(design_object,
                model,
                efficiency_criteria,
                utility,
                prior_values,
                dudx,
                candidate_set,
                rows,
                control) {

  stop("The RSC algorithm has not been implemented yet")

  # # Create a new "random" rsc_candidate each 10 000 iterations
  # if ((iter_with_no_imp %% control$algorithm$reset) == 1) {
  #   current_design_candidate <- generate_rsc_candidate(parsed_utility, control)
  # }
  #
  # # Relabel
  # current_design_candidate <- relabel(current_design_candidate)
  #
  # # Swap
  # current_design_candidate <- swap(current_design_candidate)
  #
  # # Cycle
  #
  #
  # # Return
  # as.data.frame(current_design_candidate)
}

#' Relabeling of attribute levels
#'
#' Relabels the attribute levels to create a new design candidate. For example,
#' if the column contains the levels (1, 2, 1, 3, 2, 3) and 1 and 3 are
#' relabeled, then the column becomes (3, 2, 3, 1, 2, 1), i.e. 1 becomes 3 and
#' 3 becomes 1.
#'
#' Will randomly sample 2 attribute levels that will be relabeled and the
#' relabeling is done independently for each column, which implies that
#' the same attribute will be relabeled differently depending on which
#' alternative it belongs to.
#'
#' @param x A vector of attribute levels
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
relabel <- function(x) {
  lvls <- sample(unique(x), 2, replace = FALSE)
  idx_first <- x == lvls[[1]]
  idx_second <- x == lvls[[2]]

  x[idx_first] <- lvls[[2]]
  x[idx_second] <- lvls[[1]]

  return(x)
}

#' Swapping of attribute
#'
#' Swaps the order of the attributes to create a new design candidate. For
#' example, if the attributes in the first and fourth choice situation (row)
#' are swapped, then (1, 2, 1, 3, 2, 3) becomes( 3, 2, 1, 1, 2, 3).
#'
#' To increase the variation in the designs created, the swaps are independent
#' across attributes and alternatives
#'
#' @inheritParams relabel
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
swap <- function(current_design_candidate) {
  rows <- seq_len(nrow(current_design_candidate))

  # Loop over columns
  for (i in seq_len(ncol(current_design_candidate))) {
    swap_lvls <- sample(rows, 2L)
    order_idx <- swap_values(rows, swap_lvls)
    current_design_candidate[, i] <- current_design_candidate[order_idx, i]
  }

  # Return the row-swapped current design candidate
  current_design_candidate
}

#' Cycling of attribute levels
#'
#' Cycles the attribute levels to create a new design candidate. "Cycling
#' replaces all attribute levels in each choice situation at the tiem by
#' replacing the first level with the second level, second level with the third
#' etc. Since this change affects all columns, cycling can only be performed if
#' all attributes have exactly the same sets of feasible levels,
#' (e.g., where all variables are dummy coded)." (p. 253).
#'
#' This part of the RSC algorithm is rarely invoked.
#'
#' @inheritParams relabel
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
#'
#' @return A cycled design candidate
cycle <- function(current_design_candidate) {

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
#' @inheritParams federov
#'
#' @return A matrix with rows equal to the number of choice tasks and columns
#' equal to the number of attributes in the 'wide' format
generate_rsc_candidate <- function(utility, rows) {
  # Define attributes and level occurrence locally
  attribute_lvls <- expand_attribute_levels(utility)
  level_occurrence <- occurrences(utility, rows)

  # Loop over attributes
  design_candidate <- matrix(0, nrow = rows, ncol = length(attribute_lvls))
  colnames(design_candidate) <- names(attribute_lvls)

  for (i in seq_along(attribute_lvls)) {
    # Determine how many times to sample each level suject to summing to the
    # number of tasks
    constraints <- level_occurrence[[i]]
    sum_implied_tasks <- 1

    while (sum_implied_tasks != rows) {
      sample_level <- lapply(constraints, sample, 1L)
      sum_implied_tasks <- do.call(sum, sample_level)
    }

    # Sample each attribute level according to occurrence
    levels_tmp <- do.call(
      c,
      lapply(seq_along(sample_level), function(k) {
        rep(attribute_lvls[[i]][[k]], sample_level[[k]])
      })
    )

    design_candidate[, i] <- shuffle(levels_tmp)
  }

  design_candidate
}
