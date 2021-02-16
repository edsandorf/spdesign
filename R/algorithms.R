#' Make a design candidate
#'
#' The function is a call to \code{\link{switch}} and a wrapper around the
#' design algorithms \code{\link{random}}, \code{\link{federov}} and
#'  \code{\link{rsc}}.
#'
#' The purpose of passing in utility is two-fold:
#' 1) We can determine n_alts
#' 2) We can extract the attribute names for each utility function to allow us
#' to place the correct restrictions on the design candidate. Specifically, we
#' restrict all levels of unavailable attributes to zero for alternatives where
#' they do not feature. This is to ensure that we do not give weight when
#' deriving the variance-covariance matrix using \code{\link{derive_vcov}}.
#' Furthermore, the Xs are "sorted" using the order of the candidate set, which
#' ensures that when we calculate the sum of the probabilities times X, the
#' correct columns are added together. See \code{\link{derive_vcov}}.
#'
#' @param parsed_utility A list containing the elements of the parsed utility
#' expression obtained from \code{\link{parse_utility}}.
#' @param candidate_set A valid candidate set. This is only used for the
#' 'random' and 'federov' algorithms.
#' @param current_design_candidate A candidate for the RSC algorithm. To allow for marginal
#' changes the candidate is passed back in for each iteration.
#' @param opts A list of design options
#' @param iter_with_no_imp The iteration number
#' @param type One of the implemented types of optimization algorithms
#'
#' @return A matrix with the design candidate in the 'wide' format.
make_design_candidate <- function(
  parsed_utility,
  candidate_set,
  current_design_candidate,
  opts,
  iter_with_no_imp,
  type
  ) {
  switch(
    type,
    random = random(candidate_set, opts),
    federov = federov(candidate_set, opts),
    rsc = rsc(parsed_utility, current_design_candidate, opts, iter_with_no_imp)
  )
}

#' Make a random design candidate
#'
#' @inheritParams make_design_candidate
random <- function(candidate_set, opts) {
  # Some useful things
  # n_alts <- length(utility)
  # rows <- nrow(candidate_set)
  # col_names <- colnames(candidate_set)
  # candidates <- opts$tasks * n_alts
  # split_idx <- shuffle(rep(seq_len(n_alts), opts$tasks))
  # candidate_idx <- split(
  #   sample(seq_len(rows), candidates, replace = opts$sample_with_replacement),
  #   split_idx
  # )
  #
  # # Create the design candidate
  # design_candidate <- lapply(seq_len(n_alts), function(j) {
  #   candidate_subset <- candidate_set[candidate_idx[[j]], ]
  #   # Restrict unavailable attributes to zero to correctly derive vcov
  #   restrictions <- as.integer(
  #     col_names %in% extract_attribute_names(utility[[j]])
  #   )
  #   candidate_subset <- t(t(candidate_subset) * restrictions)
  #   colnames(candidate_subset) <- paste(colnames(candidate_subset), j,
  #                                       sep = "_")
  #   candidate_subset
  # })
  #
  # # Return as data.frame
  # names(design_candidate) <- paste0("alt_", seq_along(design_candidate))
  # design_candidate

  stop(
    "The 'random' algorithm has not been implemented yet."
  )
}

#' Make a design candidate based on the Modified Federov Algorithm
#'
#' @inheritParams make_design_candidate
federov <- function(candidate_set, opts) {
  rows <- nrow(candidate_set)
  candidate_idx <- sample(
    seq_len(rows),
    opts$tasks,
    replace = opts$sample_with_replacement
  )

  candidate_set[candidate_idx, ]
}

#' Make a design candidate based on the rsc algorithm
#'
#'
#' Depending on the setting the function calls a combination of
#' \code{\link{relabel}}, \code{\link{swap}} and \code{\link{cycle}}
#' to create new design candidates. The code is intentionally written modular
#' to allow for all special cases of the algorithm.
#'
#' @inheritParams make_design_candidate
rsc <- function(
  parsed_utility,
  current_design_candidate,
  opts,
  iter_with_no_imp
) {
  # Create a new "random" rsc_candidate each 10 000 iterations
  if ((iter_with_no_imp %% opts$algorithm$reset) == 1) {
    current_design_candidate <- generate_rsc_candidate(parsed_utility, opts)
  }

  # Relabel
  current_design_candidate <- relabel(current_design_candidate)

  # Swap
  current_design_candidate <- swap(current_design_candidate)

  # Cycle


  # Return
  current_design_candidate
}

#' Relabeling of attribute levels
#'
#' Relabels the attribute levels to create a new design candidate. For example,
#' if the column contains the levels (1, 2, 1, 3, 2, 3) and 1 and 3 are
#' relabeled, then the column becomes (3, 2, 3, 1, 2, 1), i.e. 1 becomes 3 and
#' 3 becomes 1.
#'
#' The relabeling is done independently for each column, which implies that
#' the same attribute will be relabeled differently depending on which
#' alternative it belongs to.
#'
#' @inheritParams make_design_candidate
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
relabel <- function(current_design_candidate) {
  # Loop over columns
  for (i in seq_len(ncol(current_design_candidate))) {
    column <- current_design_candidate[, i]
    unique_lvls <- unique(column)

    # We only need to relabel if the attribute in question has more than two
    # levels
    if (length(unique_lvls) > 1) {
      lvls_to_relabel <- sample(unique_lvls, 2L)
      column <- swap_values(column, lvls_to_relabel)
    }

    current_design_candidate[, i] <- column
  }

  # Return the new and relabeled design candidate
  current_design_candidate
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
#' @inheritParams make_design_candidate
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
#' @inheritParams make_design_candidate
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
#'
#' @return A cycled design candidate
cycle <- function(current_design_candidate) {

}
