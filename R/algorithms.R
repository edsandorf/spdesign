#' Make a design candidate
#'
#' The function is a call to \code{\link{switch}} and a wrapper around the
#' design algorithms \code{\link{random}}, \code{\link{federov}} and
#'  \code{\link{rsc}}.
#'
#' The purpose of passing in utility is two-fold:
#' 1) We can determine nalts
#' 2) We can extract the attribute names for each utility function to allow us
#' to place the correct restrictions on the design candidate. Specifically, we
#' restrict all levels of unavailable attributes to zero for alternatives where
#' they do not feature. This is to ensure that we do not give weight when
#' deriving the variance-covariance matrix using \code{\link{derive_vcov}}.
#' Furthermore, the Xs are "sorted" using the order of the candidate set, which
#' ensures that when we calculate the sum of the probabilities times X, the
#' correct columns are added together. See \code{\link{derive_vcov}}.
#'
#' @param candidate_set A valid candidate set
#' @param opts A list of design options
#' @param utility The list of utility expressions
#' @param type One of the implemented types of optimization algorithms
#'
#' @return A list the length of nalts with the design matrix for each
#' alternative
make_design_candidate <- function(candidate_set, opts, utility, type) {
  switch(
    type,
    random = random(candidate_set, opts, utility),
    federov = federov(candidate_set, opts, utility),
    rsc = rsc(candidate_set, opts, utility)
  )
}

#' Make a random design candidate
#'
#' @inheritParams make_design_candidate
random <- function(candidate_set, opts, utility) {
  # Some useful things
  nalts <- length(utility)
  rows <- nrow(candidate_set)
  col_names <- colnames(candidate_set)
  candidates <- opts$tasks * opts$blocks * nalts
  split_idx <- shuffle(rep(seq_len(nalts), (opts$tasks * opts$blocks)))
  candidate_idx <- split(
    sample(seq_len(rows), candidates, replace = opts$sample_with_replacement),
    split_idx
  )

  # Create the design candidate
  design_candidate <- lapply(seq_len(nalts), function(j) {
    candidate_subset <- candidate_set[candidate_idx[[j]], ]
    # Restrict unavailable attributes to zero to correctly derive vcov
    restrictions <- +(col_names %in% extract_attribute_names(utility[[j]]))
    candidate_subset <- t(t(candidate_subset) * restrictions)
    colnames(candidate_subset) <- paste(colnames(candidate_subset), j,
                                        sep = "_")
    candidate_subset
  })

  # Return as data.frame
  names(design_candidate) <- paste0("alt_", seq_along(design_candidate))
  design_candidate
}

#' Make a design candidate based on the modified federov algorithm
#'
#' @inheritParams make_design_candidate
federov <- function(candidate_set, opts, utility) {
  stop(
    "The modified federov algorithm has not been implemented yet."
  )
}

#' Make a design candidate based on the rsc algorithm
#'
#' @inheritParams make_design_candidate
rsc <- function(candidate_set, opts, utility) {
  stop(
    "The RSC algorithm has not been implemented yet."
  )
}
