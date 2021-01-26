#' Make a design candidate
#'
#' The function is a call to \code{\link{switch}} and a wrapper around the
#' design algorithms \code{\link{random}}, \code{\link{federov}} and \code{\link{rsc}}.
#'
#' The purpose of passing in V is two-fold:
#' 1) We can determine J
#' 2) We can extract the attribute names for each utility function to allow us to
#' place the correct restrictions on the design candidate. Specifically, we restrict
#' all levels of unavailable attributes to zero for alternatives where they do not
#' feature. This is to ensure that we do not give weight when deriving the
#' variance-covariance matrix using \code{\link{derive_vcov}}. Furthermore,
#' the Xs are "sorted" using the order of the candidate set, which ensures that
#' when we calculate the sum of the probabilities times X, the correct columns
#' are added together. See \code{\link{derive_vcov}}.
#'
#' @param candidate_set A valid candidate set
#' @param opts A list of design options
#' @param V The list of utility expressions
#' @param type One of the implemented types of optimization algorithms
#'
#' @return A list the length of J with the design matrix for each alternative
make_design_candidate <- function (candidate_set, opts, V, type) {
  switch(
    type,
    random = random(candidate_set, opts, V),
    federov = federov(candidate_set, opts, V),
    rsc = rsc(candidate_set, opts, V)
  )
}

#' Make a random design candidate
#'
#' @inheritParams make_design_candidate
random <- function(candidate_set, opts, V) {
  J <- length(V)
  candidates <- opts$tasks * opts$blocks * J
  split_idx <- shuffle(rep(seq_len(J), (opts$tasks * opts$blocks)))
  candidate_idx <- split(sample(seq_len(nrow(candidate_set)), candidates), split_idx)
  names_string <- colnames(candidate_set)
  design_candidate <- lapply(seq_len(J), function(j) {
      candidate_subset <- candidate_set[candidate_idx[[j]], ]
      # Restrict unavailable attributes to zero. Crucial to correctly derive the vcov. Using t() converts to matrix
      candidate_subset <- t(t(candidate_subset) * +(names_string %in% extract_attribute_names(V[[j]]))) # +() forces logical to integer
      colnames(candidate_subset) <- paste(colnames(candidate_subset), j, sep = "_")
      candidate_subset
    })

  # Return as data.frame
  design_candidate
}

#' Make a design candidate based on the modified federov algorithm
#'
#' @inheritParams make_design_candidate
federov <- function(candidate_set, opts, V) {
  stop("The modified federov algorithm has not been implemented yet.")
}

#' Make a design candidate based on the rsc algorithm
#'
#' @inheritParams make_design_candidate
rsc <- function(candidate_set, opts, V) {
  stop("The RSC algorithm has not been implemented yet.")
}
