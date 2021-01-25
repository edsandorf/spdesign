#' Make a design candidate
#'
#' The function is a call to \code{\link{switch}} and a wrapper around the
#' design algorithms \code{\link{random}}, \code{\link{federov}} and \code{\link{rsc}}.
#'
#' @param candidate_set A valid candidate set
#' @param opts A list of design options
#' @param J Number of alternatives per choice task
#' @param type One of the implemented types of optimization algorithms
#'
#' @return A matrix in wide format containing the design candidate
make_design_candidate <- function (candidate_set, opts, J, type) {
  switch(
    type,
    random = random(candidate_set, opts, J),
    federov = federov(candidate_set, opts, J),
    rsc = rsc(candidate_set, opts, J)
  )
}

#' Make a random design candidate
#'
#' @inheritParams make_design_candidate
random <- function(candidate_set, opts, J) {
  # Somewhere in this function, I would need to set the attribute levels of a
  # non-included attribute to 0 to make the derivates work out later
  candidates <- opts$tasks * opts$blocks * J
  split_idx <- shuffle(rep(seq_len(J), (opts$tasks * opts$blocks)))
  candidate_idx <- split(sample(seq_len(nrow(candidate_set)), candidates), split_idx)
  do.call(
    cbind,
    lapply(seq_len(J), function(j) {
      candidate_subset <- candidate_set[candidate_idx[[j]], ]
      names(candidate_subset) <- paste(names(candidate_subset), j, sep = "_")
      candidate_subset
    })
  )
}

#' Make a design candidate based on the modified federov algorithm
#'
#' @inheritParams make_design_candidate
federov <- function(candidate_set, opts, J) {
  stop("The modified federov algorithm has not been implemented yet.")
}

#' Make a design candidate based on the rsc algorithm
#'
#' @inheritParams make_design_candidate
rsc <- function(candidate_set, opts, J) {
  stop("The RSC algorithm has not been implemented yet.")
}
