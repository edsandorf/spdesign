#' Derive the variance covariance matrix of the design
#'
#' @param type A string indicating the model for which you wish to derive the
#' variance covariance matrix. Can be either "mnl" or "rpl"
derive_vcov <- function(type) {
  switch(
    type,
    mnl = derive_vcov_mnl(),
    rpl = derive_vcov_rpl()
  )
}

#' Derive the variance covariance matrix for the MNL model
#'
#' @inheritParams derive_vcov
#'
#' @return The variance covariance matrix. If the Fisher information matrix is
#' singular, then return NULL
#'
#' @noRd
derive_vcov_mnl <- function() {
  # Calculate the probability of j
  V <- lapply(V_string, function(v) eval(parse(text = v)))
  exp_V <- lapply(V, exp)
  sum_V <- Reduce("+", exp_V)
  pr_j <- lapply(exp_V, function(v) {
    v <- v / sum_V
    v[is.na(v)] <- 0
    as.vector(v)
  })

  # Multiply pr_j with x_j
  X <- lapply(V_string, function(s) {
    do.call(cbind, lapply(extract_attribute_names(s), get))
  })

  pr_x <- mapply("*", pr_j, X, SIMPLIFY = FALSE)
  sum_pr_x <- Reduce("+", pr_x)

  # Take the square root of the probabilities
  sqrt_pr_j <- lapply(pr_j, sqrt)

  # Take the difference between the attribute levels and the sum of the
  # probabilities of the alternatives
  pr_diff <- lapply(X, function(x) x - sum_pr_x)
  pr_diff <- mapply("*", pr_diff, sqrt_pr_j, SIMPLIFY = FALSE)

  tmp <- as.matrix(Reduce(rbind, pr_diff))
  fisher <- crossprod(tmp)

  # Return the variance-covariance matrix
  if (det(fisher) != 0) {
    solve(fisher)
  } else {
    NULL
  }
}

#' Derive the variance covariance matrix for the RPL model
#'
#' @inheritParams derive_vcov
#'
#' @noRd
derive_vcov_rpl <- function() {
  stop("Designs for the RPL model has not been implemented yet.")
}
