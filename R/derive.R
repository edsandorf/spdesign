#' Derive the variance covariance matrix of the design
#'
#' The function is a wrapper around \code{\link{derive_vcov_mnl}} and
#' \code{\link{derive_vcov_rpl}} and calculates the variance-covariance matrix
#' of the specified model and design given the priors.
#'
#' The function is called inside the \code{repeat}-loop of
#' \code{\link{generate_design}} and will be called once for each set of priors
#' (or once if the priors are not Bayesian). This is followed by a call to
#' \code{\link{calculate_efficiency_criteria}}. In the case of Bayesian priors,
#' all efficiency criteria are averaged over the prior distribution
#' before reported in the optimization.
#'
#' In the case of multi-core processing, this function is passed along the
#' parallel, to take advantage of the fact that each calculation on a set of
#' priors is independent of the calculation on other sets of priors. This does
#' not change with the model used to optimize the design.
#'
#' @param design_environment An environment containing all the elements
#' necessary to derive the variance-covariance matrix
#' @param type A string indicating the model for which you wish to derive the
#' variance covariance matrix. Can be either "mnl" or "rpl"
#'
#' @return The variance covariance matrix. If the Fisher information matrix is
#' singular, then return NULL
derive_vcov <- function(design_environment, type) {
  switch(
    type,
    mnl = eval(body(derive_vcov_mnl), envir = design_environment),
    rpl = derive_vcov_rpl()
  )
}

#' Derive the variance covariance matrix for the MNL model
derive_vcov_mnl <- function() {
  # Calculate the probability of j
  utility <- lapply(utility_string, function(v) eval(parse(text = v)))
  exp_utility <- lapply(utility, exp)
  sum_utility <- Reduce("+", exp_utility)
  pr_j <- lapply(exp_utility, function(v) {
    v <- v / sum_utility
    v[is.na(v)] <- 0
    as.vector(v)
  })

  # Multiply pr_j with x_j
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
  solve(fisher)
}

#' Derive the variance covariance matrix for the RPL model
derive_vcov_rpl <- function() {
  stop(
    "Designs for the RPL model has not been implemented yet."
  )
}
