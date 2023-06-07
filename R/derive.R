#' Derive the variance covariance matrix of the design
#'
#' The function is a wrapper around \code{\link{derive_vcov_mnl}} and
#' \code{\link{derive_vcov_rpl}} and calculates the variance-covariance matrix
#' of the specified model and design given the priors.
#'
#' @param design_env An environment containing all the elements
#' necessary to derive the variance-covariance matrix
#' @param model A string indicating the model for which you wish to derive the
#' variance covariance matrix. Can be either "mnl" or "rpl"
#'
#' @return The variance covariance matrix. If the Fisher information matrix is
#' singular, then return NULL
derive_vcov <- function(design_env, model) {

  # Calculate the variance-covariance matrix
  design_vcov <- tryCatch({
    switch(
      model,
      mnl = eval(body(derive_vcov_mnl), envir = design_env),
      rpl = derive_vcov_rpl()
    )
  },
  error = function(e) {
    return(NA)
  })

  return(
    design_vcov
  )
}

#' Derive the variance covariance matrix for the MNL model
#'
#' The function takes no arguments and is evaluated in context!
#'
#' @return The variance co-variance matrix
derive_vcov_mnl <- function() {
  # Bind locally within function to avoid CRAN NOTE
  utility_string <- utility_string
  x_j <- x_j

  # Calculate the probability of j
  obs_utility <- lapply(utility_string, function(v) eval(parse(text = v)))
  exp_utility <- lapply(obs_utility, exp)
  sum_utility <- Reduce("+", exp_utility)
  pr_j <- lapply(exp_utility, function(v) {
    v <- v / sum_utility
    v[is.na(v)] <- 0
    as.vector(v)
  })

  # Multiply pr_j with x_j
  pr_x <- mapply("*", pr_j, x_j, SIMPLIFY = FALSE)
  sum_pr_x <- Reduce("+", pr_x)

  # Take the square root of the probabilities
  sqrt_pr_j <- lapply(pr_j, sqrt)

  # Take the difference between the attribute levels and the sum of the
  # probabilities of the alternatives
  pr_diff <- lapply(x_j, function(x) x - sum_pr_x)
  pr_diff <- mapply("*", pr_diff, sqrt_pr_j, SIMPLIFY = FALSE)

  tmp <- as.matrix(Reduce(rbind, pr_diff))
  fisher <- crossprod(tmp)

  # Return the variance-covariance matrix
  return(
    solve(fisher)
  )
}

#' Derive the variance covariance matrix for the RPL model
#'
#' The function takes no arguments and is evaluated in context!
#'
#' @return The variance co-variance matrix
derive_vcov_rpl <- function() {
  stop(
    "Designs for the RPL model has not been implemented yet."
  )
}
