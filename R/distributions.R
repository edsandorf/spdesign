#' Evaluating a distribution
#'
#' The function returns its arguments as a named list. The function is used
#' inside the utility functions. It is transformed to an expression using
#' \code{\link{parse}} and evaluated using \code{\link{eval}}. This ensures
#' that in the case of an RPL with Bayesian priors, recursion is handled
#' automatically. This significantly simplifies translating the utility function
#' to lists of parameters to use when optimizing the designs. It is also less
#' error prone.
#'
#' @param mu A parameter indicating the mean or location of the distribution
#' depending on whether it is a normal, log-normal, triangular or uniform,
#' or it can be another call to \code{\link{normal}}, \code{\link{lognormal}},
#' \code{\link{uniform}} or \code{\link{triangular}} if the model is an RPL with
#' a Bayesian prior.
#' @param sigma A parameter indicating the SD or spread of the distribution
#' or it can be another call to \code{\link{normal}}, \code{\link{lognormal}},
#'  \code{\link{uniform}} or \code{\link{triangular}}.
#'
#' @return A list of parameters
#'
#' @describeIn normal The normal distribution
#'
normal <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A normally distributed prior
#' @describeIn normal The normal distribution when applied to a prior
normal_p <- normal

#' @describeIn normal The log normal distribution
lognormal <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A log-normally distributed prior
#' @describeIn normal The log-normal distribution when applied to a prior
lognormal_p <- lognormal

#' @describeIn normal The triangular distribution
triangular <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A triangular distributed prior
#' @describeIn normal The triangular distribution when applied to a prior
triangular_p <- triangular

#' @describeIn normal The uniform distribution
uniform <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A uniform distributed prior
#' @describeIn normal The uniform distribution when applied to a prior
uniform_p <- uniform

#' Transform distribution
#'
#' @param mu A value for the mean of the distribution
#' @param sigma A value for the standard deviation of the distribution
#' @param eta A numeric standard uniform vector
#' @param type The type of distribution
#'
#' @return A vector with the transformed distribution given the parameters
transform_distribution <- function(mu, sigma, eta, type) {
  switch(
    type,
    normal = transform_normal(mu, sigma, eta),
    lognormal = transform_lognormal(mu, sigma, eta),
    uniform = transform_uniform(mu, sigma, eta),
    triangular = transform_triangular(mu, sigma, eta),
  )
}

#' Transform to the normal distribution
#'
#' @inheritParams transform_distribution
transform_normal <- function(mu, sigma, eta) {
  mu + sigma * qnorm(eta)
}

#' Transform to the lognormal distribution
#'
#' @inheritParams transform_distribution
transform_lognormal <- function(mu, sigma, eta) {
  exp(mu + sigma * qnorm(eta))
}

#' Transform to the uniform distribution
#'
#' @inheritParams transform_distribution
transform_uniform <- function(mu, sigma, eta) {
  mu + sigma * (2 * eta - 1)
}

#' Transform to the triangular distribution
#'
#' @inheritParams transform_distribution
transform_triangular <- function(mu, sigma, eta) {
  idx <- as.integer(eta < 0.5)
  eta <- idx * (sqrt(2 * eta) - 1) + (1 - idx) * (1 - sqrt(2 * (1 - eta)))
  mu + sigma * eta
}
