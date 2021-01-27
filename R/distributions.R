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
#' or it can be another call to \code{\link{N}}, \code{\link{LN}}, \code{\link{U}}
#' or \code{\link{TR}} if the model is an RPL with a Bayesian prior.
#' @param sigma A parameter indicating the SD or spread of the distribution
#' or it can be another call to \code{\link{N}}, \code{\link{LN}}, \code{\link{U}}
#' or \code{\link{TR}}.
#'
#' @return A list of parameters
#'
#' @describeIn N The normal distribution
#'
#' @examples
#' \dontrun{
#' eval(parse(text = "N(0, 1)"))
#' eval(parse(text = "LN(0, 1)"))
#' eval(parse(text = "TR(0, 1)"))
#' eval(parse(text = "U(0, 1)"))
#' }
N <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A normally distributed prior
#' @describeIn N The normal distribution when applied to a prior
Np <- N

#' @describeIn N The log normal distribution
LN <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A log-normally distributed prior
#' @describeIn N The log-normal distribution when applied to a prior
LNp <- LN

#' @describeIn N The triangular distribution
TR <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A triangular distributed prior
#' @describeIn N The triangular distribution when applied to a prior
TRp <- TR

#' @describeIn N The uniform distribution
U <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' A uniform distributed prior
#' @describeIn N The uniform distribution when applied to a prior
Up <- U



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
    N = transform_n(mu, sigma, eta),
    LN = transform_ln(mu, sigma, eta),
    U = transform_u(mu, sigma, eta),
    TR = transform_tr(mu, sigma, eta),
  )
}

#' Transform to the normal distribution
#'
#' @inheritParams transform_distribution
transform_n <- function(mu, sigma, eta) {
  mu + sigma * qnorm(eta)
}

#' Transform to the lognormal distribution
#'
#' @inheritParams transform_distribution
transform_ln <- function(mu, sigma, eta) {
  exp(mu + sigma * qnorm(eta))
}

#' Transform to the uniform distribution
#'
#' @inheritParams transform_distribution
transform_u <- function(mu, sigma, eta) {
  mu + sigma * (2 * eta - 1)
}

#' Transform to the triangular distribution
#'
#' @inheritParams transform_distribution
transform_tr <- function(mu, sigma, eta) {
  idx <- +(eta < 0.5)
  eta <- idx * (sqrt(2 * eta) - 1) + (1 - idx) * (1 - sqrt(2 * (1 - eta)))
  mu + sigma * eta
}
