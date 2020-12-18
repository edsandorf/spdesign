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
#'
#' @export
N <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}


#' @describeIn N The log normal distribution
LN <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' @describeIn N The triangular distribution
TR <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' @describeIn N The uniform distribution
U <- function(mu, sigma) {
  list(
    mu = mu,
    sigma = sigma
  )
}

#' Transform to the normal distribution
#'
#' @param x A numeric standard uniform vector
transform_n <- function(x) {
  qnorm(x)
}
