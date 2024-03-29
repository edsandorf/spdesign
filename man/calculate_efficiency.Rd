% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiency-criteria.R
\name{calculate_efficiency}
\alias{calculate_efficiency}
\title{Calculate efficiency}
\usage{
calculate_efficiency(
  prior_values,
  design_env,
  model,
  dudx,
  return_all = FALSE,
  significance = 1.96
)
}
\arguments{
\item{prior_values}{a list or vector of assumed priors}

\item{design_env}{A design environment in which to evaluate the
the function to derive the variance-covariance matrix.}

\item{model}{A character string indicating the model to optimize the design
for. Currently the only model programmed is the 'mnl' model and this is also
set as the default.}

\item{dudx}{A character string giving the name of the prior in the
denominator. Must be specified when optimizing for 'c-error'}

\item{return_all}{If `TRUE` return a K or K-1 vector with parameter specific error
measures. Default is `FALSE`.}

\item{significance}{A t-value corresponding to the desired level of
significance. The default is significance at the 5% level with an associated
t-value of 1.96.}
}
\value{
A list with a named vector of efficiency criteria and the
variance-covariance matrix
}
\description{
The function is called inside \code{\link{evaluate_design_candidate}}
}
