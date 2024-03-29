% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{random_design_candidate}
\alias{random_design_candidate}
\title{Create a random design_object candidate}
\usage{
random_design_candidate(utility, candidate_set, rows, sample_with_replacement)
}
\arguments{
\item{utility}{A named list of utility functions. See the examples and the
vignette for examples of how to define these correctly for different types
of experimental designs.}

\item{candidate_set}{A matrix or data frame in the "wide" format containing
all permitted combinations of attributes. The default is NULL. If no
candidate set is provided, then the full factorial subject to specified
exclusions will be used. This is passed in as an object and not a character
string. The candidate set will be expanded to include zero columns to
consider alternative specific attributes.}

\item{rows}{An integer giving the number of rows in the final design}

\item{sample_with_replacement}{A boolean equal to TRUE if we sample from the
candidate set with replacement. The default is FALSE}
}
\description{
Sample from the candidate set to create a random design_object.
}
