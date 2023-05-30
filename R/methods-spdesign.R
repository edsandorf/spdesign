#' @importFrom stats vcov
#' @export
stats::vcov

#' Extract the variance co-variance matrix
#'
#' A generic method for extracting the variance covariance matrix from a design
#' object
#'
#' @param object A model object of class 'spdesign'
#' @param ... Additional arguments passed to the function
#'
#' @method vcov spdesign
#'
#' @return A matrix with row- and column names equal to the parameter names
#'
#' @export
vcov.spdesign <- function(object, ...) {

}

#' Create a summary of the experimental design
#'
#' @inheritParams vcov.spdesign
#'
#' @method summary spdesign
#'
#' @export
summary.spdesign <- function(object, ...) {

}

#' A generic function for printing an 'spdesign' object
#'
#' @param x A model object of class 'spdesign'
#' @param ... Additional parameters passed to the function
#'
#' @method print spdesign
#'
#' @export
print.spdesign <- function(x, ...) {
  cat("---------------------------------------------------------------------\n")
  cat("An 'spdesign' object\n\n")
  cat("Utility functions:\n")
  for (i in seq_along(x$utility)) {
    cat(names(x$utility)[[i]], ":", x$utility[[i]], "\n")
  }

  cat("\n\n")
  print(x$efficiency_criteria)
  cat("\n")
  cat("---------------------------------------------------------------------\n")
}


#' Generic for extracting the vector of priors
#'
#' @inheritParams prior
#'
#' @return A vector of named priors used in the optimization
#'
#' @export
spdesign.coef <- function(object, ...) {
  return(
    prior(object, ...)
  )
}





#' Generic for extracting the attributes and levels
#'
#' @inheritParams prior.utility
#'
#' @export
# attribute.utility <- function(x) {
#   return(
#     x[["attribute_levels"]]
#   )
# }

#' Generic for extracting the number of utility functions
#'
#' @inheritParams prior.utility
#'
#' @export
# nalts.utility <- function(x) {
#   return(
#     length(x[[""]])
#   )
# }
