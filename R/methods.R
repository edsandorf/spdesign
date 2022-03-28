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
  # cat_summary_info(x, ...)
  cat("\n\n")
  # print(coef(x))
  cat("\n\n")
  cat("---------------------------------------------------------------------\n")
}
