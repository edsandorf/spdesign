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
  return(
    object[["vcov"]]
  )
}

#' Create a summary of the experimental design
#'
#' @inheritParams vcov.spdesign
#'
#' @method summary spdesign
#'
#' @export
summary.spdesign <- function(object, ...) {
  print(object, ...)
  cat("\n")
  print(object[["design"]])
  cat("\n")
  cat("---------------------------------------------------------------------\n")

  if ("blocks_correlation" %in% names(object)) {
    cat("Correlation between the blocking vector and attributes: \n \n")
    print(round(t(object$blocks_correlation), 3))
    cat("\n")
    cat("---------------------------------------------------------------------\n")
  }
}

#' Correlation
#'
#' Calculate the correlation of the design. This is a wrapper around
#' \code{\link{cor}}.
#'
#' @inheritParams print.spdesign
#'
#' @export
cor <- function(x, ...) {
  return(
    stats::cor(x[["design"]], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
  )
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
#' @inheritParams vcov.spdesign
#'
#' @return A vector of named priors used in the optimization
#'
#' @export
coef.spdesign <- function(object, ...) {
  return(
    object[["priors"]]
  )
}





