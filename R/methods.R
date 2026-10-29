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
#' @return No return value. Prints a summary of the 'spdesign' object to the
#' console
#'
#' @export
summary.spdesign <- function(object, ...) {
  print(object, ...)
  cat("\n")
  cat("Printing the first few rows of the design \n")
  print(utils::head(object[["design"]]))
  cat("\n")
  cat("---------------------------------------------------------------------\n")

  if ("blocks_correlation" %in% names(object)) {
    cat("Correlation between the blocking vector and attributes: \n \n")
    print(object$blocks_correlation)
    cat("\n")
    cat(
      "---------------------------------------------------------------------\n"
    )
  }
}

#' Correlation
#'
#' This function is deprecated and will be removed in a future version. Use
#' \code{cor(design$design)} to calculate the correlation of a design.
#'
#' For an object of class 'spdesign', the function calculates the correlation
#' of the design. For any other object, it calls \code{\link[stats]{cor}}, so
#' that it does not mask the function from stats.
#'
#' @param x An object of class 'spdesign', or any input to
#' \code{\link[stats]{cor}}
#' @param ... Additional arguments passed to \code{\link[stats]{cor}}
#'
#' @return A matrix with correlations
#'
#' @export
cor <- function(x, ...) {
  if (!inherits(x, "spdesign")) {
    return(stats::cor(x, ...))
  }

  .Deprecated(
    msg = "cor() for an 'spdesign' object is deprecated. Use cor(design$design) instead."
  )

  return(
    stats::cor(x[["design"]], ...)
  )
}

#' A generic function for printing an 'spdesign' object
#'
#' @param x A model object of class 'spdesign'
#' @param ... Additional parameters passed to the function
#'
#' @method print spdesign
#'
#' @return No return value. Prints the 'spdesign' object.
#'
#' @export
print.spdesign <- function(x, ...) {
  cat("---------------------------------------------------------------------\n")
  cat("An 'spdesign' object\n\n")
  cat("Utility functions:\n")
  for (i in seq_along(x$utility)) {
    cat(names(x$utility)[[i]], ":", clean_utility(x$utility)[[i]], "\n")
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
    object[["prior_values"]]
  )
}
