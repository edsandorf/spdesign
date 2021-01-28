#' Calculate efficiency criteria
#'
#' The function is a wrapper around \code{\link{calculate_a_error}},
#' \code{\link{calculate_c_error}}, \code{\link{calculate_d_error}} and
#' \code{\link{calculate_s_error}} to provide a unified interface for
#' calling and calculating efficiency criteria.
#'
#' The function is mainly used internally to evaluate and report on designs,
#' but is exported to allow the user to use the function to calculate the
#' efficiency criteria of the model once it has been run on their data.
#'
#' @param design_vcov A variance-covariance matrix returned by
#' \code{\link{derive_vcov}} or returned by an estimation routine. The matrix
#' should be symmetrical and K-by-K
#' @param p A vector of parameters, e.g. the named vector of priors. This is
#' used for c- and s-errors. Default value is NULL
#' @param didx An integer indicating the position of the denominator in `p` This
#' is only used for c-efficiency. Default value is NULL
#' @param all If `TRUE` return a K or K-1 vector with parameter specific error
#' measures. Default is `FALSE`.
#' @param significance A t-value corresponding to the desired level of
#' significance. The default is significance at the 5% level with an associated
#' t-value of 1.96.
#' @param type A string indicating the type of efficiency criteria to calculate
#' can be either: "a-error", "c-error", "d-error" or "s-error"
#'
#' @return See individual efficiency criteria
#'
#' @references
#' Bliemer and Rose, 2009, Efficiency and sample size requirements for state
#' choice experiments, Transportation Research Board Annual Meeting, Washington
#' DC
#' Scarpa and Rose, 2008, Designs efficiency for non-market valuation with
#' choice modelling: How to measure it, what to report and why, Australian
#' Journal of Agricultural and Resource Economics, 52(3):253-282
#' Bliemer and Rose, 2005a, Efficiency and sample size requirements for stated
#' choice experiments, Report ITLS-WP-05-08, Institute for Transport and
#' Logistics Studies, University of Sydney
#' Kessels, R., Goos, P. and Vandebroek, M., 2006, A comparison of criteria to
#' design efficient choice experiments, Journal of Marketing Research,
#' 43(3):409-419
#'
#' @export
calculate_efficiency_criteria <- function(
  design_vcov,
  p = NULL,
  didx = NULL,
  all = FALSE,
  significance = 1.96,
  type
) {
  switch(
    type,
    `a-error` = calculate_a_error(design_vcov),
    `c-error` = calculate_c_error(design_vcov, p, didx, all),
    `d-error` = calculate_d_error(design_vcov),
    `s-error` = calculate_s_error(design_vcov, p, all, significance)
  )
}

#' A-error
#'
#' Computes the A-error of the design, which is equal to the trace of the
#' variance-covariance matrix over the number of parameters to be estimated
#'
#' @inheritParams calculate_efficiency_criteria
#'
#' @return A single error measure
calculate_a_error <- function(design_vcov) {
  sum(diag(design_vcov)) / nrow(design_vcov)
}

#' C-error
#'
#' Seeks to minimize the variance of the ratio of two parameters, for example,
#' willingness-to-pay.
#'
#' @inheritParams calculate_efficiency_criteria
#'
#' @return A vector giving the variance of the ratio for each K-1 parameter or a
#' single number with the sum of the variances used for optimization
#'
calculate_c_error <- function(design_vcov, p, didx, all) {
  # Undefined if the denominator is not specified
  if (is.null(didx)) {
    NA
  } else {
    c_eff <- p[-didx]^-2 *
      (diag(design_vcov)[didx] - 2 *
         p[didx] * p[-didx]^-1 *
         design_vcov[didx, seq_len(nrow(design_vcov))[-didx]] +
         (p[didx] / p[-didx])^2 * diag(design_vcov)[-didx])

    # Check if all are to be returned
    if (all) {
      c_eff
    } else {
      sum(c_eff)
    }
  }
}

#' D-error
#'
#' Computes the D-error of the design, which is equal to the K-root of the
#' determinant of the variance-covariance matrix.
#'
#' @inheritParams calculate_efficiency_criteria
#'
#' @return A single number
calculate_d_error <- function(design_vcov) {
  det(design_vcov) ^ (1 / nrow(design_vcov))
}

#' S-error
#'
#' Calculates a "lower bound" sample size to obtain theoretically significant
#' parameter estimates under the assumption that the priors are correct.
#'
#' @inheritParams calculate_efficiency_criteria
#'
#' @return A vector giving the "minimum" sample size for each parameter or a
#' single number with the smallest sample size needed for all parameters to be
#' theoretically significant.
calculate_s_error <- function(design_vcov, p, all, significance) {
  s_eff <- ((sqrt(diag(design_vcov)) * significance) / p)^2

  # Check if all are to be returned
  if (all) {
    s_eff
  } else {
    max(s_eff)
  }

}

#' Creates a printable version of the efficiency criteria
#'
#' The function is only meant for internal use to handle pretty printing to
#' console.
#'
#' @param value The value of the efficiency criteria obtained by
#' \code{\link{calculate_efficiency_criteria}}
#' @param criteria A character string with the name of the efficiency criteria.
#' See manual for valid values
#' @param digits The nubmer of digits to round the printed value to. The default
#' is 4.
#' @param opts The list of design options. The default is NULL, but must be
#' specified to print the criteria in colour.
#'
#' @return A character string.
print_efficiency_criteria <- function(
  value,
  criteria,
  digits = 4,
  opts = NULL
) {
  if (is.na(value)) {
    string <- "N/A"
  } else {
    string <- as.character(round(value, digits))
  }

  if (!is.null(opts) && criteria %in% opts$efficiency_criteria) {
    col_green(str_pad(string, 10, "left", " "))
  } else {
    str_pad(string, 10, "left", " ")
  }
}

#' Calculates the error measures
#'
#' The function is called from inside the lapply() loop over priors in the
#' \code{\link{generate_design}} function.
#'
#' @param p A vector of parameters
#' @param design_environment A design environment
#' @param error_measures_string A string of efficiency criteria to be
#' calculated. The inputs matches \code{\link{calculate_efficiency_criteria}}
#' type.
#' @param opts List of options
#'
#' @return A named vector of error measures
calculate_error_measures <- function(
  p,
  design_environment,
  error_measures_string,
  opts
) {
  # Add the priors to the design environment
  list2env(
    as.list(p),
    envir = design_environment
  )

  # Calculate the variance-covariance matrix
  design_vcov <- tryCatch({
    derive_vcov(design_environment, type = opts$model)
  },
  error = function(e) {
    NA
  })

  if (any(is.na(design_vcov))) {
    return(rep(NA, length(error_measures_string)))
  }

  # Calculate the error measures
  error_measures <- lapply(error_measures_string, function(x) {
    calculate_efficiency_criteria(
      design_vcov,
      p,
      opts$didx,
      all = FALSE,
      type = x
    )
  })
  do.call(c, error_measures)
}
