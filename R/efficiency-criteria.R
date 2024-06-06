#' Calculate efficiency
#'
#' The function is called inside \code{\link{evaluate_design_candidate}}
#'
#' @param prior_values a list or vector of assumed priors
#' @param design_env A design environment in which to evaluate the
#' the function to derive the variance-covariance matrix.
#' @param return_all If `TRUE` return a K or K-1 vector with parameter specific error
#' measures. Default is `FALSE`.
#' @param significance A t-value corresponding to the desired level of
#' significance. The default is significance at the 5% level with an associated
#' t-value of 1.96.
#' @inheritParams generate_design
#'
#' @return A list with a named vector of efficiency criteria and the
#' variance-covariance matrix
calculate_efficiency <- function(prior_values,
                                 design_env,
                                 model,
                                 dudx,
                                 return_all = FALSE,
                                 significance = 1.96) {
  # Define the string of possible efficiency criteria
  efficiency_criteria_string <- c("a-error", "c-error", "d-error", "s-error")

  # Add the priors to the design environment
  list2env(
    as.list(prior_values),
    envir = design_env
  )

  # Derive the variance-covariance matrix
  design_vcov <- derive_vcov(design_env, model = model)

  # Check if we have NA in the variance-covariance matrix. If so, return vecor
  # of NA
  if (any(is.na(design_vcov))) {
    return(
      list(
        efficiency_criteria = rep(NA, length(efficiency_criteria_string)),
        vcov = design_vcov
      )
    )
  }

  efficiency_criteria <- lapply(efficiency_criteria_string, function(x) {
    return(
      calculate_efficiency_criteria(design_vcov,
                                    prior_values,
                                    dudx,
                                    return_all,
                                    significance,
                                    type = x)
    )
  })

  return(
    list(
      efficiency_criteria = do.call(c, efficiency_criteria),
      vcov = design_vcov
    )
  )
}

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
#' @inheritParams calculate_efficiency
#' @param p Prior values
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
  p,
  dudx,
  return_all = FALSE,
  significance = 1.96,
  type
) {
  switch(
    type,
    `a-error` = calculate_a_error(design_vcov),
    `c-error` = calculate_c_error(design_vcov, p, dudx, return_all),
    `d-error` = calculate_d_error(design_vcov),
    `s-error` = calculate_s_error(design_vcov, p, return_all, significance)
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
calculate_c_error <- function(design_vcov, p, dudx, return_all) {
  # Undefined if the denominator is not specified
  if (is.null(dudx)) {
    NA

  } else {
    # Local overwrite with respect to the actual position for correct subsetting
    # dudx <- which(names(p) == dudx)
    dudx <- which(str_detect(names(p), dudx) == TRUE)

    c_eff <- p[-dudx]^-2 * (diag(design_vcov)[dudx] - 2 * p[dudx] * p[-dudx]^-1 * design_vcov[dudx, seq_len(nrow(design_vcov))[-dudx]] + (p[dudx] / p[-dudx])^2 * diag(design_vcov)[-dudx])

    # Check if all are to be returned
    if (return_all) {
      c_eff
    } else {
      sum(c_eff, na.rm = TRUE)
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
calculate_s_error <- function(design_vcov, p, return_all, significance) {
  s_eff <- ((sqrt(diag(design_vcov)) * significance) / p)^2

  # Check if all are to be returned
  if (return_all) {
    s_eff
  } else {
    max(s_eff)
  }

}

