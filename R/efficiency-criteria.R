#' A-efficiency
#'
#' Computes the A-efficiency of the design, which is equal to the trace of the
#' variance-covariance matrix over the number of parameters to be estimated
#'
#' @param x A variance-covariance matrix
#'
#' @return A single efficiency measure
#'
#' @references
#' Bliemer and Rose, 2009, Efficiency and sample size requirements for state choice experiments, Transportation Research Board Annual Meeting, Washington DC
#' Scarpa and Rose, 2008, Designs efficiency for non-market valuation with choice modelling: How to measure it, what to report and why, Australian Journal of Agricultural and Resource Economics, 52(3):253-282
#'
#' @export
a_efficiency <- function(x) {
  sum(diag(x)) / nrow(x)
}

#' C-efficiency
#'
#' Seeks to minimize the variance of the ratio of two parameters, for example,
#' willingness-to-pay.
#'
#' @param p A vector of parameters
#' @inheritParams a_efficiency
#' @param didx An integer indicating the position of the denominator in `p`
#' @param all If `all = TRUE` return a vector with variance for each ratio,
#' else return a single number with sum of the K-1 ratios. The
#' default is FALSE. If used for optimization, then a single number is used, but
#' the variance for each parameter is always printed regardless of
#' efficiency criteria used in optimization as long as the denominator is defined.
#'
#' @return A vector giving the variance of the ratio for each K-1 parameter or a
#' single number with the sum of the variances used for optimization
#'
#' @references
#' Kessels, R., Goos, P. and Vandebroek, M., 2006, A comparison of criteria to design efficient choice experiments, Journal of Marketing Research, 43(3):409-419
#' Scarpa and Rose, 2008, Designs efficiency for non-market valuation with choice modelling: How to measure it, what to report and why, Australian Journal of Agricultural and Resource Economics, 52(3):253-282
#'
#' @export
c_efficiency <- function(p, x, didx, all = FALSE) {
  c_eff <- p[-didx]^-2 * (diag(x)[didx] - 2 * p[didx] * p[-didx]^-1 * x[didx, seq_len(nrow(x))[-didx]] + (p[didx] / p[-didx])^2 * diag(x)[-didx])

  # Check if all are to be returned
  if (all) {
    c_eff
  } else {
    sum(c_eff)
  }
}

#' D-efficiency
#'
#' Computes the D-efficiency of the design, which is equal to the K-root of the
#' determinant of the variance-covariance matrix. In the case of Bayesian priors,
#' the formula is averaged over the draws used to simulate the "prior distribution".
#'
#' @inheritParams a_efficiency
#'
#' @return A single number
#'
#' @references
#' Kessels, R., Goos, P. and Vandebroek, M., 2006, A comparison of criteria to design efficient choice experiments, Journal of Marketing Research, 43(3):409-419
#' Scarpa and Rose, 2008, Designs efficiency for non-market valuation with choice modelling: How to measure it, what to report and why, Australian Journal of Agricultural and Resource Economics, 52(3):253-282
#'
#' @export
d_efficiency <- function(x) {
  det(x)^(1/nrow(x))
}

#' S-efficiency
#'
#' Calculates a "lower bound" sample size to obtain theoretically significant
#' parameter estimates under the assumption that the priors are correct. In the
#' case of zero priors the formula is undefined and will return Inf.
#'
#' @param p A vector of parameters
#' @inheritParams a_efficiency
#' @param significance A t-value corresponding to the desired level of significance.
#' The default is significance at the 5% level with an associated t-value of
#' 1.96.
#' @param all If `all = TRUE` return a vector with sample size for each parameter,
#' else return a single number with the minimum theoretical sample size. The
#' default is FALSE. If used for optimization, then a single number is used, but
#' the suggested sample size for each parameter is always printed regardless of
#' efficiency criteria used in optimization.
#'
#' @return A vector giving the "minimum" sample size for each parameter or a
#' single number with the smallest sample size needed for all parameters to be
#' theoretically significant. This is equal to the maximum of the vector
#'
#' @references
#' Kessels, R., Goos, P. and Vandebroek, M., 2006, A comparison of criteria to design efficient choice experiments, Journal of Marketing Research, 43(3):409-419
#' Bliemer and Rose, 2005a, Efficiency and sample size requirements for stated choice experiments, Report ITLS-WP-05-08, Institute for Transport and Logistics Studies, University of Sydney
#' Bliemer and Rose, 2009, Efficiency and sample size requirements for stated choice experiments, Transportation Research Board Annual Meeting, Washington DC
#'
#' @export
s_efficiency <- function(p, x, significance = 1.96, all = FALSE) {
  s_eff <- ((sqrt(diag(x)) * significance) / p)^2

  # Check if all are to be returned
  if (all) {
    s_eff
  } else {
    max(s_eff)
  }

}
