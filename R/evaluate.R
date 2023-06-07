#' Evaluate the design candidate
#'
#' The evaluation of the design candidate is independent of the optimization
#' algorithm used.
#'
#' @param utility A utility function
#' @param design_candidate The current design candidate
#' @inheritParams generate_design
#' @inheritParams calculate_efficiency
#'
#' @return A named vector with efficiency criteria of the current design
#' candidate. If Bayesian prior_values are used, then it returns the average
#' error.
evaluate_design_candidate <- function(utility,
                                      design_candidate,
                                      prior_values,
                                      design_env,
                                      model,
                                      dudx,
                                      return_all,
                                      significance) {

  # Define x_j for the analytical derivatives
  x_j <- define_x_j(utility, design_candidate)

  # Update the design environment NB! Using design_candidate because we are
  # evaluating the expression in context and don't need the interaction cols
  list2env(
    c(as.list(as.data.frame(do.call(cbind, define_base_x_j(utility, design_candidate)))),
      list(x_j = x_j)),
    envir = design_env
  )

  # Over prior_values to consider bayesian!!!
  efficiency_outputs <- lapply(prior_values,
                               calculate_efficiency,
                               design_env,
                               model,
                               dudx,
                               return_all,
                               significance)

  # Get the average efficiency criteria (can be extended to allow for medians)
  efficiency_measures <- matrixStats::colMeans2(
    do.call(rbind, lapply(efficiency_outputs, function(x) return(x[[1L]]))),
    na.rm = TRUE
  )

  names(efficiency_measures) <- c("a-error", "c-error", "d-error", "s-error")

  # Averaging over the variance-covariance matrices to consider Bayesian
  dims <- c(rep(length(prior_values[[1]]), 2), length(efficiency_outputs))
  design_vcov <- array(unlist(lapply(efficiency_outputs, function(x) return(x[[2L]]))), dims)
  design_vcov <- rowMeans(design_vcov, na.rm = TRUE, dims = 2)
  dimnames(design_vcov) <- list(names(prior_values[[1]]), names(prior_values[[1]]))

  return(
    list(
      efficiency_measures = efficiency_measures,
      vcov = design_vcov
    )
  )
}
