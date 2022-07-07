#' Evaluate the design candidate
#'
#' The evaluation of the design candidate is independent of the optimization
#' algorithm used.
#'
#' NB! I need to extend this to include the option to choose whether to return
#' the median error.
#'
#' @param utility_parsed A parsed utility expression
#' @param design_candidate The current design candidate
#' @inheritParams generate_design
#' @inheritParams calculate_efficiency
#'
#' @return A named vector with efficiency criteria of the current design
#' candidate. If Bayesian priors are used, then it returns the average
#' error.
#'
evaluate_design_candidate <- function(utility_parsed,
                                      design_candidate,
                                      priors,
                                      design_environment,
                                      model,
                                      didx,
                                      return_all,
                                      significance) {

  # Define x_j for the analytical derivatives
  x_j <- define_x_j(utility_parsed, design_candidate)

  # Update the design environment NB! Using design_candidate because we are
  # evaluating the expression in context and don't need the interaction cols
  list2env(
    c(as.list(design_candidate),
      list(x_j = x_j)),
    envir = design_environment
  )

  # Over priors to consider bayesian!!!
  efficiency_measures <- lapply(priors, calculate_efficiency,
                                design_environment,
                                model,
                                didx,
                                return_all,
                                significance)

  # Get the average efficiency criteria (can be extended to allow for medians)
  efficiency_measures <- matrixStats::colMeans2(
    do.call(rbind, efficiency_measures), na.rm = TRUE
  )

  names(efficiency_measures) <- c("a-error", "c-error", "d-error", "s-error")

  return(
    efficiency_measures
  )
}
