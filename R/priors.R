#' Prepare the list of priors
#'
#' @inheritParams generate_design
#' @inheritParams federov
#'
#' @return A list of priors
prepare_priors <- function(utility,
                           utility_parsed,
                           draws,
                           R) {
  bayesian_prior <- has_bayesian_prior(utility)
  priors <- get_prior_values(utility_parsed)

  if (bayesian_prior) {
    prior_dists <- extract_distribution(utility, "prior")

    # Create the matrix of Bayesian priors
    bayesian_priors <- make_draws(
      1,
      R,
      length(prior_dists),
      seed = 123,
      draws
    )
    colnames(bayesian_priors) <- names(prior_dists)

    for (i in seq_len(ncol(bayesian_priors))) {
      name <- names(prior_dists[i])
      value <- priors[[name]]

      bayesian_priors[, i] <- transform_distribution(
        value$mu,
        value$sigma,
        bayesian_priors[, i],
        prior_dists[i]
      )
    }

    # Create the matrix of non-Bayesian priors
    names_bayesian_priors <- names(priors) %in% names(prior_dists)

    non_bayesian_priors <- do.call(
      cbind,
      priors[!names_bayesian_priors]
    )

    non_bayesian_priors <- rep_rows(non_bayesian_priors, nrow(bayesian_priors))

    # Combine into the matrix of priors
    priors <- cbind(bayesian_priors, non_bayesian_priors)[, names(priors)]

    # Priors as a list to allow direct use of lapply()
    priors <- lapply(seq_len(nrow(priors)), function(i) priors[i, ])

  } else {
    # if (opts$cores > 1) {
    #   opts$cores <- 1
    #   cli_alert_info(
    #     "Using multiple cores is not implemented for designs without Bayesian
    #     priors. Number of cores is restored to 1."
    #   )
    # }

    priors <- list(do.call(c, priors))
  }

  # Return the list of priors
  return(
    priors
  )
}
