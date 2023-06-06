#' Prepare the list of priors
#'
#' @inheritParams generate_design
#' @inheritParams federov
#'
#' @return A list of priors
prepare_priors <- function(utility,
                           draws,
                           R) {

  bayesian_prior <- has_bayesian_prior(utility)
  prior_values <- priors(utility)

  if (bayesian_prior) {
    # This is somewhat more cumbersome, but it can handle the dummy coding
    unparsed <- extract_unparsed_values(utility)
    prior_dists <- str_extract(unparsed, "(normal|uniform|lognormal|triangular)")
    names(prior_dists) <- names(unparsed)
    prior_dists <- prior_dists[!is.na(prior_dists)]

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
      value <- prior_values[[name]]

      bayesian_priors[, i] <- transform_distribution(
        value$mu,
        value$sigma,
        bayesian_priors[, i],
        prior_dists[i]
      )
    }

    # Create the matrix of non-Bayesian priors
    names_bayesian_priors <- names(prior_values) %in% names(prior_dists)

    non_bayesian_priors <- do.call(
      cbind,
      prior_values[!names_bayesian_priors]
    )

    non_bayesian_priors <- rep_rows(non_bayesian_priors, nrow(bayesian_priors))

    # Combine into the matrix of priors
    prior_values <- cbind(bayesian_priors, non_bayesian_priors)[, names(prior_values)]

    # Priors as a list to allow direct use of lapply()
    prior_values <- lapply(seq_len(nrow(prior_values)), function(i) prior_values[i, ])

  } else {
    # if (opts$cores > 1) {
    #   opts$cores <- 1
    #   cli_alert_info(
    #     "Using multiple cores is not implemented for designs without Bayesian
    #     priors. Number of cores is restored to 1."
    #   )
    # }

    prior_values <- list(do.call(c, prior_values))
  }

  # Return the list of priors
  return(
    prior_values
  )
}
