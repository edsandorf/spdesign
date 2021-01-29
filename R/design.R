#' Generate the experimental design
#'
#' Generates the experimental design is the main function of the package. It
#' wraps around all other functions and calls them in the correct order to
#' generate a design based on the specified utility functions, options and
#' candidate set.
#'
#' @param utility A list of utility functions
#' @param opts A list of design optsions
#' @param candidate_set A matrix or data frame in the "wide" format containing
#' all permitted combinations of attributes. The default is NULL. If no
#' candidate set is provided, then the full factorial subject to specified
#' restrictions will be used.
#'
#' @return A design list of valid designs
#'
#' @export
generate_design <- function(utility, opts, candidate_set = NULL) {
  cli_h1(
    "Setting up the design environment"
  )

  # Initial checks ----
  cli_h2(
    "Running initial checks"
  )

  # Checks on utility
  ansi_with_hidden_cursor(check_v(utility))

  # Checks on opts
  ansi_with_hidden_cursor(check_opts(opts))

  # Set default options
  opts <- set_defaults(opts)

  # Checks on the candidate set
  ansi_with_hidden_cursor(check_candidate_set(candidate_set))

  # Parse utility ----
  cli_h2(
    "Parsing the utility expression"
  )
  parsed_v <- parse_utility(utility)
  cli_alert_success(
    "The supplied utility expression 'utility' has been parsed"
  )

  # Apply restrictions to the candidate set (create one if not supplied) ----
  cli_h2(
    "Applying restrictions to the candidate set"
  )
  if (is.null(candidate_set)) {
    candidate_set <- generate_full_factorial(parsed_v$attrs)
    cli_alert_success(
      "Full factorial created"
    )
  }

  cli_alert_success(
    "All restrictions successfully applied"
  )

  # Create draws used for Bayesian priors ----
  bayesian_prior <- has_bayesian_prior(utility)
  if (bayesian_prior) {
    prior_dists <- extract_distribution(utility, "prior")

    # Create the matrix of Bayesian priors
    bayesian_priors <- make_draws(
      1,
      opts$draws_priors,
      length(prior_dists),
      seed = 123,
      opts$draws_type
    )
    colnames(bayesian_priors) <- names(prior_dists)
    for (i in seq_len(ncol(bayesian_priors))) {
      name <- names(prior_dists[i])
      value <- parsed_v[["param"]][[name]]
      bayesian_priors[, i] <- transform_distribution(
        value$mu, value$sigma,
        bayesian_priors[, i],
        prior_dists[i]
      )
    }

    # Create the matrix of non-Bayesian priors
    names_bayesian_priors <- names(parsed_v[["param"]]) %in% names(prior_dists)
    non_bayesian_priors <- do.call(
      cbind,
      parsed_v[["param"]][!names_bayesian_priors])
    non_bayesian_priors <- rep_rows(non_bayesian_priors, nrow(bayesian_priors))

    # Combine into the matrix of priors
    priors <- cbind(bayesian_priors, non_bayesian_priors)

    # Priors as a list to allow direct use of lapply()
    priors <- lapply(seq_len(nrow(priors)), function(i) priors[i, ])

  } else {
    if (opts$cores > 1) {
      opts$cores <- 1
      cli_alert_info(
        "Using multiple cores is not implemented for designs without Bayesian
        priors. Number of cores is restored to 1."
      )
    }

    priors <- do.call(c, parsed_v[["param"]])
  }

  # Set up the design environment ----
  design_environment <- new.env()
  list2env(
    list(utility_string = parsed_v[["utility"]]),
    envir = design_environment
  )

  # Set up parallel ----
  if (opts$cores > 1) {
    future::plan(
      future::multicore(workers = opts$cores)
    )
  }

  # Evaluate designs ----
  cli_h1(
    "Evaluating designs"
  )

  # Without proper stopping conditions this becomes an infinite loop
  iter <- 1
  current_best <- NULL
  error_measures_string <- c("a-error", "c-error", "d-error", "s-error")
  time_start <- Sys.time()
  repeat {
    # Create a design candidate
    design_candidate <- make_design_candidate(
      candidate_set,
      opts,
      utility,
      type = opts$optimization_algorithm
    )

    # Update the design environment
    list2env(
      c(
        as.list(as.data.frame(do.call(cbind, design_candidate))),
        list(X = design_candidate)
      ),
      envir = design_environment
    )

    # Calculate the error measures
    if (bayesian_prior) {
      if (opts$cores > 1) {
        # The overhead of sending info to the workers is too high
        # slows everything down (maybe works for larger designs)
        error_measures <- future.apply::future_lapply(
          priors,
          calculate_error_measures,
          design_environment,
          error_measures_string,
          opts
        )
      } else {
        error_measures <- lapply(
          priors,
          calculate_error_measures,
          design_environment,
          error_measures_string,
          opts
        )
      }
      error_measures <- matrixStats::colMeans2(
        do.call(
          rbind,
          error_measures
        ),
        na.rm = TRUE
      )
    } else {
      error_measures <- calculate_error_measures(
        priors,
        design_environment,
        error_measures_string,
        opts
      )
    }

    names(error_measures) <- error_measures_string
    current_error <- error_measures[[opts$efficiency_criteria]]

    if (is.na(current_error)) {
      next
    }

    # Add the efficiency measures to the design candidate
    attr(design_candidate, "error_measures") <- error_measures

    # Print update to set new current best (incl. first iteration)
    if (current_error < current_best || is.null(current_best)) {
      print_iteration_information(
        iter,
        values = error_measures,
        criteria = error_measures_string,
        digits = 4,
        padding = 10,
        width = 80,
        opts
      )

      current_best <- current_error

      # Need to find a different solution to assigning to global
      assign("best_design_candidate", design_candidate, envir = .GlobalEnv)
    }

    # Stopping conditions ----
    if (iter >= opts$max_iter) {
      cat(rule(width = 76), "\n")
      cli_alert_info(
        "Maximum number of iterations reached."
      )
      break
    }

    if (current_error < opts$eff_threshold) {
      cat(rule(width = 76), "\n")
      cli_alert_info(
        "Efficiency criteria is less than threshhold."
      )
      break
    }

    # Update the iteration counter
    iter <- iter + 1

  }

  cat("\n")
  cli_h1(
    "Cleaning up design environment"
  )
  cat(
    "Time spent searching for designs: ",
    Sys.time() - time_start,
    "\n"
  )
}
