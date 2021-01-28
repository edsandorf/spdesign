#' Generate the experimental design
#'
#' Generates the experimental design is the main function of the package. It
#' wraps around all other functions and calls them in the correct order to
#' generate a design based on the specified utility functions, options and
#' candidate set.
#'
#' @param V A list of utility functions
#' @param opts A list of design optsions
#' @param candidate_set A matrix or data frame in the "wide" format containing
#' all permitted combinations of attributes. The default is NULL. If no candidate
#' set is provided, then the full factorial subject to specified restrictions
#' will be used.
#'
#' @return A design list of valid designs
#'
#' @export
generate_design <- function(V, opts, candidate_set = NULL) {
  cli_h1("Setting up the design environment")

  # Initial checks ----
  cli_h2("Running initial checks")

  # Checks on V
  ansi_with_hidden_cursor(check_v(V))

  # Checks on opts
  ansi_with_hidden_cursor(check_opts(opts))

  # Set default options
  opts <- set_defaults(opts)

  # Checks on the candidate set
  ansi_with_hidden_cursor(check_candidate_set(candidate_set))

  # Parse utility ----
  cli_h2("Parsing the utility expression")
  parsed_v <- parse_utility(V)
  cli_alert_success("The supplied utility expression 'V' has been parsed")

  # Apply restrictions to the candidate set (create one if not supplied) ----
  cli_h2("Applying restrictions to the candidate set")
  if (is.null(candidate_set)) {
    candidate_set <- generate_full_factorial(parsed_v$attrs)
    cli_alert_success("Full factorial created")
  }

  cli_alert_success("All restrictions successfully applied")

  # Create draws used for Bayesian priors ----
  prior_distributions <- extract_distribution(V, "prior")

  # Wrapped in any() to avoid warning when length prior > 1. Possible issue?
  if (any(is.na(prior_distributions))) {
    priors <- do.call(cbind, parsed_v[["param"]])
  } else {
    # Create the matrix of Bayesian priors
    bayesian_priors <- make_draws(1, opts$draws_priors, length(prior_distributions), seed = 123, opts$draws_type)
    colnames(bayesian_priors) <- names(prior_distributions)
    for (i in seq_len(ncol(bayesian_priors))) {
      name <- names(prior_distributions[i])
      value <- parsed_v[["param"]][[name]]
      bayesian_priors[, i] <- transform_distribution(value$mu, value$sigma, bayesian_priors[, i], prior_distributions[i])
    }

    # Create the matrix of non-Bayesian priors
    non_bayesian_priors <- do.call(cbind, parsed_v[["param"]][!(names(parsed_v[["param"]]) %in% names(prior_distributions))])
    non_bayesian_priors <- rep_rows(non_bayesian_priors, nrow(bayesian_priors))

    # Combine into the matrix of priors
    priors <- cbind(bayesian_priors, non_bayesian_priors)
  }

  # Priors as a list to allow direct use of lapply()
  priors <- lapply(seq_len(nrow(priors)), function(i) priors[i, ])

  # Set up the design environment ----
  design_environment <- new.env()
  list2env(
    list(V_string = parsed_v[["V"]]),
    envir = design_environment
  )

  # Set up parallel ----


  # Evaluate designs ----
  cli_h1("Evaluating designs")

  # Without proper stopping conditions this becomes an infinite loop
  iter <- 1
  current_best <- NULL
  error_measures_string <- c("a-error", "c-error", "d-error", "s-error")
  current_design_candidates <- vector(mode = "list", length = 10)
  time_start <- Sys.time()
  repeat {
    # Set up the initial printing to console (if statement for efficiency)
    if (iter == 1) {
      cat("\n")
      cat(rule(width = 76), "\n")
      cat(str_c(
        str_pad("Iteration", 10, "left", " "),
        if ("a-error" %in% opts$efficiency_criteria) col_green(str_pad("A-error", 10, "left")) else str_pad("A-error", 10, "left", " "),
        if ("c-error" %in% opts$efficiency_criteria) col_green(str_pad("C-error", 10, "left")) else str_pad("C-error", 10, "left", " "),
        if ("d-error" %in% opts$efficiency_criteria) col_green(str_pad("D-error", 10, "left")) else str_pad("D-error", 10, "left", " "),
        if ("s-error" %in% opts$efficiency_criteria) col_green(str_pad("S-error", 10, "left")) else str_pad("S-error", 10, "left", " "),

        str_pad("Time stamp\n", 25, "left", " ")
      ))
      cat(rule(width = 76), "\n")
    }

    # Create a design candidate
    design_candidate <- make_design_candidate(candidate_set, opts, V, type = opts$optimization_algorithm)

    # Update the design environment
    list2env(
      c(as.list(as.data.frame(do.call(cbind, design_candidate))), # To enable calling variables by name
        list(X = design_candidate) # Accesses X by name
      ),
      envir = design_environment
    )

    # Loop over priors
    error_measures <- lapply(priors, calculate_error_measures, design_environment, error_measures_string, opts)
    error_measures <- matrixStats::colMeans2(do.call(rbind, error_measures), na.rm = TRUE)
    names(error_measures) <- error_measures_string
    current_error <- error_measures[[opts$efficiency_criteria]]

    if (is.na(current_error)) {
      next
    }

   # Add the efficiency measures to the design candidate
    attr(design_candidate, "error_measures") <- error_measures

    # Print update to console if we have a new best design and set new current best
    if (current_error < current_best || is.null(current_best)) {
        printable_error_measures_string <- lapply(seq_along(error_measures), function(i) {
          print_efficiency_criteria(error_measures[[i]], error_measures_string[i], 4, opts)
        })

      cat(str_c(
        str_pad(as.character(iter), 10, "left", " "),
        do.call(str_c, printable_error_measures_string),
        str_pad(paste0(Sys.time(), "\n"), 25, "left", " ")
      ))

      current_best <- current_error

      assign("best_design_candidate", design_candidate, envir = .GlobalEnv)
    }

    # Stopping conditions ----
    if (iter >= opts$max_iter) {
      cat(rule(width = 76), "\n")
      break
    }
    # if (eff < opts$eff_threshold) break

    # Update the iteration counter
    iter <- iter + 1

  }

  cat("\n")
  cli_h1("Cleaning up design environment")
  cat("Time spent searching for designs: ", Sys.time() - time_start, "\n")

}
