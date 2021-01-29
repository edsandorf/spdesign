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
  ansi_with_hidden_cursor(
    check_v(utility)
  )

  # Checks on opts
  ansi_with_hidden_cursor(
    check_opts(opts)
  )

  # Set default options
  opts <- set_defaults(opts)

  # Checks on the candidate set
  ansi_with_hidden_cursor(
    check_candidate_set(candidate_set)
  )

  # Parse utility ----
  cli_h2(
    "Parsing the utility expression"
  )

  parsed_utility <- parse_utility(utility)

  cli_alert_success(
    "The supplied utility expression 'utility' has been parsed"
  )

  # Apply restrictions to the candidate set (create one if not supplied) ----
  cli_h2(
    "Applying restrictions to the candidate set"
  )

  if (is.null(candidate_set)) {
    candidate_set <- generate_full_factorial(parsed_utility$attrs)
    cli_alert_success(
      "Full factorial created"
    )
  }

  cli_alert_success(
    "All restrictions successfully applied"
  )

  # Prepare the list of priors ----
  priors <- prepare_priors(utility, parsed_utility, opts)

  # Set up the design environment ----
  design_environment <- new.env()
  list2env(
    list(utility_string = parsed_utility[["utility"]]),
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
  best_design_candidate <- NULL
  on.exit(
    return(best_design_candidate),
    add = TRUE
  )

  # Search for designs until the criteria are met
  repeat {
    # Create a design candidate
    current_design_candidate <- make_design_candidate(
      candidate_set,
      opts,
      utility,
      type = opts$optimization_algorithm
    )

    # Update the design environment
    list2env(
      c(
        as.list(as.data.frame(do.call(cbind, current_design_candidate))),
        list(X = current_design_candidate)
      ),
      envir = design_environment
    )

    # Calculate the error measures
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

    names(error_measures) <- error_measures_string
    current_error <- error_measures[[opts$efficiency_criteria]]

    if (is.na(current_error)) {
      next
    }

    # Add the efficiency measures to the design candidate
    attr(current_design_candidate, "error_measures") <- error_measures

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
      best_design_candidate <- current_design_candidate
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

  # Return the best design candidate
  # best_design_candidate

}
