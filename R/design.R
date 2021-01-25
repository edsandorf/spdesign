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

  # Set up parallel ----

  # Evaluate designs ----
  cli_h1("Evaluating designs")

  # Without proper stopping conditions this becomes an infinite loop
  iter <- 1
  current_best <- NULL
  repeat {
    # Set up the initial printing to console (if statement for efficiency)
    if (iter == 1) {
      cat("\n")
      cat(rule(width = 76), "\n")
      cat(str_c(
        str_pad("Iteration", 10, "left", " "),
        if ("a_efficiency" %in% opts$efficiency_criteria) col_green(str_pad("A-error", 10, "left")) else str_pad("A-error", 10, "left", " "),
        if ("c_efficiency" %in% opts$efficiency_criteria) col_green(str_pad("C-error", 10, "left")) else str_pad("C-error", 10, "left", " "),
        if ("d_efficiency" %in% opts$efficiency_criteria) col_green(str_pad("D-error", 10, "left")) else str_pad("D-error", 10, "left", " "),
        if ("s_efficiency" %in% opts$efficiency_criteria) col_green(str_pad("S-error", 10, "left")) else str_pad("S-error", 10, "left", " "),

        str_pad("Time stamp\n", 25, "left", " ")
      ))
      cat(rule(width = 76), "\n")
    }

    # Create a design candidate
    design_candidate <- make_design_candidate(candidate_set, opts, length(V), type = opts$optimization_algorithm)

    # Create the design environment
    design_environment <- new.env()
    list2env(
      c(
        list(V_string = parsed_v[["V"]]),
        as.list(parsed_v[["param"]]), # Will be updated when we consider priors
        as.list(design_candidate)
      ),
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
      next
    }

    # Calculate the efficiency criteria for which the model is being optimized.
    p <- do.call(c, parsed_v[["param"]]) # Will fail with Bayesian priors!

    error_measures_string <- c("a_efficiency", "c_efficiency", "d_efficiency", "s_efficiency")
    error_measures <- lapply(error_measures_string, function(x) {
      calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = x)
    })
    names(error_measures) <- error_measures_string
    current_error <- error_measures[[opts$efficiency_criteria]]

    # One test for top 10 with unseen updates


    # Print update to console if we have a new best design and set new current best
    if (current_error < current_best || is.null(current_best)) {
        error_string <- lapply(seq_along(error_measures), function(i) {
          print_efficiency_criteria(error_measures[[i]], error_measures_string[i], 4, opts)
        })

      cat(str_c(
        str_pad(as.character(iter), 10, "left", " "),
        do.call(str_c, error_string),
        str_pad(paste0(Sys.time(), "\n"), 25, "left", " ")
      ))

      current_best <- current_error
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

}
