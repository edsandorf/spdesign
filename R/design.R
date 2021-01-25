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
  iter <- 0
  repeat {
    # Update the iteration counter
    iter <- iter + 1

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
    design_vcov <- derive_vcov(design_environment, type = opts$model)
    if (is.null(design_vcov)) {
      next
    }

    # Calculate the efficiency criteria for which the model is being optimized.
    p <- do.call(c, parsed_v[["param"]]) # Will fail with Bayesian priors!
    eff <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = opts$efficiency_criteria)

    # One test for top 10 with unseen updates

    # One test for #1 with visible updates
    a_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "a_efficiency")
    a_error_string <- print_efficiency_criteria(a_error, "a_efficiency", 4, opts)

    c_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "c_efficiency")
    c_error_string <- print_efficiency_criteria(c_error, "c_efficiency", 4, opts)


    d_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "d_efficiency")
    d_error_string <- print_efficiency_criteria(d_error, "d_efficiency", 4, opts)

    s_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "s_efficiency")
    s_error_string <- print_efficiency_criteria(s_error, "s_efficiency", 4, opts)

    cat(str_c(
      str_pad(as.character(iter), 10, "left", " "),
      a_error_string,
      c_error_string,
      d_error_string,
      s_error_string,
      str_pad(paste0(Sys.time(), "\n"), 25, "left", " ")
    ))

    # Stopping conditions ----
    if (iter > opts$max_iter) {
      cat(rule(width = 76), "\n")
      break
    }
    # if (eff < opts$eff_threshold) break
  }

  cat("\n")
  cli_h1("Cleaning up design environment")

}
