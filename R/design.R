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
        str_pad("A-error", 10, "left", " "),
        str_pad("C-error", 10, "left", " "),
        str_pad("D-error", 10, "left", " "),
        str_pad("S-error", 10, "left", " "),
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
    # design_vcov <- eval(derive_vcov(type = opts$model), envir = design_environment)
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
    c_error <- tryCatch({
      calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "c_efficiency")
    },
    error = function(e) {
     NA
    })
    d_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "d_efficiency")
    s_error <- calculate_efficiency_criteria(design_vcov, p, opts$didx, all = FALSE, type = "s_efficiency")
    cat(str_c(
      str_pad(as.character(iter), 10, "left", " "),
      str_pad(as.character(round(a_error, 4)), 10, "left", " "),
      str_pad(if (is.na(c_error)) "NA" else as.character(round(c_error, 4)), 10, "left", " "),
      str_pad(as.character(round(d_error, 4)), 10, "left", " "),
      str_pad(as.character(round(s_error, 4)), 10, "left", " "),
      str_pad(paste0(Sys.time(), "\n"), 25, "left", " ")
    ))

    # If we have a new current best print to console. The print should use a different
    # colour for the criteria used to optsimize the design. Inside the same if()
    # statement, we will update the list of valid designs.
    # If our current design is among the top 10, make adjustments to the list
    # e.g. if 5, then place and drop 10. so that we always have the top 10 results.




    # Stopping conditions
    if (iter > opts$max_iter) break
    # if (eff < opts$eff_threshold) break



    # Break here for testing purposes ONLY
    # break
  }

  # Th e output to console every time a better model is found.
  # The error columns
  # Nr  D-error D-error_diff  Time-stamp
  # 1   0.6434  0.0023        2021-01-20 10:04:94


  # If all checks pass, then we need to build the vector of parameters and
  # and model matrix.
  # Finish up and get ready to report results
  cat("\n")
  cli_h1("Cleaning up design environment")

}
