#' Create the experimental design
#'
#' Design takes the utility functions and design optsions as inputs and generates
#' candidate designs. Design cannot be run without first validating the utility
#' functions and design optsions. Running LINK TO VALIDATE will create a validated
#' environment, which is the context for evaluating the designs.
#'
#' @param V A list of utility functions
#' @param opts A list of design optsions
#' @param candidate_set A matrix or data frame in the "wide" format containing
#' all permitted combinations of attributes. The default is NULL. If no candidate
#' set is provided, then the full factorial subject to specified restrictions
#' will be used.
#'
#' @return A design object of class designer.design
#'
#' @export
design <- function(V, opts, candidate_set = NULL) {
  cli_h1("Setting up the design environment")

  # Initial checks ----
  cli_h2("Running initial checks")

  # Checks on V
  ansi_with_hidden_cursor(check_v(V))

  # Checks on opts
  ansi_with_hidden_cursor(check_opts(opts))

  # Checks on the candidate set
  ansi_with_hidden_cursor(check_candidate_set(candidate_set))

  # Parse utility ----
  cli_h2("Parsing the utility expression")
  parsed_v <- parse_utility(V)
  cli_alert_success("The supplied utility expression 'V' has been parsed")

  # Apply restrictions to the candidate set (create one if not supplied) ----
  cli_h2("Applying restrictions to the candidate set")
  if (is.null(candidate_set)) {
    candidate_set <- make_full_factorial(parsed_v$attrs)
    cli_alert_success("Full factorial created")
  }

  cli_alert_success("All restrictions successfully applied")

  # Set up parallel ----

  # Evaluate designs ----
  cli_h1("Evaluating designs")

  # Without proper stopping conditions this becomes an infinite loop
  iter <- 1
  repeat {
    # Set up the initial printing to console (if statement for efficiency)
    if (iter == 1) {
      cat("\n")
      cat(str_c(
        str_pad("Iteration", 12, "left", " "),
        str_pad("A-error", 10, "left", " "),
        str_pad("C-error", 10, "left", " "),
        str_pad("D-error", 10, "left", " "),
        str_pad("S-error", 10, "left", " "),
        str_pad("Time stamp\n", 15, "left", " ")
      ))
      cat(str_pad(rule(width = 66), 68, "left", " "))
    }

    # This is where the meat of the repeat is, i.e. grab design and calculate efficency

    # If we have a new current best print to console. The print should use a different
    # colour for the criteria used to optsimize the design. Inside the same if()
    # statement, we will update the list of valid designs.
    # If our current design is among the top 10, make adjustments to the list
    # e.g. if 5, then place and drop 10. so that we always have the top 10 results.




    # Stopping conditions
    # if (iter > opts$max_iter) break
    # if (eff < opts$eff_threshold) break

    # Update the iteration counter
    iter <- iter + 1

    # Break here for testing purposes ONLY
    break
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
