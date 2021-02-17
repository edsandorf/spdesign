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

  # Options ----
  cli_h2(
    "Checking the supplied list of options and setting defaults"
  )

  ansi_with_hidden_cursor(
    check_opts(opts)
  )

  opts <- set_default_options(opts)

  cli_alert_success(
    "Default options has been set"
  )

  # Utility ----
  cli_h2(
    "Checking and parsing utility"
  )

  ansi_with_hidden_cursor(
    check_utility(utility)
  )

  parsed_utility <- parse_utility(utility, opts)

  cli_alert_success(
    "The supplied utility functions have been parsed"
  )

  # Candidate set ----
  cli_h2(
    "Checking the candidate set and applying restrictions"
  )

  if (is.null(candidate_set)) {
    cli_alert_info(
      "No candidate set supplied. The design will use the full factorial
      subject to supplied constraints."
    )

    candidate_set <- generate_full_factorial(parsed_utility$attrs)

    cli_alert_success(
      "Full factorial created"
    )
  } else {
    ansi_with_hidden_cursor(
      check_candidate_set(candidate_set, parsed_utility)
    )

    cli_alert_info(
      "The attribute levels are determined based on the supplied candidate set
      and not the levels specified in the utility functions."
    )

    parsed_utility[["attrs"]] <- lapply(
      as.list(
        as.data.frame(candidate_set)
      ),
      unique
    )
  }

  candidate_set <- apply_restrictions(candidate_set, opts$restrictions)
  candidate_set <- as.matrix(candidate_set)

  cli_alert_success(
    "All restrictions successfully applied"
  )

  # Prepare the list of priors ----
  cli_h2(
    "Preparing the list of priors"
  )

  priors <- prepare_priors(utility, parsed_utility, opts)

  cli_alert_success(
    "Priors prepared successfully"
  )

  # Set up the design environment ----
  design_environment <- new.env()
  list2env(
    list(utility_string = parsed_utility[["utility"]]),
    envir = design_environment
  )

  # Set up parallel ----
  if (opts$cores > 1) {
    cli_h2(
      "Preparing multicore estimation"
    )

    future::plan(
      future::multicore(workers = opts$cores)
    )

    cli_alert_success(
      "Multicore estimation prepared successfully"
    )
  }

  # Evaluate designs ----
  cli_h1(
    "Evaluating designs"
  )

  # Without proper stopping conditions this becomes an infinite loop
  iter <- 1
  iter_with_no_imp <- 1
  current_best <- NULL
  error_measures_string <- c("a-error", "c-error", "d-error", "s-error")
  time_start <- Sys.time()
  best_design_candidate <- NULL
  design_candidate <- NULL
  on.exit(
    return(best_design_candidate),
    add = TRUE
  )

  # Search for designs until the criteria are met
  repeat {
    # Create a design candidate
    design_candidate <- make_design_candidate(
      parsed_utility,
      candidate_set,
      design_candidate,
      opts,
      iter_with_no_imp,
      type = opts$algorithm$alg
    )

    # Determine x_j
    alt_names <- names(utility)
    x_j <- lapply(seq_along(utility), function(j) {
      frmla <- parsed_utility$formula_utility[[j]]
      model.matrix(frmla, design_candidate)
    })
    names(x_j) <- alt_names

    # This is not memory efficient, but we can return including interactions
    current_design_candidate <- do.call(cbind, x_j)

    # Update the column names of x_j
    for (j in seq_along(x_j)) {
      colnames(x_j[[j]]) <- str_replace_all(
        colnames(x_j[[j]]),
        paste0(alt_names[[j]], "_"),
        ""
      )
    }

    colnames_x_j <- unique(do.call(c, lapply(x_j, colnames)))
    attr_mat <- matrix(0, nrow(design_candidate), ncol = length(colnames_x_j),
                       dimnames = list(NULL, colnames_x_j))

    x_j <- lapply(x_j, function(x) {
      attr_mat[, colnames(x)] <- x
      attr_mat
    })

    # Update the design environment
    list2env(
      c(
        as.list(design_candidate),
        list(x_j = x_j)
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
      best_design_candidate <- current_design_candidate
      iter_with_no_imp <- 0
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
    iter_with_no_imp <- iter_with_no_imp + 1
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
