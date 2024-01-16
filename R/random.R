#' Make a random design
#'
#' Generates a random design by sampling from the candidate set each update of
#' the algorithm.
#'
#' With no restrictions placed, this type of design will only consider efficiency.
#' There is no guarantee that you will achieve attribute level balance, nor that
#' all attribute levels will be present. More efficient designs tend to have
#' more extreme trade-offs.
#'
#' @inheritParams federov
#' @inheritParams generate_design
#'
#' @return A list of class 'spdesign'
random <- function(design_object,
                   model,
                   efficiency_criteria,
                   utility,
                   prior_values,
                   dudx,
                   candidate_set,
                   rows,
                   control) {

  # Set up the design_object environment
  design_env <- new.env()

  list2env(
    list(utility_string = update_utility(utility)),
    envir = design_env
  )

  # Make sure that design_object is returned on exit
  on.exit(
    return(design_object),
    add = TRUE
  )

  # Set iteration defaults
  iter <- 1

  repeat {
    # Create a random design_object candidate
    design_candidate <- random_design_candidate(utility,
                                                candidate_set,
                                                rows,
                                                control$sample_with_replacement)

    # Define the current design_object candidate considering alternative specific
    # attributes and interactions
    design_candidate_current <- do.call(cbind, define_base_x_j(utility, design_candidate))

    # Evaluate the design_object candidate (wrapper function)
    efficiency_outputs <- evaluate_design_candidate(
      utility,
      design_candidate,
      prior_values,
      design_env,
      model,
      dudx,
      return_all = FALSE,
      significance = 1.96
    )

    # Get the current efficiency measure
    efficiency_current <- efficiency_outputs[["efficiency_measures"]][efficiency_criteria]
    if (iter == 1) efficiency_current_best <- efficiency_current

    # If the efficiency criteria we optimize for is NA, try a new candidate
    if (is.na(efficiency_current)) {
      iter <- iter + 1
      next

    }

    # Print information to console and update ----
    if (efficiency_current < efficiency_current_best || iter == 1) {
      print_iteration_information(
        iter,
        values = efficiency_outputs[["efficiency_measures"]],
        criteria = c("a-error", "c-error", "d-error", "s-error"),
        digits = 4,
        padding = 10,
        width = 80,
        efficiency_criteria
      )

      # Update current best criteria
      design_object[["design"]] <- design_candidate_current
      design_object[["efficiency_criteria"]] <- efficiency_outputs[["efficiency_measures"]]
      design_object[["vcov"]] <- efficiency_outputs[["vcov"]]
      efficiency_current_best <- efficiency_current

    }

    # Check stopping conditions ----
    if (iter > control$max_iter) {
      cat(rule(width = 76), "\n")
      cli_alert_info("Maximum number of iterations reached.")

      break
    }

    if (efficiency_outputs[["efficiency_measures"]][efficiency_criteria] < control$efficiency_threshold) {
      cat(rule(width = 76), "\n")
      cli_alert_info("Efficiency criteria is less than threshhold.")

      break
    }

    # Add to the iteration
    iter <- iter + 1

  }

  # Return the design_object candidate
  return(
    design_object
  )

}

#' Create a random design_object candidate
#'
#' Sample from the candidate set to create a random design_object.
#'
#' @param sample_with_replacement A boolean equal to TRUE if we sample from the
#' candidate set with replacement. The default is FALSE
#' @inheritParams generate_design
random_design_candidate <- function(utility,
                                    candidate_set,
                                    rows,
                                    sample_with_replacement) {

  fits <- FALSE
  show_warning <- TRUE
  time_start <- Sys.time()

  while (fits == FALSE) {
    idx_rows <- sample(
      seq_len(nrow(candidate_set)),
      rows,
      replace = sample_with_replacement
    )

    design_candidate <- candidate_set[idx_rows, ]

    fits <- fits_lvl_occurrences(utility, design_candidate, rows)

    if (show_warning && difftime(Sys.time(), time_start, units = "secs") > 60) {
      cli_alert_info("No design candidate has been found. This could be because you have place too tight constraints on the design or that all design candidates result in a singular Fisher matrix. A singular Fisher matrix can happen if you have perfect multicollinearity in your utility functions.")
      show_warning <- FALSE
    }
  }

  return(
    design_candidate
  )

}
