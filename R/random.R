#' Make a random design candidate
#'
#' Generates a random design by sampling from the candidate set each update of
#' the algorithm.
#'
#' @inheritParams federov
#' @inheritParams generate_design
#'
#' @return A list of class 'spdesign'
random <- function(design_object,
                   model,
                   efficiency_criteria,
                   utility,
                   priors,
                   didx,
                   candidate_set,
                   tasks,
                   control) {

  # Set up the design environment
  design_environment <- new.env()

  list2env(
    list(utility_string = clean_utility(utility)),
    envir = design_environment
  )

  # Set defaults
  iter <- 1

  # Make sure that design_object is returned on exit
  on.exit(
    return(design_object),
    add = TRUE
  )

  repeat{
    # Create a random design candidate
    design_candidate <- random_design_candidate(candidate_set,
                                                tasks,
                                                control$sample_with_replacement)

    # Define the current design candidate considering alternative specific
    # attributes and interactions
    design_candidate_current <- do.call(cbind,
                                        define_base_x_j(utility,
                                                        design_candidate))

    # Evaluate the design candidate (wrapper function)
    efficiency_measures <- evaluate_design_candidate(
      utility,
      design_candidate,
      priors,
      design_environment,
      model,
      didx,
      return_all = FALSE,
      significance = 1.96
    )

    # Get the current efficiency measure
    efficiency_current <- efficiency_measures[efficiency_criteria]
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
        values = efficiency_measures,
        criteria = c("a-error", "c-error", "d-error", "s-error"),
        digits = 4,
        padding = 10,
        width = 80,
        efficiency_criteria
      )

      # Update current best criteria
      design_object[["design"]] <- design_candidate_current
      design_object[["efficiency_criteria"]] <- efficiency_measures
      efficiency_current_best <- efficiency_current

    }

    # Check stopping conditions ----
    if (iter > control$max_iter) {
      cat(rule(width = 76), "\n")
      cli_alert_info("Maximum number of iterations reached.")

      break
    }

    if (efficiency_measures[efficiency_criteria] < control$efficiency_threshold) {
      cat(rule(width = 76), "\n")
      cli_alert_info("Efficiency criteria is less than threshhold.")

      break
    }

    # Add to the iteration
    iter <- iter + 1


  }

  # Return the design candidate
  return(
    design_object
  )

}

#' Create a random design candidate
#'
#' Sample from the candidate set to create a random design.
#'
#' @param sample_with_replacement A boolean equal to TRUE if we sample from the
#' candidate set with replacement. The default is FALSE
#' @inheritParams generate_design
random_design_candidate <- function(candidate_set,
                                    tasks,
                                    sample_with_replacement) {

  idx_rows <- sample(
    seq_len(nrow(candidate_set)),
    tasks,
    replace = sample_with_replacement
  )

  return(
    as.data.frame(candidate_set[idx_rows, ])
  )

}
