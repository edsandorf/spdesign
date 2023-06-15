#' Find a design using a modified Federov algorithm
#'
#' The modified Federov algorithm implemented here starts with a random design
#' candidate and systematically swaps out rows of the design candidate to
#' iteratively find better designs. The algorithm has the following steps and
#' restrictions.
#'
#' 1) Create a random initial design and evaluate it.
#' 2) Swap the first row of the design candidate with the first row of the
#'    candidate set.
#' 3) If no better candidate is found, try the second row of the candidate set.
#'    Keep trying new rows of the candidate set until an improvement is found.
#' 4) If a better candidate is found, then we try to swap out the next row in
#'    the design candidate with the first row of the candidate set. Keep
#'    repeating the previous step.
#' 5) When all rows of the design candidate has been swapped once, reset the
#'    counter and work through the design candidate and candidate set again.
#' 6) The algorithm terminates after a pre-determined number of iterations or
#'    when a pre-determined efficiency threshold has been found.
#'
#' NOTE: I have not yet implemented a duplicate check! That is, I do not check
#'       whether the "same" choice rows are included but with the order of
#'       alternatives swapped. This can be achieved by further restricting the
#'       candidate set prior to searching for designs. That said, "identical"
#'       choice rows will not provide much additional information and should
#'       be excluded by default in the search process.
#'
#' @param design_object A list of class 'spdesign' created within the
#' \code{\link{generate_design}} function
#' @param prior_values A list of priors
#'
#' @inheritParams generate_design
#'
#' @return A list of class 'spdesign'
federov <- function(design_object,
                    model,
                    efficiency_criteria,
                    utility,
                    prior_values,
                    dudx,
                    candidate_set,
                    rows,
                    control) {
  # Reorder the rows of the candidate set to create more randomness
  candidate_set <- candidate_set[sample(seq_len(nrow(candidate_set))), ]

  # Set up the design environment
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

  # Set iterations defaults
  iter <- 1
  iter_break <- 1
  iter_break_max <- nrow(candidate_set) * rows
  iter_new_candidates <- 1
  iter_design_candidate <- 1
  iter_candidate_set <- 1

  # Create an initial random design candidate. The design_candidate is a
  # data.frame()
  design_candidate <- random_design_candidate(utility,
                                              candidate_set,
                                              rows,
                                              control$sample_with_replacement)

  repeat {
    # Update the design candidate.
    design_candidate[iter_design_candidate, ] <- candidate_set[iter_candidate_set, ]

    # Full attribute level balance for the federov but include ranges.
    fits <- fits_lvl_occurrences(utility, design_candidate, rows)

    # This becomes an infinite loop at some point. Why?
    while (fits == FALSE) {
      design_candidate[iter_design_candidate, ] <- candidate_set[iter_candidate_set, ]
      fits <- fits_lvl_occurrences(utility, design_candidate, rows)

      iter_candidate_set <- iter_candidate_set + 1

      if (iter_candidate_set == nrow(candidate_set)) {
        iter_candidate_set <- 1
        iter_design_candidate <- ifelse(iter_design_candidate == rows,
                                        1, iter_design_candidate + 1)

      } else {
        iter_candidate_set <- iter_candidate_set + 1

      }

      # Add iteration here to avoid an infinite loop
      iter_break <- iter_break + 1

      if (iter_break > iter_break_max) {
        # Try with a new random design candidate to check if we are truly stuck.
        design_candidate <- random_design_candidate(utility,
                                                    candidate_set,
                                                    rows,
                                                    control$sample_with_replacement)

        # Reset counters
        iter_candidate_set <- iter_design_candidate <- iter_break <- 1

        # Add to the new candidate counter
        iter_new_candidates <- iter_new_candidates + 1

        # After we have tried five new candidates
        if (iter_new_candidates > 5) {
          cli_alert_warning("We are struggeling to find a better design candidate that fits all level restrictions. You may want to stop here and try with looser restrictions. Alternatively, you can see if you get better results using the 'random' algorithm.")
        }

        break
      }
    }

    # Check occurrences before calling the model.matrix() this ensures that
    # dummies are correctly handled
    # Define the current design candidate considering alternative specific
    # attributes and interactions
    design_candidate_current <- do.call(cbind, define_base_x_j(utility, design_candidate))

    # Evaluate the design candidate (wrapper function)
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
      iter_candidate_set <- iter_candidate_set + 1
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

      # Reset the iterator over candidate set and add to the iterator over the
      # design candidate. If iter_design_candidate is greater than the number of
      # rows in the design, then reset to 1.
      iter_candidate_set <- 1
      iter_design_candidate <- ifelse(iter_design_candidate == rows,
                                      1, iter_design_candidate + 1)

    } else {
      # If we have iterated through the entire candidate set for one row in the
      # design candidate, we reset the candidate set counter and increment the
      # the design candidate counter by 1
      if (iter_candidate_set == nrow(candidate_set)) {
        iter_candidate_set <- 1
        iter_design_candidate <- ifelse(iter_design_candidate == rows,
                                        1, iter_design_candidate + 1)

      } else {
        iter_candidate_set <- iter_candidate_set + 1

      }
    }

    # Check stopping conditions ----
    if (iter > control$max_iter) {
      cat(rule(width = 76), "\n")
      cli_alert_info("Maximum number of iterations reached.")

      break
    }

    if (efficiency_outputs[["efficiency_measures"]][efficiency_criteria]  < control$efficiency_threshold) {
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
