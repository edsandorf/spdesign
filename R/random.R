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
random <- function(
  design_object,
  model,
  efficiency_criteria,
  utility,
  prior_values,
  dudx,
  candidate_set,
  rows,
  save_designs,
  control
) {
  # Set up the design_object environment
  design_env <- new.env()

  list2env(
    list(utility_string = update_utility(utility)),
    envir = design_env
  )

  # Set iteration defaults
  iter <- 1
  efficiency_current_best <- Inf

  # Return the best design found so far if the search is interrupted
  tryCatch(
    repeat {
      # Create a random design_object candidate
      design_candidate <- random_design_candidate(
        utility,
        candidate_set,
        rows,
        control$sample_with_replacement
      )

      # Define the current design_object candidate considering alternative specific
      # attributes and interactions
      design_candidate_current <- do.call(
        cbind,
        define_x_j(utility, design_candidate, interactions = FALSE)
      )

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
      efficiency_current <- efficiency_outputs[["efficiency_measures"]][
        efficiency_criteria
      ]

      # Accept the design candidate if it improves the design. A design where the
      # efficiency criteria is NA is never accepted.
      if (
        !is.na(efficiency_current) &&
          efficiency_current < efficiency_current_best
      ) {
        # Print information to console and update ----
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
        design_object[["efficiency_criteria"]] <- efficiency_outputs[[
          "efficiency_measures"
        ]]
        design_object[["vcov"]] <- efficiency_outputs[["vcov"]]
        efficiency_current_best <- efficiency_current
      }

      # Save designs
      if (save_designs) {
        saveRDS(
          design_object,
          file = paste0(
            "design_iter_",
            formatC(iter, width = 6, flag = "0"),
            ".rds"
          )
        )
      }

      # Check stopping conditions ----
      if (iter > control$max_iter) {
        cat(rule(width = 76), "\n")
        cli_alert_info("Maximum number of iterations reached.")

        break
      }

      if (efficiency_current_best < control$efficiency_threshold) {
        cat(rule(width = 76), "\n")
        cli_alert_info("Efficiency criteria is less than threshhold.")

        break
      }

      # Add to the iteration
      iter <- iter + 1
    },
    interrupt = function(e) {
      cat(rule(width = 76), "\n")
      cli_alert_info(
        "Search interrupted. Returning the best design found so far."
      )
    }
  )

  # Return the design_object candidate
  return(
    design_object
  )
}

#' Create a random design_object candidate
#'
#' Sample from the candidate set to create a random design_object. If level
#' occurrences are specified, single rows are swapped with random rows from the
#' candidate set until the design_object candidate satisfies them. A swap is
#' kept if it does not move the design_object candidate further away from the
#' level occurrences.
#'
#' @param sample_with_replacement A boolean equal to TRUE if we sample from the
#' candidate set with replacement. The default is FALSE
#' @param print_counter A boolean equal to TRUE if we print the number of
#' attempts to find a design candidate every 1000th attempt. The default is
#' FALSE
#' @inheritParams generate_design
random_design_candidate <- function(
  utility,
  candidate_set,
  rows,
  sample_with_replacement,
  print_counter = FALSE
) {
  # Set overall variables
  show_warning <- TRUE
  time_start <- Sys.time()
  counter <- 1

  idx <- sample(nrow(candidate_set), rows, replace = sample_with_replacement)

  if (!level_occurrences_specified(utility)) {
    return(candidate_set[idx, ])
  }

  # Level occurrences are fixed during the search, so parse them once
  ranges <- occurrences(utility, rows)
  lvls <- expand_attribute_levels(utility)

  violation <- lvl_violation(utility, candidate_set[idx, ], rows, ranges, lvls)

  # Swap single rows, keeping swaps that do not move the design candidate
  # further away from the level occurrences
  while (violation > 0) {
    if (show_warning && difftime(Sys.time(), time_start, units = "secs") > 60) {
      cli_alert_info(
        "No design candidate has been found. This could be because you have place too tight constraints on the design or that all design candidates result in a singular Fisher matrix. A singular Fisher matrix can happen if you have perfect multicollinearity in your utility functions."
      )
      show_warning <- FALSE
    }

    if (print_counter && counter %% 1000 == 0) {
      cli_alert_info("Design candidate attempts: {counter}")
    }

    counter <- counter + 1

    new_row <- sample(nrow(candidate_set), 1)
    if (!sample_with_replacement && new_row %in% idx) next

    new_idx <- replace(idx, sample(rows, 1), new_row)
    new_violation <- lvl_violation(
      utility,
      candidate_set[new_idx, ],
      rows,
      ranges,
      lvls
    )

    if (new_violation <= violation) {
      idx <- new_idx
      violation <- new_violation
    }
  }

  return(
    candidate_set[idx, ]
  )
}
