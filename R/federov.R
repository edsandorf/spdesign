#' Find a design using a modified Federov algorithm
#'
#' The modified Federov algorithm implemented here starts with a random design
#' candidate and systematically swaps out rows of the design candidate to
#' iteratively find better designs. It is a first-improvement variant: a swap
#' is kept as soon as it improves the design, instead of searching the entire
#' candidate set for the best swap. A swap that does not improve the design is
#' discarded. The algorithm has the following steps and restrictions.
#'
#' 1) Create a random initial design and evaluate it. If level occurrences are
#'    specified, the initial design satisfies them.
#' 2) Swap the first row of the design candidate with the first row of the
#'    candidate set.
#' 3) If no better candidate is found, try the next row of the candidate set.
#'    Keep trying new rows of the candidate set until an improvement is found.
#'    NOTE: A swap is skipped without being evaluated if it would include the
#'    same row multiple times or violate the level occurrences.
#' 4) If a better candidate is found, then we try to swap out the next row in
#'    the design candidate, continuing with the next row of the candidate set.
#'    If no row of the candidate set improves the current row of the design
#'    candidate, move on to the next row.
#' 5) When the end of the design candidate or the candidate set is reached,
#'    continue from the first row. This way every row of the candidate set is
#'    tried equally often.
#' 6) If a full pass over all rows of the design candidate and the candidate
#'    set finds no improvement, the design candidate is a local optimum. Store
#'    it and restart from step 1 with a new random design candidate.
#' 7) The algorithm terminates after a pre-determined number of iterations or
#'    when a pre-determined efficiency threshold has been found. Pressing Esc
#'    (or Ctrl + C) stops the search and returns the best design found so far.
#'
#' The best design found across all runs is returned as the design. The best
#' design of each run, including the one that is still running when the search
#' stops, is stored in the list element 'runs' together with its efficiency
#' criteria.
#'
#' For very large candidate sets, the maximum number of iterations decides how
#' deep into the candidate set the search goes. Every swap moves one row further
#' through the candidate set, but only evaluated swaps count towards the maximum
#' number of iterations. A search that stops after fewer iterations than there
#' are rows in the candidate set may not have tried all of them.
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
federov <- function(
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
  # Reorder the rows of the candidate set to create more randomness
  candidate_set <- candidate_set[sample(seq_len(nrow(candidate_set))), ]

  # Set up the design environment
  design_env <- new.env()

  list2env(
    list(utility_string = update_utility(utility)),
    envir = design_env
  )

  # Set iterations defaults
  iter <- 1
  iter_no_improve <- 0
  idx_candidate <- 0
  idx_design <- 1
  n_candidates <- nrow(candidate_set)
  efficiency_current_best <- Inf # best design found so far
  efficiency_design <- Inf # current design candidate
  new_start <- TRUE
  run_best <- NULL

  # Create an initial random design candidate. The design_candidate is a
  # data.frame()
  design_candidate <- random_design_candidate(
    utility,
    candidate_set,
    rows,
    FALSE
  )
  # control$sample_with_replacement)

  # Level occurrences are fixed during the search, so parse them once
  has_occurrences <- level_occurrences_specified(utility)

  if (has_occurrences) {
    ranges <- occurrences(utility, rows)
    lvls <- expand_attribute_levels(utility)
  }

  # Return the best design found so far if the search is interrupted
  tryCatch(
    repeat {
      # If a full pass over all rows of the design candidate and the candidate
      # set finds no improvement, we are at a local optimum. Store the best
      # design of this run and restart from a new random design candidate.
      if (iter_no_improve >= n_candidates * rows) {
        if (!is.null(run_best)) {
          design_object[["runs"]] <- c(design_object[["runs"]], list(run_best))
        }

        # Only report the first restart to avoid flooding the console
        if (length(design_object[["runs"]]) == 1) {
          cli_alert_info(
            "No single swap improves the design. Restarting from a new random design candidate. Further restarts are not reported."
          )
        }

        design_candidate <- random_design_candidate(
          utility,
          candidate_set,
          rows,
          FALSE
        )
        efficiency_design <- Inf
        iter_no_improve <- 0
        new_start <- TRUE
        run_best <- NULL
      }

      # Try swaps on a copy so that a rejected swap never needs undoing. The
      # first iteration of each run evaluates its starting design without a
      # swap.
      trial <- design_candidate

      if (!new_start) {
        # Continue through the candidate set where we left off. Move on to the
        # next row of the design candidate if no row of the candidate set
        # improves the current one
        idx_candidate <- idx_candidate %% n_candidates + 1

        if (iter_no_improve > 0 && iter_no_improve %% n_candidates == 0) {
          idx_design <- idx_design %% rows + 1
        }

        iter_no_improve <- iter_no_improve + 1

        trial[idx_design, ] <- candidate_set[idx_candidate, ]

        # Skip swaps that duplicate a row already in the design candidate or
        # violate the level occurrences
        if (anyDuplicated(trial) > 0) {
          next
        }

        if (
          has_occurrences &&
            lvl_violation(utility, trial, rows, ranges, lvls) > 0
        ) {
          next
        }
      }

      # Define the current design candidate considering alternative specific
      # attributes and interactions
      design_candidate_current <- do.call(
        cbind,
        define_base_x_j(utility, trial)
      )

      # Evaluate the design candidate (wrapper function)
      efficiency_outputs <- evaluate_design_candidate(
        utility,
        trial,
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

      # Accept the swap if it improves the current design candidate. A design
      # where the efficiency criteria is NA is never accepted.
      if (
        !is.na(efficiency_current) &&
          efficiency_current < efficiency_design
      ) {
        design_candidate <- trial
        efficiency_design <- efficiency_current
        iter_no_improve <- 0
        run_best <- list(
          design = tibble::as_tibble(design_candidate_current),
          efficiency_criteria = efficiency_outputs[["efficiency_measures"]]
        )

        # Update the best design found so far across all runs
        if (efficiency_current < efficiency_current_best) {
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

          design_object[["design"]] <- design_candidate_current
          design_object[["efficiency_criteria"]] <- efficiency_outputs[[
            "efficiency_measures"
          ]]
          design_object[["vcov"]] <- efficiency_outputs[["vcov"]]
          efficiency_current_best <- efficiency_current

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
        }

        # Move on to the next row of the design candidate, continuing with the
        # next row of the candidate set. Accepting the starting design does not
        # move on, so that the first swap is in the current row.
        if (!new_start) {
          idx_design <- idx_design %% rows + 1
        }
      }

      new_start <- FALSE

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

  # Store the best design of the last run
  if (!is.null(run_best)) {
    design_object[["runs"]] <- c(design_object[["runs"]], list(run_best))
  }

  cli_alert_info(
    "Completed {length(design_object[['runs']])} run{?s} of the search."
  )

  # Return the design candidate
  return(
    design_object
  )
}
