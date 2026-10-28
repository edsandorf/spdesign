#' Make a design candidate based on the rsc algorithm
#'
#' Depending on the setting the function calls a combination of
#' \code{\link{relabel}}, \code{\link{swap}} and \code{\link{cycle}}
#' to create new design candidates. The code is intentionally written modular
#' to allow for all special cases of the algorithm.
#'
#' Each iteration relabels or swaps the levels of a single column of the
#' current design candidate, working through the columns with more than one
#' level in turn. The change is kept if it improves the current design
#' candidate and discarded otherwise.
#'
#' If there is no improvement in `max_no_improve` iterations, the design
#' candidate is a local optimum. The search then restarts from a new base
#' design candidate. The best design found across all runs is returned as the
#' design. The best design of each run, including the one that is still running
#' when the search stops, is stored in the list element 'runs' together with
#' its efficiency criteria.
#'
#' @inheritParams federov
#'
rsc <- function(
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
  # Set up the design environment
  design_env <- new.env()

  list2env(
    list(utility_string = update_utility(utility)),
    envir = design_env
  )

  # Set iteration defaults
  iter <- 1
  iter_na <- 0
  iter_no_improve <- 1
  alg <- "relabel"
  efficiency_current_best <- Inf # best design found so far
  efficiency_design <- Inf # current design candidate
  run_best <- NULL

  # Return the best design found so far if the search is interrupted
  tryCatch(
    repeat {
      # Create an initial design candidate OR a new candidate when a sufficient
      # number of attempts have been made without any improvement. The best
      # design of each run is stored, and the new run starts from scratch.
      if (iter == 1 || iter_no_improve > control$max_no_improve) {
        if (!is.null(run_best)) {
          design_object[["runs"]] <- c(design_object[["runs"]], list(run_best))
        }

        # Only report the first restart to avoid flooding the console
        if (length(design_object[["runs"]]) == 1 && iter > 1) {
          cli_alert_info(paste0(
            "No improvement in ",
            control$max_no_improve,
            " iterations. Restarting from a new base design candidate. Further restarts are not reported."
          ))
        }

        efficiency_design <- Inf
        run_best <- NULL

        # Create a level balanced design candidate or near level balanced candidate
        design_candidate <- generate_rsc_candidate(utility, rows)

        # Transform the candiate set such that attributes that are dummy coded
        # are turned into factors. This ensures that we can use the model.matrix()
        for (i in which(names(design_candidate) %in% dummy_names(utility))) {
          design_candidate[, i] <- as.factor(design_candidate[, i])
        }

        # Only columns with more than one level can be relabeled or swapped
        varying <- which(vapply(
          design_candidate,
          function(x) length(unique(x)) > 1,
          logical(1)
        ))
        idx_column <- 0

        iter_no_improve <- 1
      }

      # Swith algorithm every max_relabel iterations
      if (iter %% control$max_relabel == 0) {
        alg <- ifelse(alg == "relabel", "swap", "relabel")
      }

      # Try a relabel or swap on a copy of the design candidate, so that a
      # rejected change never needs undoing. Work through the columns in turn.
      idx_column <- idx_column %% length(varying) + 1
      i <- varying[idx_column]

      trial <- design_candidate
      trial[, i] <- switch(
        alg,
        relabel = relabel(trial[, i]),
        swap = swap(trial[, i])
      )

      # Define the current design candidate considering alternative specific
      # attributes and interactions
      design_candidate_current <- do.call(
        cbind,
        define_x_j(utility, trial, interactions = FALSE)
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

      # Count consecutive design candidates where the efficiency criteria is NA
      if (is.na(efficiency_current)) {
        iter_na <- iter_na + 1
      } else {
        iter_na <- 0
      }

      if (iter_na >= 1000) {
        cli_alert_info(
          "You have tried 1000 design candidates all of which have produced a computationally singular Hessian matrix. Check yor design for identification problems."
        )
        iter_na <- 0
      }

      # Accept the design candidate if it improves the current design
      # candidate. A design where the efficiency criteria is NA is never
      # accepted.
      if (
        !is.na(efficiency_current) &&
          efficiency_current < efficiency_design
      ) {
        design_candidate <- trial
        efficiency_design <- efficiency_current
        run_best <- list(
          design = tibble::as_tibble(design_candidate_current),
          efficiency_criteria = efficiency_outputs[["efficiency_measures"]]
        )

        # Reset iter_no_improve when we have an improvement.
        iter_no_improve <- 1

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
        }
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

      # Add to the no improvement iterator. It's reset upon improvement
      iter_no_improve <- iter_no_improve + 1
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

#' Relabeling of attribute levels
#'
#' Relabels the attribute levels to create a new design candidate. For example,
#' if the column contains the levels (1, 2, 1, 3, 2, 3) and 1 and 3 are
#' relabeled, then the column becomes (3, 2, 3, 1, 2, 1), i.e. 1 becomes 3 and
#' 3 becomes 1.
#'
#' Will randomly sample 2 attribute levels that will be relabeled and the
#' relabeling is done independently for each column, which implies that
#' the same attribute will be relabeled differently depending on which
#' alternative it belongs to.
#'
#' @param x A vector of attribute levels
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
relabel <- function(x) {
  unique_lvls <- unique(x)

  # If we have a constant, the easiest is to sample with replacement. Adds a
  # tiny amount of overhead.
  replace <- length(unique_lvls) == 1
  lvls <- sample(unique_lvls, 2, replace = replace)

  idx_first <- x == lvls[[1]]
  idx_second <- x == lvls[[2]]

  x[idx_first] <- lvls[[2]]
  x[idx_second] <- lvls[[1]]

  return(x)
}

#' Swapping of attribute
#'
#' Swaps the order of the attributes to create a new design candidate. For
#' example, if the attributes in the first and fourth choice situation (row)
#' are swapped, then (1, 2, 1, 3, 2, 3) becomes( 3, 2, 1, 1, 2, 3).
#'
#' The algorithm randomly samples 2 row positions with different levels that
#' are swapped, so that every swap changes the design candidate. A column with
#' a single level is returned unchanged.
#'
#' @inheritParams relabel
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
swap <- function(x) {
  first <- sample(length(x), 1)
  candidates <- which(x != x[first])

  # A constant column cannot change
  if (length(candidates) == 0) {
    return(x)
  }

  second <- candidates[sample(length(candidates), 1)]
  x[c(first, second)] <- x[c(second, first)]

  return(x)
}

#' Cycling of attribute levels
#'
#' Cycles the attribute levels to create a new design candidate. "Cycling
#' replaces all attribute levels in each choice situation at the time by
#' replacing the first level with the second level, second level with the third
#' etc. Since this change affects all columns, cycling can only be performed if
#' all attributes have exactly the same sets of feasible levels,
#' (e.g., where all variables are dummy coded)." (p. 253).
#'
#' This part of the RSC algorithm is rarely invoked.
#'
#' @inheritParams relabel
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
#'
#' @return A cycled design candidate
cycle <- function(x) {}


#' Generates a candidate for the RSC algorithm
#'
#' Creates a design candidate by assuming attribute level balance. Will work
#' out the minimum level of times an attribute must occur for level balance. If
#' level balance cannot be achieved the function will systematically add level
#' occurrences to get as close as possible to attribute level balance.
#'
#' @inheritParams federov
#'
#' @return A data.frame with rows equal to the number of choice tasks and columns
#' equal to the number of attributes in the 'wide' format
generate_rsc_candidate <- function(utility, rows) {
  # Level occurrences will now systematically add to minimum occurrences such
  # that occurrences sum to 1
  level_occurrences <- lapply(
    lvl_occurrences(utility, rows, TRUE),
    function(x) {
      diff <- rows - sum(x)
      add <- rep(0, length(x))
      pos <- 1

      while (diff > 0) {
        if (pos == (length(x) + 1)) {
          pos <- 1
        }

        add[pos] <- add[pos] + 1
        pos <- pos + 1
        diff <- diff - 1
      }

      return(
        x + add
      )
    }
  )

  lvls <- lapply(level_occurrences, function(x) {
    return(
      shuffle(rep(as.numeric(names(x)), x))
    )
  })

  return(
    as.data.frame(do.call(cbind, lvls))
  )
}
