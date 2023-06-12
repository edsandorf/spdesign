#' Make a design candidate based on the rsc algorithm
#'
#' Depending on the setting the function calls a combination of
#' \code{\link{relabel}}, \code{\link{swap}} and \code{\link{cycle}}
#' to create new design candidates. The code is intentionally written modular
#' to allow for all special cases of the algorithm.
#'
#' @inheritParams federov
#'
rsc <- function(design_object,
                model,
                efficiency_criteria,
                utility,
                prior_values,
                dudx,
                candidate_set,
                rows,
                control) {
  # Create a level balanced design candidate or near level balanced candidate
  design_candidate <- generate_rsc_candidate(utility, rows)

  # Transform the candiate set such that attributes that are dummy coded
  # are turned into factors. This ensures that we can use the model.matrix()
  for (i in which(dummy_position(utility))) {
    design_candidate[, i] <- as.factor(design_candidate[, i])
  }

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

  # Set iteration defaults
  iter <- 1
  alg <- "relabel"

  repeat {

    # Swith algorithm every 10 000 iterations
    if (iter %% control$max_relabel == 0) {
      alg <- ifelse(alg == "relabel", "swap", "relabel")
    }

    # Get the design candidate
    for (i in seq_len(ncol(design_candidate))) {
      design_candidate[, i] <- switch(
        alg,
        relabel = relabel(design_candidate[, i]),
        swap = swap(design_candidate[, i])
      )
    }

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
#' The algorithm randomly samples 2 row positions that are swapped and
#' the swaps are independent across attributes and alternatives
#'
#' @inheritParams relabel
#'
#' @references
#' Hensher, D. A., Rose, J. M. & Greene, W., 2005, Applied Choice Analysis,
#' 2nd ed., Cambridge University Press
swap <- function(x) {
  pos <- sample(seq_len(length(x)), 2, replace = FALSE)

  pos_one_lvl <- x[[pos[[1]]]]
  pos_two_lvl <- x[[pos[[2]]]]

  x[[pos[[1]]]] <- pos_two_lvl
  x[[pos[[2]]]] <- pos_one_lvl

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
cycle <- function(x) {

}


#' Generates a candidate for the RSC algorithm
#'
#' The function will populate the design candidate matrix used for the RSC
#' algorithm.
#'
#' To ensure attribute level balance or that the number of times an attribute
#' level occurs is within the range specified in the list level_occurrence,
#' we sample from the list of occurrences subject to the constraint that the
#' sum has to be equal to the number of rows, i.e. choice tasks. While this is
#' a theoretical bottle neck for very large designs, it is still sufficiently
#' fast. It is only run once every 10 000 iterations of the RSC algorithm.
#'
#' @inheritParams federov
#'
#' @return A matrix with rows equal to the number of choice tasks and columns
#' equal to the number of attributes in the 'wide' format
generate_rsc_candidate <- function(utility, rows) {
  # Define attributes and level occurrence locally
  attribute_lvls <- expand_attribute_levels(utility)
  level_occurrence <- occurrences(utility, rows)

  # Loop over attributes
  design_candidate <- matrix(0, nrow = rows, ncol = length(attribute_lvls))
  colnames(design_candidate) <- names(attribute_lvls)

  for (i in seq_along(attribute_lvls)) {
    # Determine how many times to sample each level suject to summing to the
    # number of tasks
    constraints <- level_occurrence[[i]]
    sum_implied_tasks <- 1

    while (sum_implied_tasks != rows) {
      sample_level <- lapply(constraints, sample, 1L)
      sum_implied_tasks <- do.call(sum, sample_level)
    }

    # Sample each attribute level according to occurrence
    levels_tmp <- do.call(
      c,
      lapply(seq_along(sample_level), function(k) {
        rep(attribute_lvls[[i]][[k]], sample_level[[k]])
      })
    )

    design_candidate[, i] <- shuffle(levels_tmp)
  }
  return(
    as.data.frame(design_candidate)
  )

}
