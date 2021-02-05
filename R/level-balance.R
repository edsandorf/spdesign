#' Function for getting and setting up the list of level occurrences
#'
#' The function is called from within \code{\link{parse_utility}} with the
#' purpose of getting the list of attributes, levels and occurrences wihtin the
#' design. This is necessary to impose attribute level balance or near attribute
#' level balance.
#'
#' @param string A character string with the utility expression
#' @param attrs A named list of attributes and levels
#' @param candidate_rows An integer giving the number of rows in each design
#' candidate, which is equal to the product of the number of alternatives,
#' tasks and blocks.
#'
#' @return A named list of lists where the outer list is for the attributes and
#' the inner list, the levels of each attribute and the number or range of times
#' they can occur
get_level_occurrence <- function(string, attrs, candidate_rows) {

  attribute_names <- names(attrs)
  n_attr_lvls <- lapply(attrs, length)

  # Set the level occurrence based on attribute level balance as if no info
  # is supplied
  level_occurrence <- lapply(
    n_attr_lvls,
    set_default_level_occurrence,
    candidate_rows
  )

  # Extract specified values and check if we have occurrence syntax present
  specified_values <- extract_specified(string, TRUE)
  expr <- "(?<=\\])\\(.*?\\)"
  occurrence_idx <- str_detect(specified_values, expr)
  occurrence_string <- specified_values[occurrence_idx]
  occurrence_names <- extract_attribute_names(occurrence_string)

  # Get the list of level occurrence
  if (!is.null(occurrence_names)) {
    occurrence_values <- vector(mode = "list", length(occurrence_names))
    names(occurrence_values) <- occurrence_names

    for (i in seq_along(occurrence_values)) {
      lvls <- eval(
        parse(
          text = paste0(
            "list",
            extract_level_occurrence(occurrence_string[[i]])
          )
        )
      )

      # Check if it only occurs once and repeat equal to the number of levels
      if (length(lvls) == 1) {
        lvls <- rep(lvls, length(attrs[[i]]))
      }

      names(lvls) <- paste0("lvl", seq_along(lvls))
      occurrence_values[[i]] <- lvls
    }

    # Replace the defaults with the specified
    level_occurrence[names(occurrence_values)] <- occurrence_values
  }

  # Test that occurrence is specified once or equal to the number of levels
  n_lvl_occurrence <- lapply(level_occurrence, length)
  length_diff <- mapply(
    "-",
    n_attr_lvls,
    n_lvl_occurrence,
    SIMPLIFY = TRUE
  )

  # Check that the attribute level is only specified equal to the
  if (any(length_diff != 0)) {
    stop(
      "The number of level occurrences specified can only be of length one or
        equal to the number of levels for the attribute"
    )
  }

  # Test if
  tmp <- lapply(level_occurrence, function(x) {
    sum_min <- Reduce("+", lapply(x, min))
    sum_max <- Reduce("+", lapply(x, max))
    !(candidate_rows %in% sum_min:sum_max)
  })

  if (any(do.call(c, tmp))) {
    stop(
      "The specified ranges for the attribute level occurrence or the number of
      times an attribute level should occur does not allow you to find a design.
      The number of rows in the design candidate must be either equal to the
      sum of individual occurrences of an attribute level or contained in the
      range of the sum of the minimum number of occurrences and the sum of the
      maximum number of level occurrences."
    )
  }

  # Implicitly return the list of level occurrences
  level_occurrence
}
