#' Exclude rows from the candidate set
#'
#' The function takes the list of exclusions and transforms them into an
#' expression that is then parsed and evaluated to apply the exclusions
#' to the supplied candidate set using standard subsetting routines.
#'
#' @inheritParams generate_design
#'
#' @return A restricted candidate set
exclude <- function(candidate_set, exclusions) {
  attribute_names <- names(candidate_set)

  # Reformat the exclusions
  exclusions <- lapply(seq_along(exclusions), function(i) {
    restriction <- exclusions[[i]]

    for (j in seq_along(attribute_names)) {
      restriction <- str_replace_all(
        restriction,
        attribute_names[[j]],
        paste0("candidate_set$", attribute_names[[j]])
      )
    }

    # Implicit return
    paste0("candidate_set[!(",  restriction, "), , drop = FALSE]")
  })

  # Apply the exclusions
  for (i in seq_along(exclusions)) {
    candidate_set <- eval(parse(text = exclusions[[i]]))
  }

  # Return
  return(
    candidate_set
  )
}
