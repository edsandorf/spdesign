#' Apply restrictions to the candidate set
#'
#' The function takes the list of restrictions and transforms them into an
#' expression that is then parsed and evaluated to apply the restrictions
#' to the supplied candidate set using standard subsetting routines.
#'
#' @inheritParams generate_design
#'
#' @return A restricted candidate set
apply_restrictions <- function(candidate_set, restrictions) {
  attribute_names <- names(candidate_set)

  # Reformat the restrictions
  restrictions <- lapply(seq_along(restrictions), function(i) {
    restriction <- restrictions[[i]]

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

  # Apply the restrictions
  for (i in seq_along(restrictions)) {
    candidate_set <- eval(parse(text = restrictions[[i]]))
  }

  # Return
  return(
    candidate_set
  )
}
