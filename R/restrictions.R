#' Apply restrictions to the candidate set
#'
#' The function takes the list of restrictions and transforms them into an
#' expression that is then parsed and evaluated to apply the restrictions
#' to the supplied candidate set using standard subsetting routines.
#'
#' @param candidate_set A valid candidate set
#' @param restrictions A list of restrictions. Often this list will be pulled
#' directly from the list of options or it is a modified list of restrictions
#' following calls to _dummy or _effects coding.
#'
#' @return A restricted candidate set
apply_restrictions <- function(candidate_set, restrictions) {
  attrs <- names(candidate_set)

  # Reformat the restrictions
  restrictions <- lapply(seq_along(restrictions), function(i) {
    restriction <- restrictions[[i]]

    for (j in seq_along(attrs)) {
      restriction <- str_replace_all(
        restriction,
        attrs[[j]],
        paste0("candidate_set$", attrs[[j]])
      )
    }

    # Implicit return
    paste0("candidate_set[!(",  restriction, "), , drop = FALSE]")
  })

  # Apply the restrictions
  for (i in seq_along(restrictions)) {
    candidate_set <- eval(parse(text = restrictions[[i]]))
  }

  # Implicitly return the restricted candidate set
  candidate_set
}
