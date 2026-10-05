#' Exclude rows from the candidate set
#'
#' Each exclusion is evaluated with the columns of the candidate set as
#' variables, and the rows where it is TRUE are removed.
#'
#' @inheritParams generate_design
#'
#' @return A restricted candidate set
exclude <- function(candidate_set, exclusions) {
  for (restriction in exclusions) {
    candidate_set <- candidate_set[
      !eval(parse(text = restriction), envir = candidate_set), ,
      drop = FALSE
    ]
  }

  return(
    candidate_set
  )
}
