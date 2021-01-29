#' Prints the initial header for the table of results
#'
#' The function prints the initial header for the console output and colors in
#' the criteria used for optimization. Effectively, the function makes multiple
#' calls to \code{\link{cat}}
#'
#' @param opts A list of options
#' @param padding An integer specifying the padding of each column element
#' @param width An integer giving the width of the horizontal rules
#'
#' @return Noting
print_initial_header <- function(opts, padding, width) {
  cat("\n")
  cat(rule(width = width), "\n")
  cat(
    str_c(
      str_pad("Iteration", padding, "left", " "),
      if ("a-error" %in% opts$efficiency_criteria) {
        col_green(str_pad("A-error", padding, "left"))
      } else {
        str_pad("A-error", padding, "left", " ")
      },
      if ("c-error" %in% opts$efficiency_criteria) {
        col_green(str_pad("C-error", padding, "left"))
      } else {
        str_pad("C-error", padding, "left", " ")
      },
      if ("d-error" %in% opts$efficiency_criteria) {
        col_green(str_pad("D-error", padding, "left"))
      } else {
        str_pad("D-error", padding, "left", " ")
      },
      if ("s-error" %in% opts$efficiency_criteria) {
        col_green(str_pad("S-error", padding, "left"))
      } else {
        str_pad("S-error", padding, "left", " ")
      },

      str_pad("Time stamp\n", max(padding, 25), "left", " ")
    )
  )
  cat(rule(width = width), "\n")
}
