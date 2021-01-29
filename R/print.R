#' Prints the initial header for the table of results
#'
#' The function prints the initial header for the console output and colors in
#' the criteria used for optimization. Effectively, the function makes multiple
#' calls to \code{\link{cat}}
#'
#' @param opts A list of options
#' @param padding An integer specifying the padding of each column element.
#' Default value is 10
#' @param width An integer giving the width of the horizontal rules. Default
#' value is 80
#'
#' @return Noting
print_initial_header <- function(opts, padding = 10, width = 80) {
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

#' Creates a printable version of the efficiency criteria
#'
#' The function is only meant for internal use to handle pretty printing to
#' console.
#'
#' @param iter An integer giving the iteration of the loop
#' @param values The value of the efficiency criteria obtained by
#' \code{\link{calculate_efficiency_criteria}}
#' @param criteria A character string with the name of the efficiency criteria.
#' See manual for valid values
#' @param digits The nubmer of digits to round the printed value to. The default
#' is 4.
#' @param padding An integer specifying the padding of each column element.
#' Default value is 10.
#' @param opts The list of design options. The default is NULL, but must be
#' specified to print the criteria in colour.
#'
#' @return A character string.
print_efficiency_criteria <- function(
  iter,
  values,
  criteria,
  digits = 4,
  padding = 10,
  opts = NULL
) {
  printable_string <- lapply(seq_along(values), function(i) {
    if (is.na(values[[i]])) {
      string <- "N/A"
    } else {
      string <- as.character(round(values[[i]], digits))
    }

    if (!is.null(opts) && criteria[[i]] %in% opts$efficiency_criteria) {
      col_green(str_pad(string, 10, "left", " "))
    } else {
      str_pad(string, 10, "left", " ")
    }
  })

  cat(
    str_c(
      str_pad(as.character(iter), padding, "left", " "),
      do.call(str_c, printable_string),
      str_pad(paste0(Sys.time(), "\n"), max(padding, 25), "left", " ")
    )
  )
}
