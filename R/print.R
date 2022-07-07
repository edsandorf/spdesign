#' Prints iteration information
#'
#' Prints iteration information every time a better design is found. The
#' function wraps around \code{\link{print_initial_header}} and
#' \code{\link{print_efficiency_criteria}}. This reduces the number of
#' if-statements and function calls within \code{\link{generate_design}} in an
#' attempt simplify code maintenance.
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
#' @param width An integer giving the width of the horizontal rules. Default
#' value is 80
#' @param efficiency_criteria The criteria that we optimize over
#'
#' @return Nothing
print_iteration_information <- function(
  iter,
  values,
  criteria,
  digits = 4,
  padding = 10,
  width = 80,
  efficiency_criteria
) {
  # Print the header at the first iteration
  if (iter == 1) {
    print_initial_header(efficiency_criteria, padding = 10, width = 80)
  }

  # Print information on the efficiency criteria
  print_efficiency_criteria(
    iter,
    values,
    criteria,
    digits = 4,
    padding = 10,
    efficiency_criteria
  )

}

#' Prints the initial header for the table of results
#'
#' The function prints the initial header for the console output and colors in
#' the criteria used for optimization. Effectively, the function makes multiple
#' calls to \code{\link{cat}}.
#'
#' @inheritParams print_iteration_information
#'
#' @return Noting
print_initial_header <- function(efficiency_criteria, padding = 10, width = 80) {
  cat("\n")
  cat(rule(width = width), "\n")
  cat(
    str_c(
      str_pad("Iteration", padding, "left", " "),
      if ("a-error" %in% efficiency_criteria) {
        col_green(str_pad("A-error", padding, "left"))
      } else {
        str_pad("A-error", padding, "left", " ")
      },
      if ("c-error" %in% efficiency_criteria) {
        col_green(str_pad("C-error", padding, "left"))
      } else {
        str_pad("C-error", padding, "left", " ")
      },
      if ("d-error" %in% efficiency_criteria) {
        col_green(str_pad("D-error", padding, "left"))
      } else {
        str_pad("D-error", padding, "left", " ")
      },
      if ("s-error" %in% efficiency_criteria) {
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
#' The function prints a string of efficiency criteria to the console and
#' highlights the color of the considered efficiency criteria. Effectively it
#' is a wrapper around multiple calls to \code{\link{cat}}.
#'
#' @inheritParams print_iteration_information
#'
#' @return A character string.
print_efficiency_criteria <- function(
  iter,
  values,
  criteria,
  digits = 4,
  padding = 10,
  efficiency_criteria
) {
  printable_string <- lapply(seq_along(values), function(i) {
    if (is.na(values[[i]])) {
      string <- "N/A"
    } else {
      string <- as.character(round(values[[i]], digits))
    }

    if (criteria[[i]] %in% efficiency_criteria) {
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
