#' Runs checks on V
#'
#' @param V a list of utility functions
#'
#' @noRd
check_v <- function(V) {
  txt <- "Checking specified utility functions"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  if (!is.list(V)) stop("'V' has to be a named list of utility functions. Please see the manual.")
  spinner$spin()

  if (length(V) < 2) stop("'V' has to contain at least 2 utility functions. Please see the manual.")
  spinner$spin()

  if (!all(do.call(c, lapply(V, is_balanced, "[", "]")))) {
    stop("There is an uneven number of opening and closing brackets in the utility functions. Make sure that all opening brackets only have one closing bracket.")
  }
  spinner$spin()

  if (!all(do.call(c, lapply(V, is_balanced, "(", ")")))) {
    stop("There is an uneven number of opening and closing parentheses in the utility functions. Make sure that all opening parenthesis only have one closing parenthesis.")
  }
  spinner$finish()

  cli_alert_success(txt)
}

#' Runs checks on opts
#'
#' @param opts A list of design options
#'
#' @noRd
check_opts <- function(opts) {
  txt <- "Checking 'opts'-list and setting defaults"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  required <- c("optimization_algorithm", "efficiency_criteria", "model", "tasks")
  test_required <- lapply(required, function(x) {
    spinner$spin()
    is.null(opts[[x]])
  })

  if (any(do.call(c, test_required))) {
    stop(paste0("The list elements: ", paste0(required[-length(required)], collapse = ", "), " and tasks must be specified in the list of options 'opts'."))
  }
  spinner$spin()

  if (opts$cores >= parallel::detectCores()) {
    stop("The number of specified cores in 'opts$cores' (", opts$cores, ") is greather than or equal to the number of available cores (", parallel::detectCores(), "). We recommend to use **at most** one less than the number of available logical cores.")
  }
  spinner$spin()

  opts <- validate_opts(opts)
  spinner$finish()

  cli_alert_success(txt)
}

#' Run checks on the candidate set
#'
#' @param candidate_set A candidate set
#'
#' @noRd
check_candidate_set <- function(candidate_set) {
  txt <- "Checking the candidate set"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  if (is.null(candidate_set)) {
    cli_alert_info("No candidate set supplied. The design will use the full factorial subject to supplied constraints.")
  } else {
    if (!(is.matrix(candidate_set) | is.data.frame(candidate_set))) {
      stop("The supplied 'candidate_set' is neither a matrix nor a data.frame. If you did not intend to supply a candidate set, ommit or set this optsion to NULL.")
    }
  }
  spinner$finish()

  cli_alert_success(txt)
}
