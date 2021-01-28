#' Runs checks on utility
#'
#' \code{\link{generate_design}} runs three sets of checks, all detailed here,
#' in the order of appearance of the function arguments. The checks on utility
#' is to ensure that the user supplied list of utility functions are valid.
#'
#' As far as possible, all checks on utility should be included in this
#' function. This is to ensure that the code fails quickly to give the user the
#' ability to correct any syntax mistakes, to reduce code-duplication and make
#' it easier to maintain the code base.
#'
#' @param utility a list of utility functions
#'
#' @return Nothing
check_v <- function(utility) {
  txt <- "Checking specified utility functions"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  if (!is.list(utility)) stop(
    "'utility' has to be a named list of utility functions."
  )
  spinner$spin()

  if (length(utility) < 2) stop(
    "'utility' has to contain at least 2 utility functions."
  )
  spinner$spin()

  if (!all(do.call(c, lapply(utility, is_balanced, "[", "]")))) {
    stop(
      "There is an uneven number of opening and closing brackets in the utility
      functions. Make sure that all opening brackets only have one closing
      bracket."
    )
  }
  spinner$spin()

  if (!all(do.call(c, lapply(utility, is_balanced, "(", ")")))) {
    stop(
      "There is an uneven number of opening and closing parentheses in the
      utility functions. Make sure that all opening parenthesis only have one
      closing parenthesis."
    )
  }
  spinner$finish()

  cli_alert_success(txt)
}

#' Runs checks on opts
#'
#' \code{\link{generate_design}} runs three sets of checks, all detailed here,
#' in the order of appearance of the function arguments. The checks on 'opts'
#' is to ensure that the user supplied list of options are valid.
#'
#' As far as possible, all checks on 'opts' should be included in this function.
#' This is to ensure that the code fails quickly to give the user the ability to
#' correctly specify missing options, to reduce code-duplication and make it
#' easier to maintain the code base.
#'
#' After 'opts' has been checked, inside \code{\link{generate_design}} a call is
#' made to \code{\link{set_defaults}} to set missing optional entries to their
#' default values.
#'
#' @param opts A list of design options
#'
#' @return Nothing
check_opts <- function(opts) {
  txt <- "Checking 'opts'-list and setting defaults"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  required <- c("optimization_algorithm", "efficiency_criteria", "model",
                "tasks")
  test_required <- lapply(required, function(x) {
    spinner$spin()
    is.null(opts[[x]])
  })

  if (any(do.call(c, test_required))) {
    stop(
      paste0(
        "The list elements: ",
        paste0(required[-length(required)],
               collapse = ", "),
        " and tasks must be specified in the list of options 'opts'."
      )
    )
  }
  spinner$spin()

  optimization_algorithms <- c("random", "federov", "rsc")
  if (!(tolower(opts$optimization_algorithm) %in% optimization_algorithms)) {
    stop(
      paste0(
        "The optimization algorithm has to be on of: ",
        paste(optimization_algorithms, collapse = ", ")
      )
    )
  }
  spinner$spin()

  error_measures <- c("a-error", "c-error", "d-error", "s-error")
  if (!(tolower(opts$efficiency_criteria) %in% error_measures)) {
    stop(
      paste0(
        "The optimization algorithm has to be on of: ",
        paste(error_measures, collapse = ", ")
      )
    )
  }
  spinner$spin()

  if (length(opts$efficiency_criteria) > 1) {
    stop(
      "Optimizing for multiple efficiency criteria is not yet implemented."
    )
  }
  spinner$spin()

  if (is.null(opts$didx) && opts$efficiency_criteria == "c-error") {
    stop(
      "If you are optimizing for c-efficiency then you must specify the
        denominator index 'didx'"
    )
  }
  spinner$spin()

  if (!is.null(opts$draws_type)) {
    allowed_types <- c("pseudo-random", "mlhs", "standard-halton",
                       "scrambled-halton", "standard-sobol", "scrambled-sobol")
    if (!(tolower(opts$draws_type) %in% allowed_types)) {
      stop(
        paste0(
          "Unknown type of draws specified. Type of draws allowed: ",
          paste(allowed_types, collapse = ", ")
        )
      )
    }
  }
  spinner$spin()

  if (opts$cores >= parallel::detectCores()) {
    stop(
      "The number of specified cores in 'opts$cores' (",
      opts$cores,
      ") is greather than or equal to the number of available cores (",
      parallel::detectCores(),
      "). We recommend to use **at most** one less than the number of available
      logical cores."
    )
  }
  spinner$finish()

  cli_alert_success(txt)

  # Information prints after spinner completes to avoid awkward printing
  if (is.null(opts$didx) && opts$efficiency_criteria != "c-error") {
    cli_alert_info(
      "The index for the denominator is not specified and the c-efficiency
      measure will not be reported."
    )
  }

}

#' Run checks on the candidate set
#'
#' \code{\link{generate_design}} runs three sets of checks, all detailed here,
#' in the order of appearance of the function arguments. The checks on the
#' 'candidate_set' is to ensure that the user supplied candidate set is valid.
#' This mostly comes down to checking its dimensions against the number of
#' attributes and levels in the utility functions.
#'
#' If no candidate set is supplied, \code{\link{generate_design}} will make a
#' call to \code{\link{generate_full_factorial}} to use the full factorial
#' subject to specified constraints as the candidate set.
#'
#' @param candidate_set A candidate set
#'
#' @return Nothing
check_candidate_set <- function(candidate_set) {
  txt <- "Checking the candidate set"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  if (!is.null(candidate_set)) {
    if (!(is.matrix(candidate_set) | is.data.frame(candidate_set))) {
      stop(
        "The supplied 'candidate_set' is neither a matrix nor a data.frame.
        If you did not intend to supply a candidate set, ommit or set this
        options to NULL."
      )
    }
  }
  spinner$finish()

  cli_alert_success(txt)

  # Information prints after spinner completes to avoid awkward printing
  if (is.null(candidate_set)) {
    cli_alert_info(
      "No candidate set supplied. The design will use the full factorial
      subject to supplied constraints."
    )
  }
}
