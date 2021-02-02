#' Validate design opt
#'
#' The function takes the list of design options and adds default values where
#' none are specified. This function is exported, but is not intended to be
#' called by the user of the package. The function is called from within
#' \code{\link{generate_design}} to populate the list with sensible defaults
#'
#' @param opts_input A list of user supplied design options
#'
#' @return A list of design options populated by sensible default values
#'
#' @export
set_defaults <- function(opts_input) {
  txt <- "Setting default design options"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  # Define the complete list of design options with default values
  opts <- list(
    optimization_algorithm = "random", # "random", "federov", "rsc"
    efficiency_criteria = "d-efficiency",
    model = "mnl",
    blocks = 1,
    tasks = 6,
    cores = 1,
    didx = NULL,
    draws_priors = 100,
    draws_params = 100,
    draws_type = "scrambled-sobol",
    max_iter = 10,
    eff_threshold = 0.1,
    sample_with_replacement = FALSE,
    restrictions = NULL,
    level_balance = FALSE
  )

  # Replace the values in the default list with the user supplied list
  opts[names(opts_input)] <- opts_input
  spinner$spin()

  # Set the optimization algorihtm to lower
  opts$optimization_algorithm <- tolower(opts$optimization_algorithm)
  spinner$spin()

  # Set the efficiency criteria to lower
  opts$efficiency_criteria <- tolower(opts$efficiency_criteria)

  # Set the type of draws to lower
  opts$draws_type <- tolower(opts$draws_type)

  spinner$finish()
  cli_alert_success(txt)
  # Return
  opts
}
