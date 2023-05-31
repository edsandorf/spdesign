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
set_default_options <- function(opts_input) {
  txt <- "Setting default design options"
  spinner <- make_spinner("line", template = paste0("{spin} ", txt))

  # Define the complete list of design options with default values
  opts <- list(
    algorithm = list(
      alg = "rsc",
      swap = 1,
      swap_on_imp = 20,
      reset = 10000
    ),
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
  idx <- !(names(opts$algorithm) %in% names(opts_input$algorithm))
  opts_input$algorithm <- c(opts_input$algorithm, opts$algorithm[idx])
  opts[names(opts_input)] <- opts_input
  spinner$spin()

  # Set the optimization algorihtm to lower
  opts$algorithm$alg <- tolower(opts$algorithm$alg)
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

#' Sets the default level occurrence in an attribute level balanced design
#'
#' The function sets the default level occurrence of an attribute when a design
#' is restricted to be attribute level balanced. If the design cannot be
#' attribute level balanced, then the restriction will be relaxed for each
#' attribute failing to meet this criteria. Specifically, the code will impose
#' a minimum range of how often an attribute level can occur. This will secure
#' that the design is near attribute level balanced. In this case a warning is
#' issued.
#'
#'
#' @param n_lvls An integer giving the number of levels for the considered
#' attribute
#' @inheritParams occurrences
#'
#' @return A named list of lists where the top level gives the attribute and the
#' lower level gives the times or range each attribute level should occur in
#' the design
set_default_level_occurrence <- function(n_lvls, rows) {
  # Check if attribute level balance can be achieved
  if ((rows %% n_lvls) == 0) {
    lvl <- rows / n_lvls

  } else {
    minimum <- floor(rows / n_lvls)
    lvl <- minimum:(minimum + 1)

  }

  lvls <- lapply(seq_len(n_lvls), function(n) lvl)
  names(lvls) <- paste0("lvl", seq_along(lvls))
  lvls
}
