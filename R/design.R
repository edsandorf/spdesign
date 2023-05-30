#' Generate the experimental design
#'
#' Generates the experimental design is the main function of the package. It
#' wraps around all other functions and calls them in the correct order to
#' generate a design based on the specified utility functions, options and
#' candidate set.
#'
#' @param utility A named list of utility functions
#' @param tasks An integer giving the number of tasks (rows) in the final design
#' @param blocks An integer giving the number of blocks to block the design
#' into. The default value is 1. 2022-07-07: Blocking functionality is not
#' implemented.
#' @param model A character string indicating the model to optimize the design
#' for. Currently the only model programmed is the 'mnl' model and this is also
#' set as the default.
#' @param efficiency_criteria A character string giving the efficiency criteria
#' to optimize for. One of 'a-error', 'c-error', 'd-error' or 's-error'. No
#' default is set and argument must be specified. Optimizing for multiple
#' criteria is not yet implemented and will result in an error.
#' @param algorithm A character string giving the optimization algorithm to use.
#' No default is set and the argument must be specified to be one of 'federov'
#' or 'random'. The 'rsc' algorithm is not implemented yet.
#' @param draws The type of draws to use with Bayesian priors (and rpl, but
#' this model is not yet implemented). No default is set and must be specified
#' as one of "pseudo-random", "mlhs", "standard-halton", "scrambled-halton",
#' "standard-sobol","scrambled-sobol".
#' @param R An integer giving the number of draws to use. The default is 100.
#' @param didx A character string giving the name of the parameter in the
#' denominator. Must be specified when optimizing for 'c-error'
#' @param candidate_set A matrix or data frame in the "wide" format containing
#' all permitted combinations of attributes. The default is NULL. If no
#' candidate set is provided, then the full factorial subject to specified
#' restrictions will be used.
#' @param restrictions A list of restrictions. Often this list will be pulled
#' directly from the list of options or it is a modified list of restrictions
#' following calls to _dummy or _effects coding.
#' @param level_balance A placeholder boolean for whether to impose level
#' balance in the design
#' @param control A list of control options
#'
#' @return A design list of valid designs
#'
#' @export
generate_design <- function(utility,
                            tasks,
                            blocks = 1,
                            model = "mnl",
                            efficiency_criteria = c("a-error", "c-error",
                                                    "d-error", "s-error"),
                            algorithm = c("federov", "random"),
                            draws = c("pseudo-random", "mlhs", "standard-halton",
                                      "scrambled-halton", "standard-sobol",
                                      "scrambled-sobol"),
                            R = 100,
                            didx = NULL,
                            candidate_set = NULL,
                            restrictions = NULL,
                            level_balance = FALSE,
                            control = list(
                              cores = 1,
                              max_iter = 10000,
                              efficiency_threshold = 0.1,
                              sample_with_replacement = FALSE
                            )) {

  # Match and check model arguments ----
  cli_h2("Checking function arguments")
  # Utility NBNB! Will fail to catch if utility is a data.frame!!
  stopifnot(is.list(utility))
  stopifnot(length(utility) > 1)
  stopifnot(all(do.call(c, lapply(utility, is_balanced, "[", "]"))))
  stopifnot(all(do.call(c, lapply(utility, is_balanced, "(", ")"))))
  stopifnot(all_priors_and_levels_specified(utility))
  # This is a bit weird with not any duplicates
  stopifnot(!any_duplicates(utility))
  stopifnot(!too_small(utility, tasks))

  # Set the default for control and replace the specified values in control
  default_control <- list(
    cores = 1,
    max_iter = 10000,
    efficiency_threshold = 0.1,
    sample_with_replacement = FALSE
  )

  control <- utils::modifyList(default_control, control)

  # Blocks and tasks
  if (blocks > tasks) {
    stop("You cannot have more blocks than tasks")
  }

  if (tasks %% blocks != 0)  {
    stop("You cannot have uneven number of tasks per block")
  }

  # Model
  model <- match.arg(model)

  # Efficiency criteria
  efficiency_criteria <- match.arg(efficiency_criteria, several.ok = TRUE)

  if (length(efficiency_criteria) > 1) {
    stop("Optimizing over multiple criteria is not implemented")
  }

  if (is.null(didx) && efficiency_criteria == "c-error") {
    stop("The denominator index 'didx' must be specified for c-error")
  }

  # Algorithm
  algorithm <- match.arg(algorithm)

  # Draws
  draws <- match.arg(draws)

  # Consider a core-check if relevant at a later point.
  if (control$cores > 1) {
    warning("Multicore is not implemented yet. Design will be optimized using a single core.")
    control$cores <- 1
  }

  # Parse utility ----
  cli_h2("Parsing utility")
  utility_parsed <- parse_utility(utility, tasks)

  # Candidate set ----
  cli_h2("Checking the candidate set and applying restrictions")

  # If no candidate set is supplied generate full factorial if not run simple
  # checks
  if (is.null(candidate_set)) {
    cli_alert_info("No candidate set supplied. The design will use the full factorial subject to supplied constraints.")

    candidate_set <- full_factorial(get_attribute_levels(utility_parsed))

    cli_alert_success("Full factorial created")

  } else {
    # Check that it is a matrix
    stopifnot((is.matrix(candidate_set) || is.data.frame(candidate_set)))

    # Check candidate set
    if (!all(names(candidate_set) %in% names(get_attribute_levels(utility_parsed)))) {
      stop(
        "Not all attributes specified in the utility functions are specified in
        the candidate set. Make sure that all attributes are specified and that
        the names used in the utility functions correspond to the column names
        of the supplied candidate set. Note the candidate set must be supplied
        in 'wide' format."
      )
    }

    # Print information about attribute levels.
    cli_alert_info(
      "The attribute levels are determined based on the supplied candidate set
      and not the levels specified in the utility functions because levels not
      present in the candidate set cannot be part of the design. Should this be
      an error?"
    )

    utility_parsed[["attribute_levels"]] <- lapply(
      as.list(
        as.data.frame(
          candidate_set
        )
      ),
      unique
    )
  }

  # Apply the restrictions to the candidate set
  candidate_set <- apply_restrictions(candidate_set, restrictions)
  candidate_set <- as.matrix(candidate_set)

  cli_alert_success("All restrictions successfully applied")

  # Prepare the list of priors ----
  cli_h2("Preparing the list of priors")

  priors <- prepare_priors(utility, utility_parsed, draws, R)

  cli_alert_success("Priors prepared successfully")

  # Set up parallel ----
  if (control$cores > 1) {
    cli_h2("Preparing multicore estimation")

    stop("Multicore not implmented")

    future::plan(
      future::multicore(workers = control$cores)
    )

    cli_alert_success("Multicore estimation prepared successfully")

  }

  # Evaluate designs ----
  cli_h1("Evaluating designs")

  # Create the design object and make sure that the current status of the object
  # is returned if the program is ended prematurely from clicking "stop"
  design_object <- list()
  design_object[["utility"]] <- get_utility_clean(utility_parsed)
  design_object[["priors"]] <- get_prior_values(utility_parsed)
  design_object[["time"]] <- list(
    time_start = Sys.time()
  )

  class(design_object) <- "spdesign"


  # Make sure that the best design candidate is always return if the loop is
  # stopped prematurely Can on.exit have a function?
  on.exit(
    return(design_object),
    add = TRUE
  )

  # Optmization function!!!!!!!
  design_object <- switch(
    algorithm,
    random = random(design_object,
                    model,
                    efficiency_criteria,
                    utility_parsed,
                    priors,
                    didx,
                    candidate_set,
                    tasks,
                    control),
    federov = federov(design_object,
                      model,
                      efficiency_criteria,
                      utility_parsed,
                      priors,
                      didx,
                      candidate_set,
                      tasks,
                      control),
    rsc = rsc(design_object,
              model,
              efficiency_criteria,
              utility_parsed,
              priors,
              didx,
              candidate_set,
              tasks,
              control)
  )

  design_object[["time"]][["time_end"]] <- Sys.time()

  # Print final closing messages
  cat("\n\n")
  cli_h1("Cleaning up design environment")
  cat("Time spent searching for designs: ", Sys.time() - design_object$time$time_start, "\n")

  return(
    design_object
  )
}
