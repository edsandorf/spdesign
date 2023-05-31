#' Tests whether a utility function is balanced
#'
#' Tests whether there is an equal number of opening and closing brackets in
#' the utility functions.
#'
#' @param string A character string
#' @param open An opening bracket ( [ or <
#' @param close A closing bracket ) ] or >
#'
#' @return A boolean equal to `TRUE` if the utility expression is balanced
is_balanced <- function(string, open, close) {
  opening <- c("(", "[", "<", "{")
  closing <- c(")", "]", ">", "}")

  if (!(open %in% opening)) {
    stop(
      "The function only supports the following opening brackets:
       '(', '[', '<' ann '{'"
    )
  }

  if (!(close %in% closing)) {
    stop(
      "The function only supports the following closing brackets:
       ')', ']', '>' and '}"
    )
  }

  if (grep(paste0("\\", open), opening) != grep(paste0("\\", close), closing)) {
    warning(
      "The opening and closing brackets do not match. This will very likely
      result in an error and the function evaluating to FALSE, but left in
      because this unintended consequence might be useful.\n"
    )
  }

  opened <- str_count(string, paste0("\\", open))
  closed <- str_count(string, paste0("\\", close))

  if (opened != closed) {
    return(FALSE)

  } else {
    return(TRUE)

  }
}

#' Tests whether the utility expression contains Bayesian priors
#'
#' This is particularly useful for flow-control
#'
#' @param string A string or list of strings
#'
#' @return A boolean equal to `TRUE` if we have Bayesian priors
has_bayesian_prior <- function(string) {
  return(
    any(str_detect(string, "(normal_p|lognormal_p|uniform_p|triangular_p)\\("))
  )
}

#' Tests whether the utility expression contains random parameters
#'
#' This is particularly useful for flow-control
#'
#' @param string A string or list of strings
#'
#' @return A boolean equal to `TRUE` if we have random parameters
has_random_parameter <- function(string) {
  return(
    any(str_detect(string, "(normal|lognormal|uniform|triangular)\\("))
  )
}

#' Check whether all priors and attributes have specified levels
#'
#' @param x A list of utility expressions
#'
#' @return A boolean equal to `TRUE` if all are specified and `FALSE` if not
all_priors_and_levels_specified <- function(x) {
  # Extract all named values from the utility expression returned as a list
  named_values <- extract_named_values(x)

  # Extract all unique priors and attributes
  all_values <- unique(remove_whitespace(extract_all_names(x, simplify = TRUE)))

  if (!all(all_values %in% names(named_values))) {
    idx <- which((all_values %in% names(named_values)) == FALSE)
    missing_values <- paste0("'", all_values[idx], "'", collapse = " ")

    cli_alert_danger(
      paste0(
        missing_values,
        " does not have a specified prior or levels. Please make sure that all
        elements of the utility functions have been specified with a prior or
        levels once."
      )
    )

    return(FALSE)

  } else {
    return(TRUE)

  }
}

#' Check whether any priors or attributes are specified with a value more than
#' once
#'
#' @inheritParams all_priors_and_levels_specified
#'
#' @return A boolean equal to `TRUE` if specified more than once.
any_duplicates <- function(x) {
  # Extract all named values from the utility expression returned as a list
  named_values <- extract_named_values(x)

  idx <- duplicated(names(named_values))

  if (any(idx)) {
    duplicates <- paste0("'", names(named_values)[idx], "'", collapse = " ")

    cli_alert_danger(
      paste0(
        duplicates,
        " are specified with priors or levels more than once. Only the first
        occurance of the value is used. If you intended to use different levels
        for different attributes in each utility function, please specify
        alternative specific attributes.\n "
      )
    )

    return(TRUE)

  } else {
    return(FALSE)

  }
}

#' Check if the design is too small
#'
#' Uses the formula of T * (J - 1) to check if the design is large enough to
#' identify the parameters of the utility function.
#'
#' @inheritParams all_priors_and_levels_specified
#' @param rows The number of rows in the design
#'
#' @return A boolean equal to `TRUE` if the design is too small
too_small <- function(x, rows) {
  if ((rows * (length(x) - 1)) < length(priors(x))) {
    cli_alert_danger(
      "The design is too small to identify all parameters. You need to create
      a larger design."
    )

    return(TRUE)

  } else {
    return(FALSE)

  }
}

#' Check whether we can achieve attribute level balance
#'
#' @inheritParams too_small
#'
#' @return A boolean equal to `TRUE` if attribute level balance can be achieved
#' and `FALSE` otherwise
attribute_level_balance <- function(x, rows) {
  # Test using modulus mathematics
  if (any(do.call(c, lapply(attribute_levels(x), function(k) rows %% length(k))) != 0)) {
    cli_alert_warning(
      "The number of levels specified for one or more attributes are not a
      multiple of the number of rows in the design. Attribute level
      balance is not possible."
    )

    return(FALSE)

  } else {
    return(TRUE)

  }
}
