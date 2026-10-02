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

#' Check whether the utility function contains dummy coded variables
#'
#' We are splitting on all separators first before detecting whether we have
#' dummy coded attributes to allow for people reusing the _dummy name for the
#' attribute.
#'
#' @inheritParams has_bayesian_prior
#'
#' @return A boolean equal to `TRUE` if the utility function contains dummy
#' coded attributes and `FALSE` otherwise
contains_dummies <- function(string) {
  return(
    any(str_detect(
      unlist(str_split(string, "(\\+|\\-|\\*|\\/)")),
      "b_.*_dummy"
    ))
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
        occurrence of the value is used. If you intended to use different levels
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
  if (
    any(
      do.call(c, lapply(attribute_levels(x), function(k) rows %% length(k))) !=
        0
    )
  ) {
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

#' Measure how far a design candidate is from the level occurrence constraints
#'
#' For each attribute level, count how often it occurs in the design candidate
#' and find the distance to the nearest allowed number of occurrences. Levels
#' that do not occur in the design candidate are counted as zero occurrences.
#'
#' @param x A design candidate with one column per attribute
#' @param ranges Level occurrences as returned by \code{\link{occurrences}}.
#' Pass these in when calling repeatedly to avoid parsing the utility functions
#' each time
#' @param lvls Attribute levels as returned by
#' \code{\link{expand_attribute_levels}}
#' @inheritParams occurrences
#' @inheritParams generate_design
#'
#' @return The total distance summed over all attribute levels. A value of 0
#' means that the design candidate satisfies all level occurrence constraints.
lvl_violation <- function(
  utility,
  x,
  rows,
  ranges = occurrences(utility, rows),
  lvls = expand_attribute_levels(utility)
) {
  violation <- vapply(seq_along(ranges), function(i) {
    counts <- table(factor(x[, i, drop = TRUE], levels = lvls[[i]]))

    sum(
      mapply(function(n, allowed) min(abs(n - allowed)), counts, ranges[[i]])
    )
  }, numeric(1))

  return(
    sum(violation)
  )
}

#' Find attributes with level occurrences and levels that are not listed
#'
#' A supplied candidate set may contain attribute levels that are not listed in
#' the utility functions. This is only a problem for attributes with level
#' occurrences specified, because occurrences can only be counted for listed
#' levels.
#'
#' @param candidate_set A candidate set in wide format
#' @inheritParams lvl_violation
#'
#' @return A character vector with the names of the attributes with level
#' occurrences specified, where the candidate set contains levels that are not
#' listed in the utility functions. Empty if there are none.
unlisted_levels <- function(utility, candidate_set, rows) {
  ranges <- occurrences(utility, rows)
  lvls <- expand_attribute_levels(utility)
  unrestricted <- c(0, seq_len(rows))

  restricted <- vapply(ranges, function(r) {
    !all(vapply(r, setequal, logical(1), unrestricted))
  }, logical(1))

  unlisted <- vapply(names(ranges)[restricted], function(a) {
    any(!candidate_set[, a, drop = TRUE] %in% lvls[[a]])
  }, logical(1))

  return(
    names(which(unlisted))
  )
}

#' Test whether level occurrences are specified in the utility functions
#'
#' @inheritParams generate_design
#'
#' @return A boolean equal to TRUE if level occurrences are specified
#' and FALSE otherwise
#'
level_occurrences_specified <- function(utility) {
  specified_values <- extract_specified(utility, simplify = TRUE)
  idx <- str_detect(specified_values, "(?<=\\])\\(.*?\\)")

  return(
    any(idx)
  )
}

#' Find dummy-coded attributes that are not correctly specified
#'
#' Dummy-coded attributes must have the levels 1, 2, ..., K, where 1 is the
#' base level, and exactly K - 1 priors, one for each level except the base
#' level. Only the number of levels matters for the design, and using
#' 1, 2, ..., K ensures that the expanded dummy-coded attributes and priors are
#' named and ordered consistently.
#'
#' @inheritParams attribute_levels
#'
#' @return A character vector with the names of dummy-coded attributes that are
#' not correctly specified. Empty if there are none.
invalid_dummy_coding <- function(x) {
  # A component is dummy-coded if its parameter is, as in contains_dummies()
  components <- unlist(str_split(unlist(x), "\\+"))
  components <- components[str_detect(components, "\\bb_\\w*_dummy\\[")]

  invalid <- vapply(
    components,
    function(component) {
      lvls <- unlist(attribute_levels(component))

      !identical(as.numeric(lvls), as.numeric(seq_along(lvls))) ||
        length(priors(component)) != length(lvls) - 1
    },
    logical(1),
    USE.NAMES = FALSE
  )

  return(
    unique(extract_attribute_names(components[invalid], TRUE))
  )
}
