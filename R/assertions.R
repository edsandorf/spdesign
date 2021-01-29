#' Tests whether a utility function is balanced
#'
#' Tests whether there is an equal number of opening and closing brackets in
#' the utility functions.
#'
#' @param string A character string
#' @param open An opening bracket ( [ or <
#' @param clos A closing bracket ) ] or >
#'
#' @noRd
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
    FALSE
  } else {
    TRUE
  }
}


#' Tests whether the utility expression contains Bayesian priors
#'
#' This is useful to create a boolean for flow-control inside
#' \code{\link{generate_design}}
#'
#' @param string A string or list of strings
#'
#' @return A boolean
has_bayesian_prior <- function(string) {
  any(str_detect(string, "(normal_p|lognormal_p|uniform_p|triangular_p)\\("))
}

#' Tests whether the utility expression contains random parameters
#'
#' This is useful to create a boolean for flow-control inside
#' \code{\link{generate_design}}
#'
#' @param string A string or list of strings
#'
#' @return A boolean
has_random_parameter <- function(string) {
  any(str_detect(string, "(normal|lognormal|uniform|triangular)\\("))
}
