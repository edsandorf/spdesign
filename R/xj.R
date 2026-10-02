#' Define x_j
#'
#' Defines x_j, the list of attribute matrices with one matrix per alternative.
#' Each matrix has one column per term in the utility function of that
#' alternative: attributes, expanded dummy-coded attributes and, if requested,
#' interaction terms. The columns are named after the terms and keep the order
#' of \code{\link{model.matrix}}.
#'
#' @inheritParams federov
#' @param design_candidate The current design candidate under consideration
#' @param terms A list of terms and their parameters returned by
#' \code{\link{pair_param_terms}}. The default pairs the terms of the updated
#' utility functions.
#' @param interactions If TRUE, include interaction terms. The default is TRUE.
#'
#' @return A list of model matrices, one per alternative
define_x_j <- function(
  utility,
  design_candidate,
  terms = pair_param_terms(update_utility(utility)),
  interactions = TRUE
) {
  x_j <- mapply(
    function(formula, t) {
      x <- model.matrix(formula, design_candidate)
      colnames(x) <- remove_whitespace(colnames(x))

      unmatched <- setdiff(names(t), colnames(x))

      if (length(unmatched) > 0) {
        stop(
          "Could not match the following terms in the utility functions: ",
          paste(unmatched, collapse = ", "),
          ". Check that each prior is written before its attribute, e.g. ",
          "'b_x1[0.1] * x1[1:3]'."
        )
      }

      x[, colnames(x) %in% names(t), drop = FALSE]
    },
    utility_formula(utility),
    terms,
    SIMPLIFY = FALSE
  )

  if (!interactions) {
    x_j <- lapply(x_j, function(x) {
      x[, !str_detect(colnames(x), "^I\\("), drop = FALSE]
    })
  }

  return(
    x_j
  )
}

#' Align x_j with the priors
#'
#' Renames the columns of x_j from terms to parameters, so that a generic
#' parameter is a single column even if its attribute has a different name in
#' each alternative. Parameters that do not enter the utility function of an
#' alternative are zero for that alternative. The columns are ordered as the
#' priors, which ensures that the variance-covariance matrix derived by
#' \code{\link{derive_vcov}} matches the priors.
#'
#' @param x_j A list of model matrices returned by \code{\link{define_x_j}}
#' @param terms A list of terms and their parameters returned by
#' \code{\link{pair_param_terms}}
#' @param names_priors The names of the priors
#'
#' @return The list x_j with one column per prior in the order of the priors
align_x_j <- function(x_j, terms, names_priors) {
  unmatched <- setdiff(unlist(terms), names_priors)

  if (length(unmatched) > 0) {
    stop(
      "The following parameters do not have a prior: ",
      paste(unmatched, collapse = ", ")
    )
  }

  model_matrix <- matrix(
    0,
    nrow = nrow(x_j[[1]]),
    ncol = length(names_priors),
    dimnames = list(NULL, names_priors)
  )

  return(
    mapply(
      function(x, t) {
        model_matrix[, t[colnames(x)]] <- x
        return(model_matrix)
      },
      x_j,
      terms,
      SIMPLIFY = FALSE
    )
  )
}
