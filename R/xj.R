#' Define base x_j
#'
#' Defines the base of the x_j list using the parsed utility expression,
#' design_candidate and the base model matrix
#'
#' @inheritParams federov
#' @param design_candidate The current design candidate under consideration
#'
#' @return A base list x_j with model matrices the lenght of J
define_base_x_j <- function(utility, design_candidate) {
  x_j <- lapply(utility_formula(utility), function(x) {
    return(
      model.matrix(x, design_candidate)
    )
  })

  # Subset to exclude base dummylevel
  x_j <- lapply(x_j, function(x) {
    x[, colnames(x) %in% extract_attribute_names(update_utility(utility)), drop = FALSE]
  })

  return(x_j)
}

#' Define x_j
#'
#' Define x_j to use for the analytic derivatives of the variance-covariance
#' matrix. x_j is derived based on the provided utility functions and design
#' candidate using base model.matrix to automatically handle alternative
#' specific attributes and interaction terms
#'
#' We can extract the attribute names for each utility function to allow us
#' to place the correct restrictions on the design candidate. Specifically, we
#' restrict all levels of unavailable attributes to zero for alternatives where
#' they do not feature. This is to ensure that we do not give weight when
#' deriving the variance-covariance matrix using \code{\link{derive_vcov}}.
#' Furthermore, the Xs are "sorted" using the order of the candidate set, which
#' ensures that when we calculate the sum of the probabilities times X, the
#' correct columns are added together. See \code{\link{derive_vcov}}.
#'
#' @inheritParams define_base_x_j
#'
#' @return The list x_j
define_x_j <- function(utility, design_candidate) {
  # Determine x_j (by using model.matrix we automatically handle interactions)
  x_j <- define_base_x_j(utility, design_candidate)

  # We need to remove the alternative designation from x_j given long-format
  # model setup
  x_j <- lapply(x_j, function(x) {
    colnames(x) <- str_replace_all(colnames(x), "^.*?_", "")
    return(x)
  })

  # Create new matrices to ensure 0 columns for alternative specific attributes
  x_j_unique_colnames <- unique(do.call(c, lapply(x_j, colnames)))
  model_matrix <- matrix(0,
                         nrow = nrow(design_candidate),
                         ncol = length(x_j_unique_colnames),
                         dimnames = list(
                           NULL,
                           x_j_unique_colnames
                         ))

  # Replace the x_j which ensures that each list element (matrix) is of equal
  # size and handles interactions and alternative specific attributes
  x_j <- lapply(x_j, function(x) {
    model_matrix[, colnames(x)] <- x
    return(
      model_matrix
    )
  })

  # Return x_j
  return(
    x_j
  )
}


