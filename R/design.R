#' Create the experimental design
#'
#' Design takes the utility functions and design options as inputs and generates
#' candidate designs. Design cannot be run without first validating the utility
#' functions and design options. Running LINK TO VALIDATE will create a validated
#' environment, which is the context for evaluating the designs.
#'
#' @param U A list of utility functions
#' @param design_opt A list of design options
#' @param candidate_set A matrix or data frame in the "wide" format containing
#' all permitted combinations of attributes. The default is NULL. If no candidate
#' set is provided, then the full factorial subject to specified restrictions
#' will be used.
#'
#' @return A design object of class designer.design
#'
#' @export
design <- function(U, design_opt, candidate_set = NULL) {
  # Run checks on the inputs
  # - If no design matrix is supplied, use the full factorial
  # - If paralell, set up doParallel()

  message(red$bold(symbol$cross), " Failed")

  # Check the candidate set. If no candidate set is supplied (default), then use
  # the full factorial subject to the specified constraints if any
  if (!is.null(candidate_set)) {
    if (!(is.matrix(candidate_set) | is.data.frame(candidate_set))) {
      stop("The supplied 'candidate_set' is neither a matrix nor a data.frame. Plese ensure that the candidate set is specified correctly. If you intended to use the full factorial subject to specified constraints, please leave argument empty or set equal to NULL (default).\n")
    }
    # Run a name check on the candidate set to ensure that names can be inferred

    message(green$bold(symbol$tick), " All checks on the supplied candidate set were successful. The design will be created using the supplied candidate set. \n")

  } else {
    candidate_set <- full_factorial()
    # Apply relevant restrictions

    message(blue$bold(symbol$info), " No candidate set was supplied. The design will be created using the full factorial subject to specified constraints (if any). The complete candidate set is returned along with the design. \n")

  }

  # Check if the number of cores is appropriate
  if (design_opt$cores > 1 & design_opt$cores >= parallel::detectCores()) {
      stop("The number of cores specified in 'design_opt' (", design_opt$cores, ") is equal to or greater than the number of available logical cores (", parallel::detectCores(), "). It is recommended to use at most one less than the number of logical cores available.")
  }



  # If all checks pass, then we need to build the vector of parameters and
  # and model matrix.

  # Evaluate the function in a context
}
