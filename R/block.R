#' Block the design
#'
#' The function will take an object of class 'spdesign' and add a blocking
#' column to the design matrix. The function will use random permutations of
#' the blocking column to find the column that minimizes correlation between
#' the blocking column and the design columns. Specifically the target for the
#' minimization procedure is the mean squared correlation.
#'
#' The function uses a random permutation so every time you run the function
#' you will get a slightly different blocking column. You can set a seed prior
#' to calling the function to always return the same blocking vector.
#'
#' If you pass in a design that already contains a blocking column, then this
#' blocking column will be replaced without warning.
#'
#' @param x An object of class 'spdesign'
#' @param blocks An integer giving the number of blocks. The number of blocks
#' must be a multiple of the number of rows to ensure equal number of choices
#' within a block.
#' @param target A target value for the mean squared correlation. The default
#' value is 0.0005. Setting the target to 0 forces the function to search all
#' `max_iter` blocking candidates
#' @param max_iter The maximum number of candidates to consider before returning
#' the best blocking candidate. The default value is 1000000.
#'
#' @return A modified 'spdesign' object where the design is replaced with the
#' same design and a blocking column. In addition a correlation vector, number
#' of iterations and the target value are returned as part of the modified
#' 'spdesign' object.
#'
#' @export
block <- function(x, blocks, target = 0.0005, max_iter = 1000000) {
  # Check input class
  stopifnot(class(x) == "spdesign")

  # Copy 'x' to local copy of blocked_design
  blocked_design <- x
  design <- blocked_design[["design"]]

  # Check that the number of blocks are feasible with the size of the design
  if (blocks > nrow(design)) {
    stop("You cannot have more blocks than rows")
  }

  if (nrow(design) %% blocks != 0)  {
    stop("You cannot have uneven number of rows per block")
  }

  # Check if blocking column is present, and if so, drop it.
  blocking_column <- str_detect(colnames(design), "block")
  if (any(blocking_column)) {
    design <- design[, which(!blocking_column)]
  }

  # Create a blocking candidate
  block <- rep(seq_len(blocks), nrow(design) / blocks)

  blocked_design[["blocks_value"]] <- 1
  blocked_design[["design"]] <- dplyr::bind_cols(design, block = block)
  blocked_design[["blocks_correlation"]] <- stats::cor(design, block)
  blocked_design[["blocks_iter"]] <- 1

  # Return the blocked design object when the function exits prematurely.
  on.exit({
    return(blocked_design)
  }, add = TRUE)

  iter <- 1

  repeat {
    # Stop if more than max_iter iterations
    if (iter >= max_iter) break

    # Calculate the correlation between the attributes and a random permutation
    # of the blocking variable.
    block <- sample(block)

    # Suppress warnings when trying to calculate the correlation with respect to
    # a constant.
    correlation <- suppressWarnings(stats::cor(design, block))
    current <- mean(correlation ^ 2, na.rm = TRUE)

    if (current < blocked_design[["blocks_value"]]) {
      blocked_design[["blocks_value"]] <- current
      blocked_design[["design"]] <- dplyr::bind_cols(design, block = block)
      blocked_design[["blocks_correlation"]] <- tibble::as_tibble(t(correlation))
      blocked_design[["blocks_iter"]] <- iter

    }

    # Stopping criteria
    if (blocked_design[["blocks_value"]] <= target) break

    iter <- iter + 1
  }

  return(blocked_design)
}
