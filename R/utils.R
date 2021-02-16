#' Repeat rows
#'
#' Repeats each row in the matrix or data frame 'x' a number of times equal to
#' 'times'.
#'
#' @param x A matrix or data frame
#' @param times An integer indicating the number of times to repeat the
#' row/column
#'
#' @examples
#' test_matrix <- matrix(runif(12), 4)
#' rep_rows(test_matrix, 2)
#'
#' @export
rep_rows <- function(x, times) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("'x' must be a data.frame or matrix")
  }

  x[rep(seq_len(nrow(x)), each = times), , drop = FALSE]
}

#' Repeat columns
#'
#' Repeats each column of the matrix or data frame 'x' a number of times equal
#' to 'times'.
#'
#' @inheritParams rep_rows
#'
#' @examples
#' test_matrix <- matrix(runif(12), 4)
#' rep_cols(test_matrix, 2)
#'
#' @export
rep_cols <- function(x, times) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("'x' must be a data.frame or matrix")
  }

  x[, rep(seq_len(ncol(x)), each = times), drop = FALSE]
}

#' Swaps the values
#'
#' Swaps the first value in values with the second value in values in x.
#'
#' @param x A vector
#' @param values A vector of length 2 with levels to swap
#'
#' @return A vector of the same length as x with swapped values
swap_values <- function(x, values) {
  first_match <- x == values[[1L]]
  second_match <- x == values[[2L]]
  x[first_match] <- values[[2L]]
  x[second_match] <- values[[1L]]
  x
}
