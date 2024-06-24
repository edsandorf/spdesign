#'
#'
#' Print level balance of your design
#'
#' Prints a table of level balance for your design. If the design is blocked
#' you will get both level balance per block and overall level balance
#'
#' @param design An spdesign object
#' @param block A boolean equal to TRUE if you want frequency tables per block.
#' The default value is FALSE
#'
#' @export
level_balance <- function(design, block = FALSE) {
  x <- design[["design"]]

  if (block) {
    blocked <- split(x, x$block)
    names(blocked) <- paste("block", unique(x$block), sep = "_")
    # Dropping the last column because it is the blocking column by default.
    unlist(lapply(blocked, function(y) lapply(y[, -ncol(y)], table)), recursive = FALSE)

  } else {
    lapply(x, table)

  }

}
