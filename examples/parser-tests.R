string <- "beta_1[0.1] * x"
remove_whitespace(extract_name_args(string, simplify = TRUE))
eval(parse(text = unlist(extract_value_args(string))))




str_locate_all(string, "(\\(|\\[)(\\)\\])")

find_matching_parenthesis <- function(string) {
  # Count how many opening and closing parenthesis and brackets we have
  brackets_open <- str_count(string, "(\\(|\\[)")
  brackets_closed <- str_count(string, "(\\)|\\])")
  if (brackets_open - brackets_closed != 0) {
    stop("The number of opening and closing parentheses or brackets do not match. Check your utility function carefully. Pay close attention to random parameters, bayesian priors and other nested structures.")
  }

  # Remove white spaces
  string <- remove_whitespace(string)

  # Split into individual pieces to work on
  string <- str_split(string, "", simplify = TRUE)

  # Define a matrix to store outputs
  pos <- matrix(nrow = brackets_open, ncol = 2)
  type <- rep(NA, brackets_open)
  match_counter <- 0

  # Set the initial open position to 0
  pos_open <- 0

  # First find an opening parenthesis
  for (s in string) {
    # Increment the counter of the opening bracket
    pos_open <- pos_open + 1

    # Check if we have encountered an opening bracket
    if (s %in% c("(", "[")) {
      match_counter <- match_counter + 1
      type[match_counter] <- s
      pos[match_counter, 1] <- pos_open


      pos_close <- pos_open


      # Find the corresponding matching parenthesis or bracket - it insists on completing the string walk
      counter <- 1
      while (counter > 0) {
        # Search through the rest of the vector to find the close
        for (sub_s in string[(pos_open + 1):length(string)]) {
          # Update the closing position
          pos_close <- pos_close + 1
          if (sub_s %in% c("(", "[")) {
            counter <- counter + 1
          } else if (sub_s %in% c(")", "]")) {
            counter <- counter - 1
          }
        }
      }
      pos[match_counter, 2] <- pos_close
    }
  }
  list(
    pos = pos,
    type = type
  )
}




#' Checks whether
is_balanced_square <- function(string) {

}



string <- "beta0[0.1] * x + beta2[N(0, 1)] * x_3[seq(0, 1, 0.1)] / delta[1+2]"






V <- list(
  alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[Np(0, 1)] * x_3[seq(0, 1, 0.25)]",
  alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3 + x_2[1:6]"
)


tmp <- parse_utility(V)


# When testing attributes, check whether levels are multiples of each other to determine whether
# a balanced design is possible.

#'
#'
#'
#'
#'
#'
#'
create_design_env <- function() {

}
