#
# Example file for creating a simple MNL design
#

# Set design options ----
design_opt <- list(
  optimization_algorithm = "random", # "random", "federov", "rsc"
  efficiency_criteria = "d-efficiency",
  model = "mnl",
  blocks = 1,
  tasks = 6,
  cores = 1
)

# Define the list of utility functions ----
U <- list(
  alt_1 = "beta_1 | c(0.1, 0.5) * x_1.dummy | c(0, 1, 2) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8) * x_3 | c(2, 5)",
  alt_2 = "beta_1 * x_1 + beta_2 * x_2"
)

# Round bracket - prior distribution
# Squared bracket - parameter distribution
# "beta_1 | N[N(0, 1), 1]"

# Create a candidate set (optional) ----
full_fct <- full_factorial(U, design_opt)


# Generate designs ----
design(U, design_opt, candidate_set = NULL)


# This test string contains the most common cases we need to accommodate.
string <- "beta_1 | c(0.1) * x_1 | c(0, 1) + beta_2.dummy | c(0, 1) * x_2 | c(1, 2, 3) + beta_3 | N(0, 1) * x_3 | c(2, 4, 6, 8) + beta_4 | LN[0.4, 1] * x_4 | c(2, 5) + beta_5 | N[N(-0.5, 0.5), 1] * x_5 | c(1, 2, 4, 5)"

# Split so that we have one parameter - attribute pair
x <- unlist(str_split(string, "\\+"))

# Use this to check!
str_detect(x, "(N|LN)\\[")

# This is the worst case sub-string
string <- "beta_1.dummy | c(N[LN(0, 1), T(-1, 1)], U[N(0, 1), LN(1, 0.5)]) * x_1 | c(1, 2, 3)"



string <- "c(N[LN(0, 1), T(-1, 1)], U[N(0, 1), LN(1, 0.5)])"

# Split on the outermost comma
str_extract(string, "c\\(\\w+(\\(|\\[).*?(\\)|\\])\\s*,")

# Strips away the c()
str_extract(string, "(?<=c\\().*(?=\\))")



# Write a loop parser to identifiy opening and closing brackets!

string_split <- str_trim(unlist(str_split(string, "\\+")), side = "both")

string <- "c(N[LN(0,1),T(-1,1)],U[N(0,1),LN(1,0.5)])"


round_bracket <- 0

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

# Worst case : beta_1.dummy | c(N[LN(0, 1), T(-1, 1)], U[N(0, 1), LN(1, 0.5)]) * x_1 | c(1, 2, 3)

# Dummy coded interaction terms?
tmp_1 <- extract_value_arg(tmp, simplify = TRUE)
as.numeric(str_extract_all(tmp_1, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"))

lapply(str_extract_all(tmp_1, expr), function(s) {as.numeric(s)})

# Number of random parameters - Need to run checks on the complete utility functions at some point.
sum(str_count(string, "(N|LN|T|U)(\\(|\\[)"))

# Add a global distribution grouping .

str_split(string, "(\\]|\\)),")

#(\[(?:\[??[^\[]*?\]))

# Important that the parameter names correspond exactly to the names in the priors!


x <- "beta_1 | c(0.1) * x_1 | c(0, 1) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8) * x_3 | c(2, 5)"

x <- "beta_1 | N[N(0, 1), 1] * x_1 + beta_2 | c(0.1) * x_2"

# Remove all white spaces
x <- remove_whitespace(x)

x <- split_string(x, "(?s)\\+", simplify = TRUE)
x <- split_string(x, "(?s)\\*", simplify = FALSE)


str_extract(x, "\\|(.*?)\\+")


# (?<=\\+) positive look behind + and *
# (?=\\|) positive look ahead for |
# The following extracts everything between, but not including * and |
# so basically all of the attribute names
# https://stackoverflow.com/questions/2403122/regular-expression-to-extract-text-between-square-brackets
str_extract_all(x, "(?<=\\*).*?(?=\\|)")


# Can add groupings to it
str_extract_all(x, "(?<=(\\+|\\*)).*?(?=\\|)")


# Extract the utility functions. This appears to work brilliantly !!!!
# (\\*|\\+|^) - Start match at * + or beginning of string
# .*? Non-greedy capture between start match and end match
# (?=(\\||\\Z)) - Positive look ahead for | or end of string \Z escaped with \
str_c(unlist(stringr::str_extract_all(x, "(^|\\*|\\+).*?(?=(\\||\\Z))")), collapse = "")


for (i in c("beta_1", "beta_2")) {

}
# Might work for nested
#(\[(?:\[??[^\[]*?\]))

str_extract(string, "(?<=(beta_1))\\d*?(?=(\\*|\\+\\Z))")

str_extract_all(string, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")
str_extract_all(string, "(?<=(\\))[-+]?[0-9]*\\.?[0-9]+?")


string

str_extract(str_extract(string, "(?<=(\\|))(.*?)(?=(\\*|\\+|$))"), "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")

str_extract(string, "\\d+?")


# Split into components based on "+" - length equal to number of utility elements
x <- unlist(strsplit(x, "\\+", perl = TRUE))

# Each list element contains the parameters and values in the first position and
# attributes in subsequent positions
x <- strsplit(x, "\\*", perl = TRUE)

param <- lapply(x, function(x) x[1])
attrs <- lapply(x, function(x) x[2:length(x)])



x <- param[[1]]

extract_name(x)


str_tmp <- "c(N(-0.3, 1))"
gregexpr("(N|LN|U|T)", str_tmp, perl = TRUE)

unlist(regmatches(str_tmp, gregexpr("(N|LN|U|T)", str_tmp, perl = TRUE)))


str_tmp <- "c(N(N(-0.3, 0.1), 1))"

str_tmp <- "c(N(U(-0.3, 0.1), 1))"





# Now it works.
strs <- c("LN(0, 1)", "N(N(0.1, 1), 1)", "N(U(0, 1), 1)", "N(0, T(0, 1))", "N(LN(0, 1), N(0, 1))")




str_split(strs, "(?:\\G(?!^)\\s*,\\s*$)")
