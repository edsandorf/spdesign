#
# Example file for creating a simple MNL design
#

# Set design options ----
design_opt <- list(
  efficiency_criteria = "d-efficiency",
  model = "mnl"
)

# Define the attributes and levels ----


# Create a candidate set ----
full_fct <- full_factorial(design_opt)

# Define the list of priors ----


# Define the list of utility functions ----

# Round bracket - prior distribution
# Squared bracket - parameter distribution
# "beta_1 | N[N(0, 1), 1]"


# Generate designs ----
design()

# Specify a named vector of priors. Remember, no prior/a zero prior is still a prior
priors <- list(
  fixed = c(
    beta_1 = 0.1,
    beta_2 = 0.2
  ),
  random = NULL, # list of named pairs of parameters, e.g. list(beta_3 = c(mu_beta_3 = 0, sigma_beta_3 = 0.1)))
  # length of random determines the number of random parameters.
  bayesian = NULL # This would refer to the parameters themselves either fixed or random
)

# Specify a list of attributes and their levels


# Important that the parameter names correspond exactly to the names in the priors!
U <- list(
   alt_1 = "beta_1 | c(0.1, 0.5) * x_1.dummy | c(0, 1, 2) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8) * x_3 | c(2, 5)",
   alt_2 = "beta_1 * x_1 + beta_2 * x_2"
)

x <- "beta_1 | c(0.1) * x_1 | c(0, 1) + beta_2 | 0.1 * x_2 | c(2, 4, 6, 8) * x_3 | c(2, 5)"

# Remove all white spaces
x <- remove_whitespace(x)

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

# Bayesian prir, N, LN, U, T,
# N(0, 1)
# Mixed with bayesian
# N(N(0 , 1), N(0, 1))

sub("\\|.* \\*\\|.*", "", x, perl = TRUE)
grepl("\\|.*", x, perl = TRUE)
regexec("\\|.*", x, perl = TRUE)


strs <- c("N(0, 1)", "N(N(0.1, 1), 1)", "N(U(0, 1), 1)", "N(0, T(0, 1))", "N(N(0, 1), N(0, 1))")
p <- "(?:\\G(?!^)\\s*,\\s*|^N\\()\\K(?:\\d+|\\w+(\\([^()]*(?:(?1)[^()]*)*\\)))(?=\\s*,|\\)$)"
regmatches(strs, gregexpr(p, strs, perl=TRUE))


strings <- c("N(0, 1)", "LN(LN(0.1, 1), 1)", "N(U(0, 1), 1)", "N(0, LN(0, 1))", "N(N(0, 1), N(0, 1))")
lapply(parse(text=strings), function(x)c(deparse(x[[2]]), deparse(x[[3]])))



lapply(parse(text = strings), function(x) deparse(x[[3]]))




# input is char vec; output is list of char vecs
Split <- function(s) {
  p <- proto(
    pre = function(this) this$num <- 0,
    fun = function(this, x) {
      this$num <- this$num + ( x == "(" ) - ( x == ")" )
      if (x == "," && this$num == 0) ";" else x
    }
  )

  s %>%
    sub("^..(.*).$", r"{\1}", .) %>%  # rm junk
    gsubfn(r"{[\(\),]}", p, .) %>%  # replace outer , with ;
    strsplit(" *; *")  # split at ;
}

# test 1
s <- c(str_1 = "LN(0, 1)", str_2 = "LN(N(0.1, 1), 1)", str_3 = "N(U(0, 1), 1)",
       str_4 = "N(0, T(0, 1))", str_5 = "N(N(0, 1), N(0, 1))")
Split("LN(0, N(0, 1))")
