#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Set design options ----
opts <- list(
  algorithm = list(
    alg = "federov"
  ),
  efficiency_criteria = "d-error",
  model = "mnl",
  blocks = 1,
  tasks = 20,
  cores = 1,
  max_iter = 10000
)

# Define the list of utility functions ----
utility <- list(
  alt1 = "b_x1[0.1] * x_1[1:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x1x2[-0.1] * I(x_1 * x_2)",
  alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
)

# Generate designs ----
design <- generate_design(utility, opts)
