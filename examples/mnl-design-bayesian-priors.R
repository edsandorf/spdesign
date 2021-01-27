#
# Example file for creating a simple MNL design with Bayesian priors
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Set design options ----
opts <- list(
  optimization_algorithm = "random",
  efficiency_criteria = "d-error",
  model = "mnl",
  blocks = 1,
  tasks = 6,
  cores = 1,
  max_iter = 1000
)

# Define the list of utility functions ----
V <- list(
  alt1 = "b_x1[0.1] * x_1[2:5] + b_x2[Up(-1, 1)] * x_2[c(0, 1)] + b_x3[Np(0, 1)] * x_3[seq(0, 1, 0.25)]",
  alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
)

# Generate designs ----
generate_design(V, opts)
