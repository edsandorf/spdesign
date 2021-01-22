#
# Example file for creating a simple MNL design
#

library(spdesign)

# Set design options ----
opts <- list(
  optimization_algorithm = "random", # "random", "federov", "rsc"
  efficiency_criteria = "d_efficiency",
  model = "mnl",
  blocks = 1,
  tasks = 6,
  cores = 1
)

# Define the list of utility functions ----
V <- list(
  alt1 = "b_x1[0.1] * x_1[2:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
  alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
)

# Generate designs ----
generate_design(V, opts)
