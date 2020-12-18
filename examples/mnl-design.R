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

# Create a candidate set (optional) ----
full_fct <- full_factorial(U, design_opt)

# Generate designs ----
design(U, design_opt, candidate_set = NULL)
