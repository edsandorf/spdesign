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
  tasks = 6,
  cores = 1,
  max_iter = 10000
)

# Define the list of utility functions ----
utility <- list(
  alt1 = "b_x1[0.1] * x1[2:5] + b_x2[0.4] * x2[c(0, 1)] + b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
  alt2 = "b_x1      * x1      + b_x2      * x2          + b_x3       * x3"
)

# Use the full factorial as the candidate set
candidate_set <- generate_full_factorial(
  list(
    alt1_x1 = 1:5,
    alt1_x2 = c(0, 1),
    alt1_x3 = seq(0, 1, 0.25),
    alt2_x1 = 1:5,
    alt2_x2 = c(0, 1),
    alt2_x3 = seq(0, 1, 0.25)
  )
)

candidate_set <- candidate_set[!(candidate_set$alt1_x1 == 2 & candidate_set$alt1_x2 == 0 & candidate_set$alt1_x3 == 0), ]
candidate_set <- candidate_set[!(candidate_set$alt2_x2 == 1 & candidate_set$alt2_x3 == 1), ]

# Generate designs ----
design <- generate_design(utility, opts, candidate_set)
