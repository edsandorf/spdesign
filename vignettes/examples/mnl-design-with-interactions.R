#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
utility <- list(
  alt1 = "b_x1[0.1] * x1[1:5] + b_x2[0.4] * x2[c(0, 1)] + b_x3[-0.2] * x3[seq(0, 1, 0.25)] + b_x1x2[-0.1] * I(x1 * x2)",
  alt2 = "b_x1      * x1      + b_x2      * x2          + b_x3       * x3"
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "federov", draws = "scrambled-sobol",
                          dudx = "b_x3")

summary(design)
