#
# Example file for creating a simple MNL design with Bayesian priors
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
#' Generating a design with Bayesian priors.
utility <- list(
  alt1 = "b_x1[0.1] * x1[2:5] + b_x2[uniform_p(-1, 1)] * x2[c(0, 1)] + b_x3[normal_p(0, 1)] * x3[seq(0, 1, 0.25)]",
  alt2 = "b_x1      * x1      + b_x2                   * x2          + b_x3                 * x3"
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "rsc", draws = "scrambled-sobol",
                          control = list(
                            max_iter = 10000
                          ))

summary(design)
