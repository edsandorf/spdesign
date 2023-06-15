#
# Example file for creating a simple MNL design with Bayesian priors
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
#' Generating a design with Bayesian priors using dummy coding and level occurrences
#' NOTE: This design may take a long time. It has to first find a candidate that
#' meets the restrictions and then evaluate whether it is better than the
#' current best. Little information is provided about the process along the way.
utility <- list(
  alt1 = "b_x1_dummy[c(uniform_p(-1, 1), uniform_p(-1, 1))] * x1[c(1, 2, 3)](6:10, 4:14, 6:10) + b_x2[0.4] * x2[c(0, 1)](9:11) + b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
  alt2 = "b_x1_dummy                                        * x1                               + b_x2      * x2                + b_x3       * x3"
)


# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "random", draws = "scrambled-sobol",
                          control = list(
                            max_iter = 10000
                          ))

summary(design)
