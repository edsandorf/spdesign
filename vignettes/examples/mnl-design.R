#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
#' Specifying a utility function with 3 attributes and a constant for the
#' SQ alternative. The design has 20 rows.
utility <- list(
  alt1 = "b_x1[0.1]  * x1[1:5] + b_x2[0.4] * x2[c(0, 1)] + b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
  alt2 = "b_x1       * x1      + b_x2      * x2          + b_x3       * x3",
  alt3 = "b_sq[0.15] * sq[1]"
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "rsc", draws = "scrambled-sobol",
                          control = list(
                            max_iter = 21000,
                            max_no_improve = 5000
                          ))

# Add a blocking variable to the design with 4 blocks.
design <- block(design, 4)


summary(design)
