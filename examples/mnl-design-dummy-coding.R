#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
#' A design where the first attribute is dummy-coded.
utility <- list(
  alt1 = "b_x1_dummy[c(0.1, 0.2)] * x1[c(1, 2, 3)] + b_x2[0.4] * x2[c(0, 1)] + b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
  alt2 = "b_x1_dummy              * x1             + b_x2      * x2          + b_x3       * x3"
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "rsc", draws = "scrambled-sobol")

# Add a blocking variable to the design with 2 blocks.
design <- block(design, 2)


summary(design)
