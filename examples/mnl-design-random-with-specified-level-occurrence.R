#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
utility <- list(
  alt1 = "b_x1_dummy[c(0.1, 0.2)] * x_1[c(1, 3, 5)](6:10) + b_x2[0.4] * x_2[c(0, 1)](9:11) + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
  alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
)


# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "federov", draws = "scrambled-sobol")

design <- block(design, 4)
