rows <- 80
model <- "mnl"
efficiency_criteria <- "d-error"
algorithm <- "federov"
draws <- "pseudo-random"
R <- 100
dudx <- "x3"
candidate_set <- NULL
exclusions <- NULL
control <- list(
  cores = 1,
  max_iter = 100000,
  max_relabel = 10000,
  max_no_improve = 100000,
  efficiency_threshold = 0.00001,
  sample_with_replacement = FALSE
)
return_all <- FALSE
significance <- 1.96


#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
#' Specifying a utility function with 3 attributes and a constant for the
#' SQ alternative. The design has 20 rows.
utility <- list(
  alt1 = "b_x1[0.1] * x1[1:6] + b_x2_dummy[c(0, 0)] * x2[1:3] + b_x3[-0.01] * x3[seq(1, 10, 2)]",
  alt2 = "b_x1          * x1      + b_x2_dummy          * x2      + b_x3          * x3"
)

# Generate designs ----
design <- generate_design(utility,
                          rows = 50,
                          model = "mnl",
                          efficiency_criteria = "d-error",
                          algorithm = "federov",
                          draws = "scrambled-sobol",
                          control = list(
                            max_iter = 100000,
                            max_no_improve = 5000,
                            efficiency_threshold = 0.00001
                          ))

# Add a blocking variable to the design with 4 blocks.
design <- block(design, 4)


summary(design)



utility
rows = 20
model = "mnl"
efficiency_criteria = "d-error"
algorithm = "federov"
draws = "scrambled-sobol"
exclusions = list(
  "alt1_x1 == 2 & alt1_x2 == 0 & alt1_x3 == 0",
  "alt2_x2 == 1 & alt2_x3 == 1"
)
