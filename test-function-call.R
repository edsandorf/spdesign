rows <- 10
model <- "mnl"
efficiency_criteria <- "d-error"
algorithm <- "rsc"
draws <- "pseudo-random"
R <- 100
dudx <- "x3"
candidate_set <- NULL
exclusions <- NULL
control <- list(
  cores = 1,
  max_iter = 10000,
  max_relabel = 10000,
  max_no_improve = 100000,
  efficiency_threshold = 0.1,
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
  alt1 = "b_x1_dummy[c(0, 0)] * x1[1:3] + b_x2_dummy[c(0, 0)] * x2[1:3] + b_x3_dummy[c(0, 0)] * x3[1:3]",
  alt2 = "b_x1_dummy          * x1      + b_x2_dummy          * x2      + b_x3_dummy          * x3"
)

# Generate designs ----
design <- generate_design(utility,
                          rows = 10,
                          model = "mnl",
                          efficiency_criteria = "d-error",
                          algorithm = "rsc",
                          draws = "scrambled-sobol",
                          control = list(
                            max_iter = 21000,
                            max_no_improve = 5000
                          ))

# Add a blocking variable to the design with 4 blocks.
design <- block(design, 4)


summary(design)
