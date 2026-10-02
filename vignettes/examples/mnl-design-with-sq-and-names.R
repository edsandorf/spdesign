#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
# Dummy-coded attributes must have the levels 1, 2, ..., K, where 1 is the base
# level. Here, new_wind_mills: 1 = 1500 (base), 2 = 750 and 3 = 500 wind mills.
utility <- list(
  sq = "b_sq[0.1] * sq[1]                                                              ",
  alt1 = "                   b_new_wind_mills_dummy[c(-0.2, -0.4)] * new_wind_mills[c(1, 2, 3)] + b_distance[0.15] * distance_km[c(0.25, 0.50, 0.75, 1, 1.25)] + b_fee[-0.25] * fee[c(5, 10, 15, 20, 25)]",
  alt2 = "                   b_new_wind_mills_dummy                * new_wind_mills             + b_distance       * distance_km                               + b_fee        * fee"
)

# Generate designs ----
design <- generate_design(
  utility,
  rows = 20,
  model = "mnl",
  efficiency_criteria = "d-error",
  algorithm = "federov",
  draws = "scrambled-sobol",
  control = list(
    max_iter = 15000,
    efficiency_threshold = 0.01
  )
)

# Add a blocking variable to the design with 2 blocks.
design <- block(design, 2)

summary(design)
