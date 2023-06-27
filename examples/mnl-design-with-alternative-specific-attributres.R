#
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
# library(spdesign)

# Define the list of utility functions ----
utility <- list(
  car   = "                          b_travel_time[-0.1] * travel_time_car[c(10, 15, 20, 25)]   + b_travel_cost[-0.2] * travel_cost_car[c(25, 50, 75, 100)]  + b_comfort_dummy[c(0.1, 0.2)] * comfort[c(1, 2, 3)]",
  bus   = "b_bus[-0.1]  * bus[1]   + b_travel_time       * travel_time_bus[c(20, 25, 30, 35)]   + b_travel_cost       * travel_cost_bus[c(10, 15, 20, 25)]   + b_comfort_dummy              * comfort",
  train = "b_train[0.1] * train[1] + b_travel_time       * travel_time_train[c(15, 20, 25, 30)] + b_travel_cost       * travel_cost_train[c(20, 30, 40, 50)] + b_comfort_dummy              * comfort"
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "rsc", draws = "scrambled-sobol",
                          control = list(
                            max_iter = 21000,
                            efficiency_threshold = 0.01
                          ))

# Add a blocking variable to the design with 4 blocks.
design <- block(design, 4)


summary(design)
