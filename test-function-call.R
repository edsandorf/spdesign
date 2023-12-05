rows <- 100
model <- "mnl"
efficiency_criteria <- "d-error"
algorithm <- "federov"
draws <- "pseudo-random"
R <- 100
dudx <- NULL
candidate_set <- candidate.set.final
exclusions <- NULL
control <- list(
  cores = 1,
  max_iter = 10000,
  max_relabel = 10000,
  max_no_improve = 100000,
  efficiency_threshold = 0.1,
  sample_with_replacement = FALSE
)
