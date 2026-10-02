context("Test the design search")

utility <- list(
  alt1 = "b_x1[0.2] * x1[c(1, 2, 3)](2:4) + b_x2[-0.4] * x2[c(0, 1)](4)",
  alt2 = "b_x1      * x1                  + b_x2       * x2"
)

rows <- 8

# Generate a design without printing the search to the console
quiet_design <- function(utility, rows, ...) {
  utils::capture.output(
    design <- suppressMessages(
      generate_design(
        utility,
        rows = rows,
        model = "mnl",
        efficiency_criteria = "d-error",
        draws = "pseudo-random",
        ...
      )
    )
  )

  return(
    design
  )
}

test_that("Federov and random designs satisfy level occurrences", {
  set.seed(1234)

  for (algorithm in c("federov", "random")) {
    design <- quiet_design(
      utility,
      rows,
      algorithm = algorithm,
      control = list(max_iter = 150)
    )

    expect_equal(nrow(design$design), rows)
    expect_equal(lvl_violation(utility, design$design, rows), 0)
    expect_equal(anyDuplicated(design$design), 0)
  }
})

test_that("Federov restarts and keeps the best design of each run", {
  set.seed(1234)

  design <- quiet_design(
    utility,
    rows,
    algorithm = "federov",
    control = list(max_iter = 150)
  )

  run_errors <- vapply(
    design$runs,
    function(run) run$efficiency_criteria[["d-error"]],
    numeric(1)
  )

  expect_gt(length(design$runs), 1)
  expect_equal(min(run_errors), design$efficiency_criteria[["d-error"]])
})

test_that("RSC designs are found", {
  utility <- list(
    alt1 = "b_x1[0.2] * x1[c(1, 2, 3)] + b_x2[-0.4] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1             + b_x2       * x2"
  )

  set.seed(1234)

  design <- quiet_design(
    utility,
    rows = 6,
    algorithm = "rsc",
    control = list(max_iter = 50)
  )

  expect_equal(nrow(design$design), 6)
  expect_false(is.na(design$efficiency_criteria[["d-error"]]))
})

test_that("An error is raised when no design can identify all parameters", {
  set.seed(1234)

  # With identical x2 in both alternatives, b_x2 is never identified
  utility <- list(
    alt1 = "b_x1[0.2] * x1[c(1, 2, 3)] + b_x2[-0.4] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1             + b_x2       * x2"
  )

  candidate_set <- full_factorial(expand_attribute_levels(utility))
  candidate_set <- candidate_set[
    candidate_set$alt1_x2 == candidate_set$alt2_x2,
  ]

  for (algorithm in c("federov", "random")) {
    expect_error(
      quiet_design(
        utility,
        rows = 6,
        algorithm = algorithm,
        candidate_set = candidate_set,
        control = list(max_iter = 20)
      ),
      "non-singular"
    )
  }

  # A constant attribute can never be identified. This used to loop forever
  # in the RSC algorithm.
  utility$alt1 <- "b_x1[0.2] * x1[c(1, 2, 3)] + b_x2[-0.4] * x2[c(1, 1)]"

  expect_error(
    quiet_design(
      utility,
      rows = 6,
      algorithm = "rsc",
      control = list(max_iter = 20)
    ),
    "non-singular"
  )
})

test_that("Argument errors are not swallowed", {
  expect_error(
    quiet_design(
      utility,
      rows,
      efficiency_criteria = c("d-error", "a-error"),
      algorithm = "federov"
    )
  )
})

test_that("Designs with interactions give a complete variance-covariance matrix", {
  # Interaction terms used to be dropped from the Fisher information matrix,
  # which gave the warning 'longer object length is not a multiple of shorter
  # object length'
  utility <- list(
    alt1 = "b_x1[0.1] * x1[1:5] + b_x2[0.4] * x2[c(0, 1)] + b_x12[-0.1] * I(x1 * x2)",
    alt2 = "b_x1      * x1      + b_x2      * x2"
  )

  set.seed(1234)

  expect_warning(
    design <- quiet_design(
      utility,
      rows = 10,
      algorithm = "federov",
      control = list(max_iter = 20)
    ),
    NA
  )

  expect_equal(rownames(design$vcov), names(priors(utility)))
  expect_true(isSymmetric(unname(design$vcov)))
  expect_false("I(alt1_x1*alt1_x2)" %in% names(design$design))
})
