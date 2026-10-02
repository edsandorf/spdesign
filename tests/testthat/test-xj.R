context("Test the definition and alignment of x_j")

# Draw a design candidate from the full factorial. Dummy-coded attributes are
# turned into factors before sampling, as in generate_design()
draw_design_candidate <- function(utility, rows = 12) {
  candidate_set <- full_factorial(expand_attribute_levels(utility))

  for (i in which(names(candidate_set) %in% dummy_names(utility))) {
    candidate_set[, i] <- as.factor(candidate_set[, i])
  }

  set.seed(1234)

  return(
    candidate_set[sample(nrow(candidate_set), rows), ]
  )
}

utility_interactions <- list(
  alt1 = "b_x1[0.1] * x1[1:3] + b_x2[0.4] * x2[c(0, 1)] + b_x12[-0.1] * I(x1 * x2)",
  alt2 = "b_x1      * x1      + b_x2      * x2"
)

test_that("Terms are paired with their parameters", {
  expect_equal(
    pair_param_terms(update_utility(utility_interactions)),
    list(
      alt1 = c(
        alt1_x1 = "b_x1",
        alt1_x2 = "b_x2",
        "I(alt1_x1*alt1_x2)" = "b_x12"
      ),
      alt2 = c(alt2_x1 = "b_x1", alt2_x2 = "b_x2")
    )
  )
})

test_that("x_j has one column per term and can drop interactions", {
  design_candidate <- draw_design_candidate(utility_interactions)
  x_j <- define_x_j(utility_interactions, design_candidate)

  expect_equal(colnames(x_j$alt1), c("alt1_x1", "alt1_x2", "I(alt1_x1*alt1_x2)"))
  expect_equal(colnames(x_j$alt2), c("alt2_x1", "alt2_x2"))
  expect_equal(
    x_j$alt1[, "I(alt1_x1*alt1_x2)"],
    design_candidate$alt1_x1 * design_candidate$alt1_x2,
    check.attributes = FALSE
  )

  x_j <- define_x_j(utility_interactions, design_candidate, interactions = FALSE)
  expect_equal(colnames(x_j$alt1), c("alt1_x1", "alt1_x2"))
})

test_that("Squared terms, unspaced interactions and names starting with I work", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[1:3] + b_Inc[0.2] * Income[c(1, 2)] + b_x1sq[0.1] * I(x1^2) + b_x1inc[0.1] * I(x1*Income)",
    alt2 = "b_x1      * x1      + b_Inc      * Income"
  )

  x_j <- define_x_j(utility, draw_design_candidate(utility))

  expect_equal(
    colnames(x_j$alt1),
    c("alt1_x1", "alt1_Income", "I(alt1_x1^2)", "I(alt1_x1*alt1_Income)")
  )
})

test_that("Aligned x_j has one column per prior in the order of the priors", {
  # Generic parameters with a differently named attribute in each alternative
  # and an alternative specific constant
  utility <- list(
    car = "b_time[-0.1] * time_car[c(10, 20)] + b_cost[-0.2] * cost_car[c(5, 10)]",
    bus = "b_bus[0.1] * bus[1] + b_time * time_bus[c(20, 30)] + b_cost * cost_bus[c(2, 4)]"
  )

  design_candidate <- draw_design_candidate(utility, rows = 8)
  terms <- pair_param_terms(update_utility(utility))
  x_j <- align_x_j(define_x_j(utility, design_candidate, terms), terms, names(priors(utility)))

  expect_equal(colnames(x_j$car), names(priors(utility)))
  expect_equal(colnames(x_j$bus), names(priors(utility)))
  expect_equal(x_j$car[, "b_time"], design_candidate$car_time_car, check.attributes = FALSE)
  expect_equal(x_j$bus[, "b_time"], design_candidate$bus_time_bus, check.attributes = FALSE)
  expect_true(all(x_j$car[, "b_bus"] == 0))
})

test_that("Dummy-coded columns are matched to the right prior", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0.1, 0.2)] * x1[c(1, 2, 3)] + b_x2[0.4] * x2[c(0, 1)]",
    alt2 = "b_x1_dummy                * x1                + b_x2      * x2"
  )

  design_candidate <- draw_design_candidate(utility)
  terms <- pair_param_terms(update_utility(utility))
  x_j <- align_x_j(define_x_j(utility, design_candidate, terms), terms, names(priors(utility)))

  expect_equal(colnames(x_j$alt1), names(priors(utility)))
  expect_equal(
    x_j$alt1[, "b_x12"],
    as.numeric(design_candidate$alt1_x1 == 2),
    check.attributes = FALSE
  )
  expect_equal(
    x_j$alt1[, "b_x2"],
    design_candidate$alt1_x2,
    check.attributes = FALSE
  )
})

test_that("Unmatched terms and parameters without a prior are errors", {
  # The prior must be written before the attribute
  utility <- list(
    alt1 = "x1[1:3] * b_x1[0.1] + b_x2[0.2] * x2[c(0, 1)]",
    alt2 = "x1      * b_x1      + b_x2      * x2"
  )

  expect_error(
    define_x_j(utility, draw_design_candidate(utility)),
    "Could not match"
  )

  terms <- pair_param_terms(update_utility(utility_interactions))
  x_j <- define_x_j(utility_interactions, draw_design_candidate(utility_interactions), terms)

  expect_error(align_x_j(x_j, terms, c("b_x1", "b_x2")), "do not have a prior")
})
