context("Test assertions")

test_that("is_balance returns TRUE/FALSE in correct cases", {
  expect_true(
    is_balanced("normal()", "(", ")")
  )
  expect_true(
    is_balanced("b_x1[normal_p(0, 1), normal_p(0, 1)]", "(", ")")
  )
  expect_false(
    is_balanced("b_x1[normal_p(0, 1), normal_p(0, 1]", "(", ")")
  )
  expect_true(
    is_balanced("b_x1[normal_p(0, 1), normal_p(0, 1)]", "[", "]")
  )
  expect_warning(
    is_balanced("b_x1[normal_p(0, 1), normal_p(0, 1)]", "(", "]")
  )
})

test_that("Correctly identifies whether a prior is present", {
  expect_true(
    has_bayesian_prior("b_x1[normal(normal_p(0, 1), normal_p(0, 1))]")
  )
  expect_false(
    has_bayesian_prior("b_x1[normal(normal(0, 1), normal(0, 1))]")
  )
  expect_true(
    has_bayesian_prior(
      list(
        "b_x1[normal(normal_p(0, 1), normal_p(0, 1))]",
        "b_x1[0.1]"
      )
    )
  )
  expect_true(
    has_bayesian_prior(
      list(
        "b_x1[normal(normal(0, 1), normal(0, 1))]",
        "b_x1[uniform_p(0.1, 0.2)]"
      )
    )
  )
})

test_that("Correctly identifies whether a random parameter is present", {
  expect_true(
    has_random_parameter("b_x1[normal(normal_p(0, 1), normal_p(0, 1))]")
  )
  expect_false(
    has_random_parameter("b_x1[normal_p(normal_p(0, 1), normal_p(0, 1))]")
  )
  expect_true(
    has_random_parameter(
      list(
        "b_x1[normal(normal_p(0, 1), normal_p(0, 1))]",
        "b_x1[0.1]"
      )
    )
  )
  expect_true(
    has_random_parameter(
      list(
        "b_x1[normal_p(normal(0, 1), normal_p(0, 1))]",
        "b_x1[U(0.1, 0.2)]"
      )
    )
  )
})

test_that("Correctly identifies whether dummy codingsi  present", {
  expect_true(
    contains_dummies("b_x1_dummy[c(0.1, 0.2)] * x1[c(1, 2, 3)]")
  )

  expect_false(
    contains_dummies("b_x1[c(0.1, 0.2] * dummy_x1[1:3]")
  )

  expect_false(
    contains_dummies(
      list(
        "b_x1 * x2[c(1, 2, 3)]",
        "b_x1[c(0.1, 0.2)] * x1_dummy[c(1, 2, 3)]"
      )
    )
  )

  expect_false(
    contains_dummies(
      list(
        "b_x1[c(0.1, 0.2)] * x1_dummy[c(1, 2, 3)]",
        "b_x2[c(0.1, 0.2)] * x2_dummy[c(1, 2, 3)]"
      )
    )
  )

  expect_true(
    contains_dummies(
      list(
        "b_x1_dummy[c(0.1, 0.2)] * x1[c(1, 2, 3)]",
        "b_x2[c(0.1, 0.2)] * x2_dummy[c(1, 2, 3)]"
      )
    )
  )
})


test_that("Correctly determines when occurrences are specified", {
  expect_true(
    level_occurrences_specified(
      list(
        alt1 = "b_x1[0.1] * x1[1:6](0:10) + b_x2_dummy[c(0, 0)] * x2[1:3]",
        alt2 = "b_x1          * x1      + b_x2_dummy          * x2"
      )
    )
  )

  expect_true(
    level_occurrences_specified(
      list(
        alt1 = "b_x1[0.1] * x1[1:6](4:14) + b_x2[c(0, 0)] * x2[1:3](9:11)",
        alt2 = "b_x1          * x1                   + b_x2      * x2"
      )
    )
  )

  expect_false(
    level_occurrences_specified(
      list(
        alt1 = "b_x1[0.1] * x1[1:6] + b_x2[c(0, 0)] * x2[1:3]",
        alt2 = "b_x1          * x1      + b_x2          * x2"
      )
    )
  )
})

test_that("Correctly measures the distance from the level occurrences", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[c(1, 2, 3)](1:2, 2, 1:2) + b_x2[0.4] * x2[c(0, 1)](2)",
    alt2 = "b_x1      * x1                          + b_x2      * x2"
  )

  rows <- 4

  # All level occurrences satisfied
  design <- data.frame(
    alt1_x1 = c(1, 2, 2, 3),
    alt1_x2 = c(0, 0, 1, 1),
    alt2_x1 = c(3, 2, 1, 2),
    alt2_x2 = c(1, 0, 1, 0)
  )

  expect_equal(lvl_violation(utility, design, rows), 0)
  expect_equal(lvl_violation(utility, as.matrix(design), rows), 0)

  # alt1_x1 has counts (3, 1, 0) against (1:2, 2, 1:2): distance 1 + 1 + 1
  # alt1_x2 has counts (3, 1) against (2, 2): distance 1 + 1
  design$alt1_x1 <- c(1, 1, 1, 2)
  design$alt1_x2 <- c(0, 0, 0, 1)

  expect_equal(lvl_violation(utility, design, rows), 5)

  # Passing pre-computed occurrences and levels gives the same result
  expect_equal(
    lvl_violation(
      utility,
      design,
      rows,
      occurrences(utility, rows),
      expand_attribute_levels(utility)
    ),
    5
  )

  # A level missing from the design counts as zero occurrences
  design$alt1_x1 <- c(1, 1, 2, 2)
  design$alt1_x2 <- c(0, 0, 1, 1)

  expect_equal(lvl_violation(utility, design, rows), 1)
})

test_that("Finds unlisted levels only for attributes with level occurrences", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[c(1, 2, 3)](1:2, 2, 1:2) + b_x2[0.4] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1                          + b_x2      * x2"
  )

  rows <- 4

  candidate_set <- data.frame(
    alt1_x1 = c(1, 2, 3, 2),
    alt1_x2 = c(0, 1, 0, 1),
    alt2_x1 = c(3, 2, 1, 1),
    alt2_x2 = c(1, 0, 1, 0)
  )

  expect_length(unlisted_levels(utility, candidate_set, rows), 0)

  # An unlisted level for an attribute without level occurrences is allowed
  candidate_set$alt1_x2 <- c(0, 1, 2, 5)
  expect_length(unlisted_levels(utility, candidate_set, rows), 0)

  # An unlisted level for an attribute with level occurrences is flagged
  candidate_set$alt2_x1 <- c(3, 2, 1, 4)
  expect_equal(unlisted_levels(utility, candidate_set, rows), "alt2_x1")
})

test_that("Dummy-coded attributes must have levels 1, 2, ..., K and K - 1 priors", {
  # Valid levels written in different ways, with fixed and Bayesian priors and
  # level occurrences
  expect_length(
    invalid_dummy_coding(list(
      alt1 = "b_x1_dummy[c(0.1, 0.2)] * x1[c(1, 2, 3)] + b_x2_dummy[c(uniform_p(-1, 1))] * x2[1:2]",
      alt2 = "b_x1_dummy * x1 + b_x2_dummy * x2 + b_x3_dummy[c(0.1, 0.2, 0.3)] * x3[seq(1, 4)](2:6)"
    )),
    0
  )

  # No dummy-coded attributes
  expect_length(
    invalid_dummy_coding(list(alt1 = "b_x1[0.1] * x1[c(0, 5, 10)]", alt2 = "b_x1 * x1")),
    0
  )

  # Starting at 0, gaps, unsorted, negative and decimal levels are invalid
  expect_equal(
    invalid_dummy_coding(list(
      alt1 = "b_a_dummy[0.1] * a[c(0, 1)] + b_b_dummy[c(0.1, 0.2)] * b[c(1, 3, 5)] + b_c_dummy[c(0.1, 0.2)] * c[c(3, 1, 2)]",
      alt2 = "b_d_dummy[c(0.1, 0.2)] * d[c(0, -1, 2)] + b_e_dummy[c(0.1, 0.2)] * e[c(1, 1.5, 2)] + b_f_dummy[c(0.1, 0.2)] * f[1:3]"
    )),
    c("a", "b", "c", "d", "e")
  )

  # Too many and too few priors are invalid
  expect_equal(
    invalid_dummy_coding(list(
      alt1 = "b_a_dummy[c(uniform_p(-1, 1), uniform_p(-1, 1))] * a[c(1, 2)] + b_b_dummy[0.1] * b[c(1, 2, 3)]",
      alt2 = "b_a_dummy * a + b_b_dummy * b"
    )),
    c("a", "b")
  )
})
