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
