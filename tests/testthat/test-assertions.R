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
