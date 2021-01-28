context("Test assertions")

test_that("is_balance returns TRUE/FALSE in correct cases", {
  expect_true(is_balanced("N()", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", ")"))
  expect_false(is_balanced("b_x1[Np(0, 1), Np(0, 1]", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "[", "]"))
  expect_warning(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", "]"))
})

test_that("has_bayesian_prior correctly identifies whether a prior is present", {
  expect_true(has_bayesian_prior("b_x1[N(Np(0, 1), Np(0, 1))]"))
  expect_false(has_bayesian_prior("b_x1[N(N(0, 1), N(0, 1))]"))
  expect_true(has_bayesian_prior(list("b_x1[N(Np(0, 1), Np(0, 1))]",
                                      "b_x1[0.1]")))
  expect_true(has_bayesian_prior(list("b_x1[N(N(0, 1), N(0, 1))]",
                                      "b_x1[Up(0.1, 0.2)]")))
})
