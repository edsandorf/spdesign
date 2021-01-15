context("Test util functions")

test_that("is_balance returns TRUE/FALSE in correct cases", {
  expect_true(is_balanced("N()", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", ")"))
  expect_false(is_balanced("b_x1[Np(0, 1), Np(0, 1]", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "[", "]"))
  expect_warning(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", "]"))
})
