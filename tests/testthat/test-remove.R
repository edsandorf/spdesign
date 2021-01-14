context("Remove and replace")

test_that("Remove white spaces does exactly that", {
  expect_equal(remove_whitespace("   x _1"), "x_1")
  expect_equal(remove_whitespace("x_1"), "x_1")
  expect_equal(remove_whitespace("x_1  "), "x_1")
  expect_equal(remove_whitespace("x_ 1  "), "x_1")
})

test_that("Remove square brakcets returns a string without the expressions inside square brackets", {
  expect_equal(remove_square_bracket("beta[0.1]"), "beta")
  expect_equal(remove_square_bracket("beta[0.1] / x_1[2^5]"), "beta / x_1")
  expect_equal(remove_square_bracket("beta[N(N(0, 1), 1)]"), "beta")
  expect_equal(remove_square_bracket("beta     [0.1]"), "beta     ")
})
