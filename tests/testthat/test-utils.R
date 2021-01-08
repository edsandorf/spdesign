context("Utils")

test_that("Remove white spaces does exactly that", {
  expect_equal(remove_whitespace("   x _1"), "x_1")
  expect_equal(remove_whitespace("x_1"), "x_1")
  expect_equal(remove_whitespace("x_1  "), "x_1")
  expect_equal(remove_whitespace("x_ 1  "), "x_1")
})

test_that("Sanitize utility returns a string without the expressions inside square brackets", {
  expect_equal(sanitize_utility("beta[0.1]"), "beta")
  expect_equal(sanitize_utility("beta[0.1] / x_1[2^5]"), "beta / x_1")
  expect_equal(sanitize_utility("beta[N(N(0, 1), 1)]"), "beta")
  expect_equal(sanitize_utility("beta     [0.1]"), "beta     ")
})
