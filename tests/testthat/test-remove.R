context("Remove and replace")

test_that("Remove white spaces does exactly that", {
  expect_equal(
    remove_whitespace("   x _1"),
    "x_1"
  )
  expect_equal(
    remove_whitespace("x_1"),
    "x_1"
  )
  expect_equal(
    remove_whitespace("x_1  "),
    "x_1"
  )
  expect_equal(
    remove_whitespace("x_ 1  "),
    "x_1"
  )
})

test_that("Returns a string without the expressions inside square brackets", {
  expect_equal(
    remove_square_brackets("b_x[0.1]"),
    "b_x"
  )
  expect_equal(
    remove_square_brackets("b_x[0.1] / x_1[2^5]"),
    "b_x / x_1"
  )
  expect_equal(
    remove_square_brackets("b_x[normal(normal(0, 1), 1)]"),
    "b_x"
  )
  expect_equal(
    remove_square_brackets("b_x     [0.1]"),
    "b_x     ")
})


test_that("Returns a string without the expressions inside brackets", {
  expect_equal(
    remove_all_brackets("b_x[0.1](0.1)"),
    "b_x"
  )
  expect_equal(
    remove_all_brackets("b_x[0.1](0:5) / x_1[2^5](1, 2)"),
    "b_x / x_1"
  )
  expect_equal(
    remove_all_brackets("b_x[normal(normal(0, 1), 1)](0, 1)"),
    "b_x"
  )
  expect_equal(
    remove_all_brackets("b_x     [0.1]"),
    "b_x     ")

  expect_equal(
    remove_all_brackets("b_x[0.1] * x1[c(0, 1)] + b_x2[0.1] * I(x1)"),
    "b_x * x1 + b_x2 * I(x1)"
  )
})
