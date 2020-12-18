context("Utility parsers")

test_that("Remove white spaces does exactly that", {
  expect_equal(remove_whitespace("   x _1"), "x_1")
  expect_equal(remove_whitespace("x_1"), "x_1")
  expect_equal(remove_whitespace("x_1  "), "x_1")
  expect_equal(remove_whitespace("x_ 1  "), "x_1")

})

test_that("Extract name arguments is robust to a variety of functional forms", {
  expect_equal(extract_name_args(c("x_1|n"), simplify = TRUE), "x_1")
  expect_true(all(extract_name_args(c("x_1|n", "x_2"), simplify = TRUE) ==  c("x_1", "x_2")))
  # expect_error(extract_names("|test")) # No name specified
})

test_that("Extract params returns a named vector", {
  expect_true(extract_value_args("beta_1 | c(0.1)", simplify = TRUE) == " c(0.1)")
})

test_that("Extract distribution correctly retrieves the distributions", {
  expect_equal(extract_distribution(c("x_1|c(N(0, 1))")), "N")
  expect_true(all(extract_distribution(c("x_1|c(N(0, LN(1, 1)))")) == c("N", "LN")))
  expect_true(all(extract_distribution(list("x_1|c(N(0, LN(1, 1)))", c("TR(0, 1)"))) == c("N", "LN", "TR")))
})
