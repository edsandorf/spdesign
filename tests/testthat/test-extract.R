context("Correctly extracts elements from the utility functions")

test_that("Name arguments are extracted correctly", {

})

test_that("Value arguments are extracted correctly", {
  expect_equal(extract_value_args("beta[1]", TRUE), "1")
  expect_equal(extract_value_args("beta[1*0.2]", TRUE), "1*0.2")
  expect_equal(extract_value_args("beta[1*beta_3]", TRUE), "1*beta_3")
  expect_equal(extract_value_args("beta[N(0, 1)]", TRUE), "N(0, 1)")
  expect_true(all(extract_value_args("beta[N(0, 1)] * x[0.1]", TRUE) == c("N(0, 1)", "0.1")))
  expect_true(all(extract_value_args("beta[N(0, 1)] * x[0.1] + alpha[0.2*45/2]", TRUE) == c("N(0, 1)", "0.1", "0.2*45/2")))
})

test_that("Distributions are extracted correctly", {

})



test_that("Extract name arguments is robust to a variety of functional forms", {
  expect_equal(extract_name_args(c("x_1|n"), simplify = TRUE), "x_1")
  expect_true(all(extract_name_args(c("x_1|n", "x_2"), simplify = TRUE) ==  c("x_1", "x_2")))
  # expect_error(extract_names("|test")) # No name specified
})


test_that("Extract distribution correctly retrieves the distributions", {
  expect_equal(extract_distribution(c("x_1|c(N(0, 1))")), "N")
  expect_true(all(extract_distribution(c("x_1|c(N(0, LN(1, 1)))")) == c("N", "LN")))
  expect_true(all(extract_distribution(list("x_1|c(N(0, LN(1, 1)))", c("TR(0, 1)"))) == c("N", "LN", "TR")))
})
