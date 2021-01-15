context("Correctly extracts elements from the utility functions")

test_that("Name arguments are extracted correctly", {
  expect_equal(extract_name_args("b_x[2]", TRUE), "b_x")
  expect_equal(extract_name_args("b_x[N(0, 1)]", TRUE), "b_x")
  expect_true(all(extract_name_args("b_x[N(0, 1)] * x[1:5]", TRUE) == c("b_x ", " x")))
  expect_equal(extract_name_args("b_x[2*d]", TRUE), "b_x")
  expect_true(all(extract_name_args("b_x[2*d] / x_1[N(0, 1)] ^ b_x_2", TRUE) == c("b_x ", " x_1 ", " b_x_2")))
})

test_that("Value arguments are extracted correctly", {
  expect_equal(extract_value_args("b_x[1]", TRUE), "1")
  expect_equal(extract_value_args("b_x[1*0.2]", TRUE), "1*0.2")
  expect_equal(extract_value_args("b_x[1*b_x_3]", TRUE), "1*b_x_3")
  expect_equal(extract_value_args("b_x[N(0, 1)]", TRUE), "N(0, 1)")
  expect_true(all(extract_value_args("b_x[N(0, 1)] * x[0.1]", TRUE) == c("N(0, 1)", "0.1")))
  expect_true(all(extract_value_args("b_x[N(0, 1)] * x[0.1] + alpha[0.2*45/2]", TRUE) == c("N(0, 1)", "0.1", "0.2*45/2")))
})

test_that("Extract specified only extracts parameters and attributes with specified priors and levels", {
  expect_true(all(extract_specified("b_x0[0.1] * x + b_x2[N(0, 1)] * x_3[seq(0, 1, 0.1)] / b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[N(0, 1)]", "x_3[seq(0, 1, 0.1)]", "b_y[1+2]")))
  expect_true(all(extract_specified("b_x0[0.1] * x+ b_x2[N(0, 1)] * x_3[seq(0, 1, 0.1)] / b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[N(0, 1)]", "x_3[seq(0, 1, 0.1)]", "b_y[1+2]")))
  expect_true(all(extract_specified("b_x0[0.1]*x+ b_x2[N(0, 1)] *x_3[seq(0, 1, 0.1)] /b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[N(0, 1)]", "x_3[seq(0, 1, 0.1)]", "b_y[1+2]")))
})

test_that("Extract named values does that correctly", {
  expect_identical(
    extract_named_values("b_x1[0.1] * x_1 + b_x2 * x_2[1:3] + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]"),
    list(
      b_x1 = 0.1,
      x_2 = 1:3,
      b_x3 = list(
        mu = 0,
        sigma = 1
      ),
      x_3 = seq(0, 1, 0.25)
    )
  )
  expect_identical(
    extract_named_values("b_x1[0.1] * x_1 + b_x2 * x_2[1:3] + b_x3[N(0, 1)] * x_2[seq(0, 1, 0.25)]"),
    list(
      b_x1 = 0.1,
      x_2 = 1:3,
      b_x3 = list(
        mu = 0,
        sigma = 1
      ),
      x_2 = seq(0, 1, 0.25)
    )
  )
  extract_named_values("b_x1[0.1] * x_1 ++ b_x2 */* x_2[1:3] + b_x3[N(0, 1)] * x_2[seq(0, 1, 0.25)]")
})

test_that("Distributions are extracted correctly", {
  expect_equal(extract_distribution(c("x_1[c(N(0, 1))]")), "N")
  expect_true(all(extract_distribution(c("x_1[c(N(0, LN(1, 1)))]")) == c("N", "LN")))
  expect_true(all(extract_distribution(list("x_1[c(N(0, LN(1, 1)))]", c("TR(0, 1)"))) == c("N", "LN", "TR")))
})
