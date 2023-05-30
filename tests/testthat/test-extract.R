context("Correctly extracts elements from the utility functions")

test_that("All names are extracted correctly", {
  expect_equal(extract_all_names("b_x[2]", TRUE), "b_x")
  expect_equal(extract_all_names("b_x[normal(0, 1)]", TRUE), "b_x")
  expect_true(all(extract_all_names("b_x[normal(0, 1)] * x[1:5]", TRUE) == c("b_x", "x")))
  expect_equal(extract_all_names("b_x[2*d]", TRUE), "b_x")
  expect_true(all(extract_all_names("b_x[2*d] / x_1[normal(0, 1)] ^ b_x_2", TRUE) == c("b_x", "x_1", "b_x_2")))
})

test_that("Parameter names are extracted correctly", {
  expect_true(length(extract_param_names("x1", TRUE)) == 0)
  expect_equal(extract_param_names("b_x1", TRUE), "b_x1")
  expect_true(all(extract_param_names("b_x1[0.1] * x_1 + b_x2[seq(0, 1, 0.1)] * x_2[1]", TRUE) == c("b_x1", "b_x2")))
})

test_that("Attribute names are extracted correctly", {
  expect_true(length(extract_attribute_names("b_x1", TRUE)) == 0)
  expect_equal(extract_attribute_names("x1", TRUE), "x1")
  expect_true(all(extract_attribute_names("b_x1[0.1] * x_1 + b_x2[seq(0, 1, 0.1)] * x_2[1]", TRUE) == c("x_1", "x_2")))
})

test_that("Value arguments are extracted correctly", {
  expect_equal(extract_values("b_x[1]", TRUE), "1")
  expect_equal(extract_values("b_x[1*0.2]", TRUE), "1*0.2")
  expect_equal(extract_values("b_x[1*b_x_3]", TRUE), "1*b_x_3")
  expect_equal(extract_values("b_x[normal(0, 1)]", TRUE), "normal(0, 1)")
  expect_true(all(extract_values("b_x[normal(0, 1)] * x[0.1]", TRUE) == c("normal(0, 1)", "0.1")))
  expect_true(all(extract_values("b_x[normal(0, 1)] * x[0.1] + alpha[0.2*45/2]", TRUE) == c("normal(0, 1)", "0.1", "0.2*45/2")))
})

test_that("Extract specified only extracts parameters and attributes with specified priors and levels", {
  expect_true(all(extract_specified("b_x0[0.1] * x + b_x2[normal(0, 1)] * x_3[seq(0, 1, 0.1)] / b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[normal(0,1)]", "x_3[seq(0,1,0.1)]", "b_y[1+2]")))
  expect_true(all(extract_specified("b_x0[0.1] * x+ b_x2[normal(0, 1)] * x_3[seq(0, 1, 0.1)] / b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[normal(0,1)]", "x_3[seq(0,1,0.1)]", "b_y[1+2]")))
  expect_true(all(extract_specified("b_x0[0.1]*x+ b_x2[normal(0, 1)] *x_3[seq(0, 1, 0.1)] /b_y[1+2]", TRUE) == c("b_x0[0.1]", "b_x2[normal(0,1)]", "x_3[seq(0,1,0.1)]", "b_y[1+2]")))
})

test_that("Extract named values does that correctly", {
  expect_identical(
    extract_named_values("b_x1[0.1] * x_1 + b_x2 * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]"),
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
    extract_named_values("b_x1[0.1] * x_1 + b_x2 * x_2[1:3] + b_x3[normal(0, 1)] * x_2[seq(0, 1, 0.25)]"),
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
  expect_identical(
    extract_named_values(
      V <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      )
    ),
    list(
      b_x1 = 0.1,
      x_2 = 1:3,
      b_x3 = list(
        mu = 0,
        sigma = 1
      ),
      x_3 = seq(0, 1, 0.25),
      x_1 = 2:5,
      b_x2 = 0.4
    )
  )
})

test_that("Attribute level balance frequencies are extracted", {
  expect_equal(
    extract_level_occurrence(
      "b_x1[normal_p(0, 1)] + x1[seq(0, 1, 0.25)](1, 2, 3, 4, 5) + b_x2[0.1] * x2[1:4](1, 2, 3, 4)",
      TRUE
    ),
    c(
      "(1, 2, 3, 4, 5)",
      "(1, 2, 3, 4)"
    )
  )
  expect_equal(
    extract_level_occurrence(
      "b_x1[normal_p(0, 1)] + x1[seq(0, 1, 0.25)](1, 2, 3:5, 4, 5) + b_x2[0.1] * x2[1:4](4)",
      TRUE
    ),
    c(
      "(1, 2, 3:5, 4, 5)",
      "(4)"
    )
  )
})

test_that("Distributions are extracted correctly", {
  expect_equal(extract_distribution("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]", "prior"), c(b_x2 = "uniform", b_x3 = "normal"))
  expect_equal(extract_distribution(list("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]",
                                         "b_x1[0.1] * x_1[2:5] + b_x2 * x_2[c(0, 1)] + b_x3 * x_3[seq(0, 1, 0.25)]"), "prior"), c(b_x2 = "uniform", b_x3 = "normal"))
  expect_equal(extract_distribution(list("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3 * x_3[seq(0, 1, 0.25)]",
                                         "b_x1[0.1] * x_1[2:5] + b_x2 * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]"), "prior"), c(b_x2 = "uniform", b_x3 = "normal"))
  expect_equal(extract_distribution(list("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]",
                                         "b_x1[0.1] * x_1[2:5] + b_x2 * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]"), "prior"), c(b_x2 = "uniform", b_x3 = "normal"))
  expect_equal(extract_distribution(list("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]",
                                         "b_x1[0.1] * x_1[2:5] + b_x2 * x_2[c(0, 1)] + b_x3[triangular_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]"), "prior"), c(b_x2 = "uniform", b_x3 = "normal"))
  expect_equal(extract_distribution("b_x1[0.1] * x_1[2:5] + b_x2[0.2] * x_2[c(0, 1)] + b_x3[triangular_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]", "prior"), c(b_x3 = "triangular"))
  expect_equal(extract_distribution("b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(-0.2, 0.2)] * x_3[seq(0, 1, 0.25)]", "param"), NA)
})
