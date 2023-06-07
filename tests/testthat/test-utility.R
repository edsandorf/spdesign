context("Clean and update utility works as expected")

test_that("When nothing is dummy-coded both functions return the same utility functions ", {

  utility <- list(
    alt1 = "b_x1[0.1] * x_1[1:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )

})

test_that("When nothing is dummy coded both functions return the same utility functions in the presence of interaction terms", {

  utility <- list(
    alt1 = "b_x1[0.1] * x_1[1:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x1x2[-0.1] * I(x_1 * x_2)",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3 + b_x1x2x3[0.1] * I(x_1 * x_2 * x_3)"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3 + b_x1x2 * I(alt1_x_1 * alt1_x_2)",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3 + b_x1x2x3 * I(alt2_x_1 * alt2_x_2 * alt2_x_3)")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3 + b_x1x2 * I(alt1_x_1 * alt1_x_2)",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3 + b_x1x2x3 * I(alt2_x_1 * alt2_x_2 * alt2_x_3)")
  )
})

test_that("When nothing is dummy coded, but we have Bayesian priors", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(0, 1)] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )
})

test_that("Updates correctly with a single dummy coded attribute", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0.1, 0.2)] * x_1[c(1, 3, 5)] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1_dummy * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1_dummy * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x12 * alt1_x_13 + b_x13 * alt1_x_15 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x12 * alt2_x_13 + b_x13 * alt2_x_15 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3")
  )

})

test_that("When we have bayesian priors on dummy coded attributes with only 2 levels", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5] + b_x2_dummy[c(uniform_p(-1, 1), uniform_p(-1, 1))] * x_2[c(0, 1)] + b_x3[normal_p(0, 1)] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2_dummy      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x2_dummy * alt1_x_2 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x2_dummy * alt2_x_2 + b_x3 * alt2_x_3")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x1 * alt1_x_1 + b_x22 * alt1_x_21 + b_x3 * alt1_x_3",
         alt2 = "b_x1 * alt2_x_1 + b_x22 * alt2_x_21 + b_x3 * alt2_x_3")
  )
})

test_that("Updates correctly with multiple dummy coded attributes", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0.1, 0.2)] * x_1[c(1, 3, 5)] + b_x2[0.4] * x_2[c(0, 1)] + b_x3_dummy[c(-0.2, 0.1, 0.5, 0.2)] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3_dummy          * x_3"
  )

  expect_equal(
    clean_utility(utility),
    list(alt1 = "b_x1_dummy * alt1_x_1 + b_x2 * alt1_x_2 + b_x3_dummy * alt1_x_3",
         alt2 = "b_x1_dummy * alt2_x_1 + b_x2 * alt2_x_2 + b_x3_dummy * alt2_x_3")
  )

  expect_equal(
    update_utility(utility),
    list(alt1 = "b_x12 * alt1_x_13 + b_x13 * alt1_x_15 + b_x2 * alt1_x_2 + b_x32 * alt1_x_30alt1_.alt1_.5 + b_x33 * alt1_x_30alt1_.5 + b_x34 * alt1_x_30.alt1_.5 + b_x35 * alt1_x_31",
         alt2 = "b_x12 * alt2_x_13 + b_x13 * alt2_x_15 + b_x2 * alt2_x_2 + b_x32 * alt2_x_30alt2_.alt2_.5 + b_x33 * alt2_x_30alt2_.5 + b_x34 * alt2_x_30.alt2_.5 + b_x35 * alt2_x_31")
  )

})
