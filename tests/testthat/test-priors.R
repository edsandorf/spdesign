context("Priors are correctly prepared")

draws <- "standard-halton"
R <- 5
seed <- 123

test_that("Normal priors are extracted", {

  utility <- list(
    alt1 = "b_x1[0.1] * x_1[1:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x1 = 0.1, b_x2 = 0.4, b_x3 = -0.2))
  )
})

test_that("Normal interaction priors are extracted", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[1:5] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x1x2[-0.1] * I(x_1 * x_2)",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3 + b_x1x2x3[0.1] * I(x_1 * x_2 * x_3)"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x1 = 0.1, b_x2 = 0.4, b_x3 = -0.2, b_x1x2 = -0.1, b_x1x2x3 = 0.1))
  )
})


test_that("Normal dummy priors are extracted", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0.1, 0.2)] * x_1[c(1, 3, 5)] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x2 = 0.4, b_x3 = -0.2, b_x12 = 0.1, b_x13 = 0.2))
  )

})


test_that("Normal dummy priors are extracted", {
  utility <- list(
    alt1 = "b_x1_dummy[c(-0.1, 0.2)] * x_1[c(1, 3, 5)] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x2 = 0.4, b_x3 = -0.2, b_x12 = -0.1, b_x13 = 0.2))
  )

})

test_that("Normal dummy priors are extracted", {
  utility <- list(
    alt1 = "b_x1_dummy[c(-0.1, 0.2)] * x_1[c(1, 3, 5)] + b_x2_dummy[c(0.3, 0.4)] * x_2[c(0, 1, 2)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x3 = -0.2, b_x12 = -0.1, b_x13 = 0.2, b_x22 = 0.3, b_x23 = 0.4))
  )

})

test_that("Normal dummy priors are extracted", {
  utility <- list(
    alt1 = "b_x1_dummy[c(-0.1, --0.2)] * x_1[c(1, 3, 5)] + b_x2[0.4] * x_2[c(0, 1)] + b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1_dummy      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x2 = 0.4, b_x3 = -0.2, b_x12 = -0.1, b_x13 = -0.2))
  )

})

test_that("Normal bayesian priors are extracted", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5] + b_x2[uniform_p(-1, 1)] * x_2[c(0, 1)] + b_x3[normal_p(0, 1)] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x1 = 0.1, b_x2 = -1, b_x3 = -0.430727299295457), c(b_x1 = 0.1,
                                                                b_x2 = -1.5, b_x3 = 0.430727299295457), c(b_x1 = 0.1, b_x2 = -0.5,
                                                                                                          b_x3 = -1.22064034884735), c(b_x1 = 0.1, b_x2 = -1.75, b_x3 = -0.139710298881862
                                                                                                          ), c(b_x1 = 0.1, b_x2 = -0.75, b_x3 = 0.764709673786387))
  )
})


test_that("Normal bayesian dummy priors are extracted", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5] + b_x2_dummy[c(uniform_p(-1, 1), uniform_p(-1, 1))] * x_2[c(0, 1)] + b_x3[normal_p(0, 1)] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1      + b_x2_dummy      * x_2          + b_x3          * x_3"
  )

  expect_equal(
    prepare_priors(utility, draws, R),
    list(c(b_x1 = 0.1, b_x3 = 0, b_x22 = -1.33333333333333, b_x23 = -1.6
    ), c(b_x1 = 0.1, b_x3 = -0.674489750196082, b_x22 = -0.666666666666667,
         b_x23 = -1.2), c(b_x1 = 0.1, b_x3 = 0.674489750196082, b_x22 = -1.77777777777778,
                          b_x23 = -0.8), c(b_x1 = 0.1, b_x3 = -1.15034938037601, b_x22 = -1.11111111111111,
                                           b_x23 = -0.4), c(b_x1 = 0.1, b_x3 = 0.318639363964375, b_x22 = -0.444444444444445,
                                                            b_x23 = -1.92))
  )
})

