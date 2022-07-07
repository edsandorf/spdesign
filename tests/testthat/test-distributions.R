# context("Test the transformation of distributions")
#
# eta <- make_draws(1, 2, 1, 123, "scrambled-sobol")
#
# test_that("Normal distribution", {
#   expect_equal(
#     transform_distribution(0, 1, eta, "normal"),
#     structure(
#       c(0.902109721985119, -0.239394921333748),
#       .Dim = 2:1
#     )
#   )
# })
#
# test_that("Lognormal distribution", {
#   expect_equal(
#     transform_distribution(0, 1, eta, "lognormal"),
#     structure(
#       c(2.46479766752339, 0.787103976832784),
#       .Dim = 2:1
#     )
#   )
# })
#
# test_that("Normal distribution", {
#   expect_equal(
#     transform_distribution(0, 1, eta, "uniform"),
#     structure(
#       c(0.63300141505897, -0.189200632274151),
#       .Dim = 2:1
#     )
#   )
# })
#
# test_that("Normal distribution", {
#   expect_equal(
#     transform_distribution(0, 1, eta, "triangular"),
#     structure(
#       c(0.394195918682426, -0.0995560163309163),
#       .Dim = 2:1
#     )
#   )
# })
