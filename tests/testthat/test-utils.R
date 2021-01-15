context("Test util functions")

test_that("is_balance returns TRUE/FALSE in correct cases", {
  expect_true(is_balanced("N()", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", ")"))
  expect_false(is_balanced("b_x1[Np(0, 1), Np(0, 1]", "(", ")"))
  expect_true(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "[", "]"))
  expect_warning(is_balanced("b_x1[Np(0, 1), Np(0, 1)]", "(", "]"))
})

test_that("cleaned utility is correctly returned", {
  expect_identical(
    lapply(
      list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      ),
      clean_utility
    ),
    list(
      alt1 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3",
      alt2 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3"
    )
  )
})
