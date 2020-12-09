context("Test that utility is split correctly")

test_that("Utility splits correctly into matrices", {
  utility_funcs <- list(
    alt_1 = "beta_1 | c(0.1) * x_1 | c(0, 1) + beta_2 | 0.1 * x_2 | c(0, 1)",
    alt_2 = "beta_1 * x_1 + beta_2 * x_2"
  )

  utility_mats <- lapply(utility_funcs, function(v) split_utility(v))
  expect_length(utility_mats, 2)
  dim_vec <- unlist(lapply(utility_mats, function(m) dim(m)))
  names(dim_vec) <- NULL
  expect_equal(dim_vec, c(2, 2, 2, 2))


})
