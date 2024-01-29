context("Test the function for creating level occurrence lookup tables")

test_that("Test that we still get a list when simplification is possible. ", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0, 0)] * x1[1:3] + b_x2_dummy[c(0, 0)] * x2[1:3] + b_x3_dummy[c(0, 0)] * x3[1:3]",
    alt2 = "b_x1_dummy          * x1      + b_x2_dummy          * x2      + b_x3_dummy          * x3"
  )


  expect_equal(
    suppressWarnings(
      lvl_occurrences(utility, 10, TRUE)
    ),
    list(alt1_x1 = c(`1` = 3, `2` = 3, `3` = 3), alt1_x2 = c(`1` = 3,
                                                             `2` = 3, `3` = 3), alt1_x3 = c(`1` = 3, `2` = 3, `3` = 3), alt2_x1 = c(`1` = 3,
                                                                                                                                    `2` = 3, `3` = 3), alt2_x2 = c(`1` = 3, `2` = 3, `3` = 3), alt2_x3 = c(`1` = 3,
                                                                                                                                                                                                           `2` = 3, `3` = 3)))
})

test_that("Test that we still get a list when simplification is not possible. ", {
  utility <- list(
    alt1 = "b_x1_dummy[c(0, 0)] * x1[1:3] + b_x2_dummy[c(0)] * x2[1:2] + b_x3_dummy[c(0, 0)] * x3[1:3]",
    alt2 = "b_x1_dummy          * x1      + b_x2_dummy          * x2      + b_x3_dummy          * x3"
  )


  expect_equal(
    suppressWarnings(
      lvl_occurrences(utility, 10, TRUE)
    ),
    list(alt1_x1 = c(`1` = 3, `2` = 3, `3` = 3), alt1_x2 = c(`1` = 5,
                                                             `2` = 5), alt1_x3 = c(`1` = 3, `2` = 3, `3` = 3), alt2_x1 = c(`1` = 3,
                                                                                                                           `2` = 3, `3` = 3), alt2_x2 = c(`1` = 5, `2` = 5), alt2_x3 = c(`1` = 3,
                                                                                                                                                                                         `2` = 3, `3` = 3)))
})
