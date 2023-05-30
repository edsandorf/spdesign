context("Test the function getting level occurrence")

test_that("Test 1", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[2:5]  + b_x2[0.4] * x2[c(0, 1)]+ b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x1             + b_x3          * x3"
  )

  rows <- 12

  expect_equal(
    suppressWarnings(
      occurrences(utility, rows)
    ),
    list(alt1_x1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3),
         alt1_x2 = list(lvl1 = 6, lvl2 = 6), alt1_x3 = list(lvl1 = 2:3,
                                                            lvl2 = 2:3, lvl3 = 2:3, lvl4 = 2:3, lvl5 = 2:3), alt2_x1 = list(
                                                              lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3), alt2_x2 = list(
                                                                lvl1 = 12), alt2_x3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3,
                                                                                           lvl4 = 2:3, lvl5 = 2:3))
  )
})


test_that("Test 2", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5]  +  b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x2[0.4] * x_2[c(0, 1)]",
    alt2 = "b_x1      * x_1             + b_x3          * x_3"
  )

  rows <- 12

  expect_equal(
    suppressWarnings(
      occurrences(utility, rows)
    ),
    list(alt1_x_1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3),
         alt1_x_3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3, lvl4 = 2:3,
                         lvl5 = 2:3), alt1_x_2 = list(lvl1 = 6, lvl2 = 6), alt2_x_1 = list(
                           lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3), alt2_x_3 = list(
                             lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3, lvl4 = 2:3, lvl5 = 2:3),
         alt2_x_2 = list(lvl1 = 12))
  )
})


test_that("Test 3", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5]  +  b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x2[0.4] * x_2",
    alt2 = "b_x1      * x_1             + b_x3          * x_3 + b_x2 * x_2[c(0, 1)]"
  )


  rows <- 12

  expect_equal(
    suppressWarnings(
      occurrences(utility, rows)
    ),
    list(alt1_x_1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3),
         alt1_x_3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3, lvl4 = 2:3,
                         lvl5 = 2:3), alt1_x_2 = list(lvl1 = 6, lvl2 = 6), alt2_x_1 = list(
                           lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3), alt2_x_3 = list(
                             lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3, lvl4 = 2:3, lvl5 = 2:3),
         alt2_x_2 = list(lvl1 = 6, lvl2 = 6))
  )
})

