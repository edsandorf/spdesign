context("Test the function getting level occurrence")

test_that("Test 1", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5]  + b_x2[0.4] * x_2[c(0, 1)]+ b_x3[-0.2] * x_3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x_1             + b_x3          * x_3"
  )

  attrs <- list(
    x1 = 2:5,
    x2 = c(0, 1),
    x3 = seq(0, 1, 0.25)
  )

  candidate_rows <- 12

  expect_equal(
    suppressWarnings(
      get_level_occurrence(utility, attrs, candidate_rows)
    ),
    list(x1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3), x2 = list(
      lvl1 = 6, lvl2 = 6), x3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3,
                                     lvl4 = 2:3, lvl5 = 2:3))
  )
})


test_that("Test 2", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5]  +  b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x2[0.4] * x_2[c(0, 1)]",
    alt2 = "b_x1      * x_1             + b_x3          * x_3"
  )

  attrs <- list(
    x1 = 2:5,
    x3 = seq(0, 1, 0.25),
    x2 = c(0, 1)
  )

  candidate_rows <- 12

  expect_equal(
    suppressWarnings(
      get_level_occurrence(utility, attrs, candidate_rows)
    ),
    list(x1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3),
         x3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3,
                   lvl4 = 2:3, lvl5 = 2:3),
         x2 = list(    lvl1 = 6, lvl2 = 6))
  )
})


test_that("Test 3", {
  utility <- list(
    alt1 = "b_x1[0.1] * x_1[2:5]  +  b_x3[-0.2] * x_3[seq(0, 1, 0.25)] + b_x2[0.4] * x_2",
    alt2 = "b_x1      * x_1             + b_x3          * x_3 + b_x2 * x_2[c(0, 1)]"
  )

  attrs <- list(
    x1 = 2:5,
    x3 = seq(0, 1, 0.25),
    x2 = c(0, 1)
  )

  candidate_rows <- 12

  expect_equal(
    suppressWarnings(
      get_level_occurrence(utility, attrs, candidate_rows)
    ),
    list(x1 = list(lvl1 = 3, lvl2 = 3, lvl3 = 3, lvl4 = 3),
         x3 = list(lvl1 = 2:3, lvl2 = 2:3, lvl3 = 2:3,
                   lvl4 = 2:3, lvl5 = 2:3),
         x2 = list(    lvl1 = 6, lvl2 = 6))
  )
})

test_that("Test 4", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[2:5](3)  +  b_x3[-0.2] * x3[seq(0, 1, 0.25)] + b_x2[0.4] * x2",
    alt2 = "b_x1      * x1             + b_x3          * x3 + b_x2 * x2[c(0, 1)](1:4)"
  )

  attrs <- list(
    x1 = 2:5,
    x3 = seq(0, 1, 0.25),
    x2 = c(0, 1)
  )

  candidate_rows <- 12

  expect_error(
    suppressWarnings(
      get_level_occurrence(utility, attrs, candidate_rows)
    )
  )
})

test_that("Test 5", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[2:5](3,  3, 3)  +  b_x3[-0.2] * x3[seq(0, 1, 0.25)] + b_x2[0.4] * x2",
    alt2 = "b_x1      * x1             + b_x3          * x3 + b_x2 * x2[c(0, 1)](5:7, 4:8)"
  )

  attrs <- list(
    x1 = 2:5,
    x3 = seq(0, 1, 0.25),
    x2 = c(0, 1)
  )

  candidate_rows <- 12

  expect_error(
    suppressWarnings(
      get_level_occurrence(utility, attrs, candidate_rows)
    )
  )

})
