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
    list(alt1_x1 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt1_x2 = list(lvl1 = c(0,
                                                                                                                                                      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3,
                                                                                                                                                                                                       4, 5, 6, 7, 8, 9, 10, 11, 12)), alt1_x3 = list(lvl1 = c(0, 1,
                                                                                                                                                                                                                                                               2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3, 4,
                                                                                                                                                                                                                                                                                                             5, 6, 7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7,
                                                                                                                                                                                                                                                                                                                                                  8, 9, 10, 11, 12), lvl4 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                                                                                                                                                                                                                                                                                                                                              11, 12), lvl5 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
         alt2_x1 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                                 ), lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                                 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt2_x2 = list(lvl1 = c(0,
                                                                                                                                                                  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt2_x3 = list(lvl1 = c(0,
                                                                                                                                                                                                                                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2,
                                                                                                                                                                                                                                                                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2, 3, 4,
                                                                                                                                                                                                                                                                                                                               5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0, 1, 2, 3, 4, 5, 6,
                                                                                                                                                                                                                                                                                                                                                                    7, 8, 9, 10, 11, 12), lvl5 = c(0, 1, 2, 3, 4, 5, 6, 7, 8,
                                                                                                                                                                                                                                                                                                                                                                                                   9, 10, 11, 12)))
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
    list(alt1_x_1 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                  11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt1_x_3 = list(
                                                                                        lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0,
                                                                                                                                                     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2,
                                                                                                                                                                                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0, 1, 2, 3, 4,
                                                                                                                                                                                                                                                 5, 6, 7, 8, 9, 10, 11, 12), lvl5 = c(0, 1, 2, 3, 4, 5, 6,
                                                                                                                                                                                                                                                                                      7, 8, 9, 10, 11, 12)), alt1_x_2 = list(lvl1 = c(0, 1, 2,
                                                                                                                                                                                                                                                                                                                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5,
                                                                                                                                                                                                                                                                                                                                                                                 6, 7, 8, 9, 10, 11, 12)), alt2_x_1 = list(lvl1 = c(0, 1, 2, 3,
                                                                                                                                                                                                                                                                                                                                                                                                                                    4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           10, 11, 12), lvl4 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           )), alt2_x_3 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl5 = c(0, 1, 2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt2_x_2 = list(lvl1 = c(0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
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
    list(alt1_x_1 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                  11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt1_x_3 = list(
                                                                                        lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0,
                                                                                                                                                     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2,
                                                                                                                                                                                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0, 1, 2, 3, 4,
                                                                                                                                                                                                                                                 5, 6, 7, 8, 9, 10, 11, 12), lvl5 = c(0, 1, 2, 3, 4, 5, 6,
                                                                                                                                                                                                                                                                                      7, 8, 9, 10, 11, 12)), alt1_x_2 = list(lvl1 = c(0, 1, 2,
                                                                                                                                                                                                                                                                                                                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5,
                                                                                                                                                                                                                                                                                                                                                                                 6, 7, 8, 9, 10, 11, 12)), alt2_x_1 = list(lvl1 = c(0, 1, 2, 3,
                                                                                                                                                                                                                                                                                                                                                                                                                                    4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            7, 8, 9, 10, 11, 12), lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           10, 11, 12), lvl4 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           )), alt2_x_3 = list(lvl1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        11, 12), lvl2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               lvl3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl4 = c(0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl5 = c(0, 1, 2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), alt2_x_2 = list(lvl1 = c(0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), lvl2 = c(0, 1, 2, 3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          4, 5, 6, 7, 8, 9, 10, 11, 12)))
  )
})


test_that("Level occurrences only apply to alternatives with the attribute", {
  # bus does not have x1, but x10 and b_x10 contain the text x1
  utility <- list(
    car = "b_x1[0.1] * x1[1:3](4:8) + b_cost[-0.2] * cost[c(5, 10)]",
    bus = "b_bus[0.1] * bus[1] + b_x10[0.2] * x10[1:2] + b_cost * cost"
  )

  o <- occurrences(utility, 12)
  expect_equal(o$car_x1$lvl1, 4:8)
  expect_equal(o$bus_x1$lvl1, c(0, seq_len(12)))

  # A status quo parameter b_sq contains the attribute name b
  utility <- list(
    sq = "b_sq[0.2] * sq[1]",
    alt1 = "b_a[0.1] * a[1:2] + b_b[0.2] * b[1:3](3:5)",
    alt2 = "b_a * a + b_b * b"
  )

  o <- occurrences(utility, 12)
  expect_equal(o$alt1_b$lvl1, 3:5)
  expect_equal(o$sq_b$lvl1, c(0, seq_len(12)))
})

test_that("A single range applies to every level of the attribute", {
  # x10 is listed before x1 and has fewer levels
  utility <- list(
    alt1 = "b_x10[0.2] * x10[1:2] + b_x1[0.1] * x1[1:3](2:6)",
    alt2 = "b_x10      * x10      + b_x1      * x1"
  )

  expect_length(occurrences(utility, 12)$alt1_x1, 3)
})
