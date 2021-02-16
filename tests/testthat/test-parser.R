context("Correctly parses the list of utility functions")

opts <- list(
  level_balance = FALSE,
  tasks = 6
)

test_that("Error messages are triggered correctly", {
  expect_error(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2 + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      ),
      opts
    )
  )
})

test_that("Warning messages are triggered correctly", {
  expect_warning(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1[6]      * x_1[2:5] + b_x2[0.4] * x_2[1:6]      + b_x3          * x_3"
      ),
      opts
    )
  )
})

test_that("The utility parser correctly splits and returns attributes and parameters", {
  expect_identical(
    suppressMessages(
      parse_utility(
        utility <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
        ),
        opts
      )
    ),
    list(utility = list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
                        alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3"),
         param = list(b_x1 = 0.1, b_x3 = list(mu = 0, sigma = 1),
                      b_x2 = 0.4), attrs = list(alt1_x_2 = 1:3, alt1_x_3 = c(0,
                                                                             0.25, 0.5, 0.75, 1), alt1_x_1 = 2:5, alt2_x_2 = 1:3, alt2_x_3 = c(0,
                                                                                                                                               0.25, 0.5, 0.75, 1), alt2_x_1 = 2:5), level_occurrence = list(
                                                                                                                                                 alt1_x_2 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2), alt1_x_3 = list(
                                                                                                                                                   lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                                                                 alt1_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2),
                                                                                                                                                 alt2_x_2 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2), alt2_x_3 = list(
                                                                                                                                                   lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                                                                 alt2_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2)))
    )

  # With alternative specific attributes
  expect_identical(
    suppressMessages(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3 + b_x4[0.001] * x_4[c(2, 4, 6)]"
      ),
      opts
    )),
    list(utility = list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
                        alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3 + b_x4 * alt2_x_4"),
         param = list(b_x1 = 0.1, b_x3 = list(mu = 0, sigma = 1),
                      b_x2 = 0.4, b_x4 = 0.001), attrs = list(alt1_x_2 = 1:3,
                                                              alt1_x_3 = c(0, 0.25, 0.5, 0.75, 1), alt1_x_1 = 2:5,
                                                              alt1_x_4 = 0, alt2_x_2 = 1:3, alt2_x_3 = c(0, 0.25, 0.5,
                                                                                                         0.75, 1), alt2_x_1 = 2:5, alt2_x_4 = c(2, 4, 6)), level_occurrence = list(
                                                                                                           alt1_x_2 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2), alt1_x_3 = list(
                                                                                                             lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                           alt1_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2),
                                                                                                           alt1_x_4 = list(lvl1 = 6), alt2_x_2 = list(lvl1 = 2,
                                                                                                                                                      lvl2 = 2, lvl3 = 2), alt2_x_3 = list(lvl1 = 1:2,
                                                                                                                                                                                           lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                           alt2_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2),
                                                                                                           alt2_x_4 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2))))


  # With alternative specific attributes and one utility function re-ordered
  expect_identical(
    suppressMessages(
    parse_utility(
      utility <- list(
        alt1 = "b_x2      * x_2[1:3] +  b_x1[0.1] * x_1      + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3 + b_x4[0.001] * x_4[c(2, 4, 6)]"
      ),
      opts
    )),
    list(utility = list(alt1 = "b_x2 * alt1_x_2 + b_x1 * alt1_x_1 + b_x3 * alt1_x_3",
                        alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3 + b_x4 * alt2_x_4"),
         param = list(b_x1 = 0.1, b_x3 = list(mu = 0, sigma = 1),
                      b_x2 = 0.4, b_x4 = 0.001), attrs = list(alt1_x_2 = 1:3,
                                                              alt1_x_3 = c(0, 0.25, 0.5, 0.75, 1), alt1_x_1 = 2:5,
                                                              alt1_x_4 = 0, alt2_x_2 = 1:3, alt2_x_3 = c(0, 0.25, 0.5,
                                                                                                         0.75, 1), alt2_x_1 = 2:5, alt2_x_4 = c(2, 4, 6)), level_occurrence = list(
                                                                                                           alt1_x_2 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2), alt1_x_3 = list(
                                                                                                             lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                           alt1_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2),
                                                                                                           alt1_x_4 = list(lvl1 = 6), alt2_x_2 = list(lvl1 = 2,
                                                                                                                                                      lvl2 = 2, lvl3 = 2), alt2_x_3 = list(lvl1 = 1:2,
                                                                                                                                                                                           lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2),
                                                                                                           alt2_x_1 = list(lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2),
                                                                                                           alt2_x_4 = list(lvl1 = 2, lvl2 = 2, lvl3 = 2))))

  # Testing multiple duplicates
  expect_identical(
    suppressMessages(
    suppressWarnings(
      parse_utility(
        utility <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[uniform_p(-1, 1)]      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        ),
        opts
      )
    )),
    list(utility = list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + b_x3 * alt1_x_3",
                        alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3"),
         param = list(b_x1 = 0.1, b_x2 = list(mu = -1, sigma = 1),
                      b_x3 = list(mu = 0, sigma = 1)), attrs = list(alt1_x_2 = 1:3,
                                                                    alt1_x_3 = c(0, 0.25, 0.5, 0.75, 1), alt1_x_1 = 2:5,
                                                                    alt2_x_2 = 1:3, alt2_x_3 = c(0, 0.25, 0.5, 0.75, 1),
                                                                    alt2_x_1 = 2:5), level_occurrence = list(alt1_x_2 = list(
                                                                      lvl1 = 2, lvl2 = 2, lvl3 = 2), alt1_x_3 = list(lvl1 = 1:2,
                                                                                                                     lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2), alt1_x_1 = list(
                                                                                                                       lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2), alt2_x_2 = list(
                                                                                                                         lvl1 = 2, lvl2 = 2, lvl3 = 2), alt2_x_3 = list(lvl1 = 1:2,
                                                                                                                                                                        lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2), alt2_x_1 = list(
                                                                                                                                                                          lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2))))

  # Testing multiple duplicates and reverse order fo some attributes and parameters
  expect_identical(
    suppressMessages(
    suppressWarnings(
      parse_utility(
        utility <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[uniform_p(-1, 1)]      * x_2[1:3] + x_3[seq(0, 1, 0.25)] * b_x3[normal(0, 1)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        ),
        opts
      )
    )),
    list(utility = list(alt1 = "b_x1 * alt1_x_1 + b_x2 * alt1_x_2 + alt1_x_3 * b_x3",
                        alt2 = "b_x1 * alt2_x_1 + b_x2 * alt2_x_2 + b_x3 * alt2_x_3"),
         param = list(b_x1 = 0.1, b_x2 = list(mu = -1, sigma = 1),
                      b_x3 = list(mu = 0, sigma = 1)), attrs = list(alt1_x_2 = 1:3,
                                                                    alt1_x_3 = c(0, 0.25, 0.5, 0.75, 1), alt1_x_1 = 2:5,
                                                                    alt2_x_2 = 1:3, alt2_x_3 = c(0, 0.25, 0.5, 0.75, 1),
                                                                    alt2_x_1 = 2:5), level_occurrence = list(alt1_x_2 = list(
                                                                      lvl1 = 2, lvl2 = 2, lvl3 = 2), alt1_x_3 = list(lvl1 = 1:2,
                                                                                                                     lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2), alt1_x_1 = list(
                                                                                                                       lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2), alt2_x_2 = list(
                                                                                                                         lvl1 = 2, lvl2 = 2, lvl3 = 2), alt2_x_3 = list(lvl1 = 1:2,
                                                                                                                                                                        lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2, lvl5 = 1:2), alt2_x_1 = list(
                                                                                                                                                                          lvl1 = 1:2, lvl2 = 1:2, lvl3 = 1:2, lvl4 = 1:2))))
})
