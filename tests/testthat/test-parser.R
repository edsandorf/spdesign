context("Correctly parses the list of utility functions")

test_that("Error messages are triggered correctly", {
  expect_error(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2 + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      )
    )
  )
})

test_that("Warning messages are triggered correctly", {
  expect_warning(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1[6]      * x_1[2:5] + b_x2[0.4] * x_2[1:6]      + b_x3          * x_3"
      )
    )
  )
})

test_that("The utility parser correctly splits and returns attributes and parameters", {
  expect_identical(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      )
    ),
    list(
      utility = list(
        alt1 = "b_x1 * x_1_1 + b_x2 * x_2_1 + b_x3 * x_3_1",
        alt2 = "b_x1 * x_1_2 + b_x2 * x_2_2 + b_x3 * x_3_2"
      ),
      param = list(
        b_x1 = 0.1,
        b_x3 = list(
          mu = 0,
          sigma = 1
        ),
        b_x2 = 0.4
      ),
      attrs = list(
        x_2 = 1:3,
        x_3 = seq(0, 1, 0.25),
        x_1 = 2:5
      )
    )
  )

  # With alternative specific attributes
  expect_identical(
    parse_utility(
      utility <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3 + b_x4[0.001] * x_4[c(2, 4, 6)]"
      )
    ),
    list(
      utility = list(
        alt1 = "b_x1 * x_1_1 + b_x2 * x_2_1 + b_x3 * x_3_1",
        alt2 = "b_x1 * x_1_2 + b_x2 * x_2_2 + b_x3 * x_3_2 + b_x4 * x_4_2"
      ),
      param = list(
        b_x1 = 0.1,
        b_x3 = list(
          mu = 0,
          sigma = 1
        ),
        b_x2 = 0.4,
        b_x4 = 0.001
      ),
      attrs = list(
        x_2 = 1:3,
        x_3 = seq(0, 1, 0.25),
        x_1 = 2:5,
        x_4 = c(2, 4, 6)
      )
    )
  )


  # With alternative specific attributes and one utility function re-ordered
  expect_identical(
    parse_utility(
      utility <- list(
        alt1 = "b_x2      * x_2[1:3] +  b_x1[0.1] * x_1      + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3 + b_x4[0.001] * x_4[c(2, 4, 6)]"
      )
    ),
    list(
      utility = list(
        alt1 = "b_x2 * x_2_1 + b_x1 * x_1_1 + b_x3 * x_3_1",
        alt2 = "b_x1 * x_1_2 + b_x2 * x_2_2 + b_x3 * x_3_2 + b_x4 * x_4_2"
      ),
      param = list(
        b_x1 = 0.1,
        b_x3 = list(
          mu = 0,
          sigma = 1
        ),
        b_x2 = 0.4,
        b_x4 = 0.001
      ),
      attrs = list(
        x_2 = 1:3,
        x_3 = seq(0, 1, 0.25),
        x_1 = 2:5,
        x_4 = c(2, 4, 6)
      )
    )
  )

  # Testing multiple duplicates
  expect_identical(
    suppressWarnings(
      parse_utility(
        utility <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[uniform_p(-1, 1)]      * x_2[1:3] + b_x3[normal(0, 1)] * x_3[seq(0, 1, 0.25)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        )
      )
    ),
    list(
      utility = list(
        alt1 = "b_x1 * x_1_1 + b_x2 * x_2_1 + b_x3 * x_3_1",
        alt2 = "b_x1 * x_1_2 + b_x2 * x_2_2 + b_x3 * x_3_2"
      ),
      param = list(
        b_x1 = 0.1,
        b_x2 = list(
          mu = -1,
          sigma = 1
        ),
        b_x3 = list(
          mu = 0,
          sigma = 1
        )
      ),
      attrs = list(
        x_2 = 1:3,
        x_3 = seq(0, 1, 0.25),
        x_1 = 2:5
      )
    )
  )

  # Testing multiple duplicates and reverse order fo some attributes and parameters
  expect_identical(
    suppressWarnings(
      parse_utility(
        utility <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[uniform_p(-1, 1)]      * x_2[1:3] + x_3[seq(0, 1, 0.25)] * b_x3[normal(0, 1)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        )
      )
    ),
    list(
      utility = list(
        alt1 = "b_x1 * x_1_1 + b_x2 * x_2_1 + x_3_1 * b_x3",
        alt2 = "b_x1 * x_1_2 + b_x2 * x_2_2 + b_x3 * x_3_2"
      ),
      param = list(
        b_x1 = 0.1,
        b_x2 = list(
          mu = -1,
          sigma = 1
        ),
        b_x3 = list(
          mu = 0,
          sigma = 1
        )
      ),
      attrs = list(
        x_2 = 1:3,
        x_3 = seq(0, 1, 0.25),
        x_1 = 2:5
      )
    )
  )
})
