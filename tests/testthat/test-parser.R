context("Correctly parses the list of utility functions")

test_that("Error messages are triggered correctly", {
  expect_error(
    parse_utility(
      V <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2 + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      )
    )
  )
})

test_that("Warning messages are triggered correctly", {
  expect_warning(
    parse_utility(
      V <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1[6]      * x_1[2:5] + b_x2[0.4] * x_2[1:6]      + b_x3          * x_3"
      )
    )
  )
})

test_that("The utility parser correctly splits and returns attributes and parameters", {
  expect_identical(
    parse_utility(
      V <- list(
        alt1 = "b_x1[0.1] * x_1      + b_x2      * x_2[1:3] + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]",
        alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3"
      )
    ),
    list(
      V = list(
        alt1 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3",
        alt2 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3"
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

  # Testing multiple duplicates
  expect_identical(
    suppressWarnings(
      parse_utility(
        V <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[Up(-1, 1)]      * x_2[1:3] + b_x3[N(0, 1)] * x_3[seq(0, 1, 0.25)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        )
      )
    ),
    list(
      V = list(
        alt1 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3",
        alt2 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3"
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
        V <- list(
          alt1 = "b_x1[0.1] * x_1      + b_x2[Up(-1, 1)]      * x_2[1:3] + x_3[seq(0, 1, 0.25)] * b_x3[N(0, 1)]",
          alt2 = "b_x1      * x_1[2:5] + b_x2[0.4] * x_2      + b_x3          * x_3[1:6]"
        )
      )
    ),
    list(
      V = list(
        alt1 = "b_x1 * x_1 + b_x2 * x_2 + x_3 * b_x3",
        alt2 = "b_x1 * x_1 + b_x2 * x_2 + b_x3 * x_3"
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
