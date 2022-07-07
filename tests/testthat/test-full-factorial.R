context("Test the dimensions and range of the full factorial")

test_that("Full factorial retrieves correct dimensions", {
  expect_equal(
    dim(
      full_factorial(
        list(
          attr1 = c(0, 1),
          attr2 = c(0, 1)
        )
      )
    ),
    c(4, 2)
  )
  expect_equal(
    dim(
      full_factorial(
        list(
          attr1 = c(0, 1, 3),
          attr2 = c(0, 1, 3),
          attr3 = c(0, 1)
        )
      )
    ),
    c(18, 3)
  )
  expect_equal(
    dim(
      full_factorial(
        list(
          attr1 = c(0, 1, 3),
          attr2 = c(0, 1, 3),
          attr3 = c(0, 1, 2)
        )
      )
    ),
    c(27, 3)
  )
})
