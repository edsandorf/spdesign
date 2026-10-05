context("Test the dimensions and range of the full factorial")

# full_factorial() is deprecated. Remove these tests with the function.
test_that("Full factorial is deprecated", {
  expect_warning(
    full_factorial(list(attr1 = c(0, 1), attr2 = c(0, 1))),
    "deprecated"
  )
})

test_that("Full factorial retrieves correct dimensions", {
  suppressWarnings({
    expect_equal(dim(full_factorial(list(attr1 = c(0, 1), attr2 = c(0, 1)))), c(4, 2))
    expect_equal(dim(full_factorial(list(attr1 = c(0, 1, 3), attr2 = c(0, 1, 3), attr3 = c(0, 1)))), c(18, 3))
    expect_equal(dim(full_factorial(list(attr1 = c(0, 1, 3), attr2 = c(0, 1, 3), attr3 = c(0, 1, 2)))), c(27, 3))
  })
})
