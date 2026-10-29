context("Test the relabel and swap moves of the RSC algorithm")

test_that("Swapping always changes a column with more than one level", {
  set.seed(1234)
  x <- c(1, 1, 1, 2, 2, 3)

  for (i in seq_len(50)) {
    y <- swap(x)

    expect_false(identical(y, x))
    expect_equal(sum(y != x), 2)
    expect_equal(as.vector(table(y)), as.vector(table(x)))
  }
})

test_that("Swapping leaves a constant column unchanged", {
  expect_equal(swap(rep(1, 6)), rep(1, 6))
})

test_that("Swapping works with dummy-coded attributes", {
  set.seed(1234)
  x <- factor(c(1, 2, 3, 1, 2, 3))
  y <- swap(x)

  expect_true(is.factor(y))
  expect_equal(as.vector(table(y)), as.vector(table(x)))
  expect_false(identical(y, x))
})

test_that("Relabeling keeps the number of occurrences of each level", {
  set.seed(1234)
  x <- c(1, 1, 1, 2, 2, 3)
  y <- relabel(x)

  expect_equal(sort(as.vector(table(y))), sort(as.vector(table(x))))
})
