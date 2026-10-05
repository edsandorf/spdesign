context("That exclusions are correctly applied to the candidate set")

candidate_set <- expand.grid(x1 = c(0, 1), x2 = c(0, 1), KEEP.OUT.ATTRS = FALSE)

test_that("Restriction pattern one", {
  exclusions <- list(
    "x1 == 1 & x2 == 1"
  )

  expect_equal(
    exclude(candidate_set, exclusions),
    data.frame(x1 = c(0, 1, 0), x2 = c(0, 0, 1))
  )
})


test_that("Restriction pattern two", {
  exclusions <- list(
    "x1 == 1 & x2 == 1",
    "x1 == 0"
  )

  expect_equal(
    exclude(candidate_set, exclusions),
    data.frame(x1 = 1, x2 = 0, row.names = 2L)
  )
})

test_that("Attribute names that share a prefix are excluded correctly", {
  # alt1_x1 is a prefix of alt1_x10. This used to exclude all rows.
  candidate_set <- expand.grid(alt1_x1 = 1:3, alt1_x10 = 1:2, KEEP.OUT.ATTRS = FALSE)

  expect_equal(nrow(exclude(candidate_set, list("alt1_x10 == 2"))), 3)
  expect_equal(nrow(exclude(candidate_set, list("alt1_x1 == 1"))), 4)
})
