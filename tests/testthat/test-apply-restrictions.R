context("That exclusions are correctly applied to the candidate set")

candidate_set <- full_factorial(list(x1 = c(0, 1), x2 = c(0, 1)))

test_that("Restriction pattern one", {
  exclusions <- list(
    "x1 == 1 & x2 == 1"
  )

  expect_equal(
    exclude(candidate_set, exclusions),
    structure(
      list(
        x1 = c(0, 1, 0),
        x2 = c(0, 0, 1)),
      out.attrs = list(
        dim = c(x1 = 2L, x2 = 2L),
        dimnames = list(
          x1 = c("x1=0", "x1=1"),
          x2 = c("x2=0", "x2=1")
        )
      ),
      row.names = c(NA, 3L),
      class = "data.frame")
  )
})


test_that("Restriction pattern two", {
  exclusions <- list(
    "x1 == 1 & x2 == 1",
    "x1 == 0"
  )

  expect_equal(
    exclude(candidate_set, exclusions),
    structure(
      list(
        x1 = 1,
        x2 = 0
      ),
      out.attrs = list(
        dim = c(x1 = 2L, x2 = 2L),
        dimnames = list(
          x1 = c("x1=0", "x1=1"),
          x2 = c("x2=0", "x2=1")
        )
      ),
      row.names = 2L,
      class = "data.frame")
  )

})
