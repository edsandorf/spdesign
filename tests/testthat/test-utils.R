context("Test utils")

x <- matrix(1:4, nrow = 2)

test_that("Repeat rows correctly repeats the rows of a matrix", {
  expect_equal(
    rep_rows(x, 2),
    structure(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      .Dim = c(4L, 2L)
    )
  )
})

test_that("Repeat columns correctly repeats the columns of a matrix", {
  expect_equal(
    rep_cols(x, 2),
    structure(
      c(1L, 2L, 1L, 2L, 3L, 4L, 3L, 4L),
      .Dim = c(2L, 4L)
    )
  )
})

test_that("Names are matched as whole words", {
  expect_true(stringr::str_detect("b_x1 * x1", as_whole_word("x1")))
  expect_true(stringr::str_detect("I(x1*x2)", as_whole_word("x1")))
  expect_false(stringr::str_detect("b_x10 * x10", as_whole_word("x1")))
  expect_false(stringr::str_detect("alt1_x1", as_whole_word("x1")))
  expect_false(stringr::str_detect("b_sq[0.2] * sq[1]", as_whole_word("b")))
  expect_false(stringr::str_detect("b_p[normal_p(0, 1)] * q", as_whole_word("p")))
})
