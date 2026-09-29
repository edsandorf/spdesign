context("Test blocking of the design")

test_that("Blocking adds a balanced blocking column", {
  utility <- list(
    alt1 = "b_x1[0.2] * x1[c(1, 2, 3)] + b_x2[-0.4] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1             + b_x2       * x2"
  )

  set.seed(1234)

  utils::capture.output(
    design <- suppressMessages(
      generate_design(
        utility,
        rows = 6,
        model = "mnl",
        efficiency_criteria = "d-error",
        algorithm = "rsc",
        draws = "pseudo-random",
        control = list(max_iter = 20)
      )
    )
  )

  blocked <- suppressWarnings(block(design, 2, max_iter = 100))

  expect_true("block" %in% names(blocked$design))
  expect_equal(as.vector(table(blocked$design$block)), c(3, 3))
  expect_error(block(design, 4), "uneven number of rows")
})
