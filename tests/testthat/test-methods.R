context("Test the methods for spdesign objects")

test_that("cor() does not mask stats::cor()", {
  x <- matrix(c(1, 2, 3, 2, 4, 7), ncol = 2)

  expect_equal(cor(x), stats::cor(x))
  expect_equal(cor(x[, 1], x[, 2], method = "spearman"), stats::cor(x[, 1], x[, 2], method = "spearman"))
})

test_that("cor() for a design object is deprecated", {
  utility <- list(
    alt1 = "b_x1[0.2] * x1[1:3] + b_x2[-0.4] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1      + b_x2       * x2"
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

  expect_warning(correlation <- cor(design), "deprecated")
  expect_equal(correlation, stats::cor(design$design))
})
