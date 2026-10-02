context("Test the random design candidate")

test_that("Random design candidates satisfy the level occurrences", {
  utility <- list(
    alt1 = "b_x1[0.1] * x1[c(1, 2, 3)](6:8, 4:8, 6:8) + b_x2[0.4] * x2[c(0, 1)](10) + b_x3[-0.2] * x3[seq(0, 1, 0.25)]",
    alt2 = "b_x1      * x1                            + b_x2      * x2               + b_x3       * x3"
  )

  rows <- 20
  candidate_set <- expand.grid(expand_attribute_levels(utility), KEEP.OUT.ATTRS = FALSE)

  set.seed(1234)

  for (i in seq_len(5)) {
    design_candidate <- random_design_candidate(
      utility,
      candidate_set,
      rows,
      sample_with_replacement = FALSE
    )

    expect_equal(nrow(design_candidate), rows)
    expect_equal(lvl_violation(utility, design_candidate, rows), 0)
    expect_equal(anyDuplicated(design_candidate), 0)
  }
})
