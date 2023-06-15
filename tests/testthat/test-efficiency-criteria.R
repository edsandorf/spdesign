context("Calculation of efficiency criteria")

x <- matrix(
  c(
    0.0216545064932912, 0.0124575011460145,
    0.0124575011460146, 0.0195733876561729
  ),
  nrow = 2)
p <- c("b_x1" = 0.2, "b_x2" = -0.1)

test_that(
  "A-efficiency calculates correctly", {
    expect_equal(
      calculate_efficiency_criteria(
        design_vcov = x,
        type = "a-error"
      ),
      0.0206139,
      tolerance = 1e-6
    )
  }
)

test_that(
  "C-efficiency calculates correctly", {
    expect_equal(
      calculate_efficiency_criteria(
        design_vcov = x,
        p = p,
        dudx = "b_x2",
        type = "c-error"
      ),
      0.936113,
      tolerance = 1e-6
    )
  }
)

test_that(
  "D-efficiency calculates correctly", {
    expect_equal(
      calculate_efficiency_criteria(
        design_vcov = x,
        type = "d-error"
      ),
      0.0163909,
      tolerance = 1e-6
    )
  }
)

test_that(
  "S-efficiency calculates correctly", {
    expect_equal(
      calculate_efficiency_criteria(
        design_vcov = x,
        p = p,
        type = "s-error"
      ),
      7.51931,
      tolerance = 1e-6
    )
  }
)
