context("Test that the efficiency criteria are calculated to an arbitrary precision level")

x <- matrix(c(0.0216545064932912, 0.0124575011460145, 0.0124575011460146,
              0.0195733876561729), nrow = 2)
p <- c(0.2, -0.1)

test_that(
  "A-efficiency calculates correctly",
  {
    expect_equal(a_efficiency(x), 0.0206139, tolerance = 1e-6)
  }
)

test_that(
  "C-efficiency calculates correctly",
  {
    expect_equal(c_efficiency(p, x, 2), 0.936113, tolerance = 1e-6)
  }
)

test_that(
  "D-efficiency calculates correctly",
  {
    expect_equal(d_efficiency(x), 0.0163909, tolerance = 1e-6)
  }
)

test_that(
  "S-efficiency calculates correctly",
  {
    expect_equal(s_efficiency(p, x), 7.51931, tolerance = 1e-6)
  }
)
