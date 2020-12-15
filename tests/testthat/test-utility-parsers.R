context("Utility parsers")

test_that("Remove white spaces does exactly that", {
  expect_equal(remove_whitespace("   x _1"), "x_1")
  expect_equal(remove_whitespace("x_1"), "x_1")
  expect_equal(remove_whitespace("x_1  "), "x_1")
  expect_equal(remove_whitespace("x_ 1  "), "x_1")

})

test_that("Extract names is robust to a variety of vectors", {
  expect_equal(extract_names(c("x_1|n")), "x_1")
  expect_true(all(extract_names(c("x_1|n", "x_2")) ==  c("x_1", "x_2")))
  expect_error(extract_names(c("x_1|n", "x_1|666kn"))) # Duplicates
  expect_error(extract_names("|test")) # No name specified
})



test_that("Extract params returns a named vector", {
  expect_true(extract_params("beta_1 | c(0.1)") == c(beta_1 = 0.1))
  expect_true(all(extract_params("beta_1 | c(N(0.1, 1))") ==  c(mu_beta_1 = 0.1, sigma_beta_1 = 1)))
  expect_true(all(extract_params("beta_1 | N(0.1, 1)") ==  c(mu_beta_1 = 0.1, sigma_beta_1 = 1)))
})
