context("Test building the candidate set")

# The full factorial of all alternatives with the exclusions applied
full_factorial_excluded <- function(utility, exclusions = list()) {
  exclude(
    expand.grid(expand_attribute_levels(utility), KEEP.OUT.ATTRS = FALSE),
    exclusions
  )
}

# One string per row and alternative, and an order-free key per row
profile_keys <- function(candidate_set, alts) {
  sapply(alts, function(j) {
    do.call(paste, unname(candidate_set[startsWith(names(candidate_set), paste0(j, "_"))]))
  })
}

unordered_keys <- function(keys) {
  apply(keys, 1, function(k) paste(sort(k), collapse = " | "))
}

utility_unlabelled <- list(
  sq = "b_sq[0.2] * sq[1]",
  alt1 = "b_x1[0.1] * x1[1:3] + b_x2[-0.2] * x2[c(0, 1)]",
  alt2 = "b_x1      * x1      + b_x2       * x2"
)

test_that("Labelled alternatives give the full factorial", {
  utility <- list(
    car = "b_time[-0.1] * time_car[c(10, 20)] + b_cost[-0.2] * cost_car[c(5, 10)]",
    bus = "b_bus[0.1] * bus[1] + b_time * time_bus[c(20, 30)] + b_cost * cost_bus[c(2, 4)]"
  )

  candidate_set <- build_candidate_set(utility)
  full <- full_factorial_excluded(utility)

  expect_equal(names(candidate_set), names(full))
  expect_setequal(do.call(paste, candidate_set), do.call(paste, full))
})

test_that("Exchangeable alternatives give each set of different profiles once", {
  set.seed(1234)
  exclusions <- list(
    "alt1_x1 == 1 & alt1_x2 == 0",
    "alt2_x1 == 1 & alt2_x2 == 0",
    "alt1_x2 == alt2_x2"
  )

  candidate_set <- build_candidate_set(utility_unlabelled, exclusions)
  keys <- profile_keys(candidate_set, c("alt1", "alt2"))

  full <- full_factorial_excluded(utility_unlabelled, exclusions)
  keys_full <- profile_keys(full, c("alt1", "alt2"))
  keys_full <- keys_full[keys_full[, 1] != keys_full[, 2], ]

  expect_equal(names(candidate_set), names(full))
  expect_true(all(keys[, 1] != keys[, 2]))
  expect_false(anyDuplicated(unordered_keys(keys)) > 0)
  expect_setequal(unordered_keys(keys), unordered_keys(keys_full))
})

test_that("Reversed pairs give every order of different profiles", {
  candidate_set <- build_candidate_set(utility_unlabelled, allow_reversed_pairs = TRUE)
  keys <- profile_keys(candidate_set, c("alt1", "alt2"))

  keys_full <- profile_keys(full_factorial_excluded(utility_unlabelled), c("alt1", "alt2"))
  keys_full <- keys_full[keys_full[, 1] != keys_full[, 2], ]

  # 6 profiles give 6 x 5 ordered pairs
  expect_equal(nrow(candidate_set), 30)
  expect_setequal(paste(keys[, 1], keys[, 2]), paste(keys_full[, 1], keys_full[, 2]))
})

test_that("Three exchangeable alternatives give each set of profiles once", {
  set.seed(1234)
  utility <- list(
    alt1 = "b_x1[0.1] * x1[1:3] + b_x2[-0.2] * x2[c(0, 1)]",
    alt2 = "b_x1      * x1      + b_x2       * x2",
    alt3 = "b_x1      * x1      + b_x2       * x2"
  )

  keys <- profile_keys(build_candidate_set(utility), c("alt1", "alt2", "alt3"))

  # 6 profiles give choose(6, 3) sets of three
  expect_equal(nrow(keys), choose(6, 3))
  expect_false(anyDuplicated(unordered_keys(keys)) > 0)

  keys <- profile_keys(
    build_candidate_set(utility, allow_reversed_pairs = TRUE),
    c("alt1", "alt2", "alt3")
  )
  expect_equal(nrow(keys), 6 * 5 * 4)
})

test_that("An exclusion on only one of two alternatives keeps them apart", {
  # The alternatives are no longer exchangeable, so the result is the full
  # factorial with the exclusion applied
  exclusions <- list("alt1_x1 == 1 & alt1_x2 == 0")

  candidate_set <- build_candidate_set(utility_unlabelled, exclusions)
  full <- full_factorial_excluded(utility_unlabelled, exclusions)

  expect_setequal(do.call(paste, candidate_set), do.call(paste, full))
})

test_that("A large candidate set with reversed pairs gives a warning", {
  # 1001 profiles give 1001 x 1000 ordered pairs
  utility <- list(
    alt1 = "b_x1[0.1] * x1[1:7] + b_x2[0.1] * x2[1:11] + b_x3[0.1] * x3[1:13]",
    alt2 = "b_x1      * x1      + b_x2      * x2       + b_x3      * x3"
  )

  expect_warning(
    build_candidate_set(utility, allow_reversed_pairs = TRUE),
    "1,001,000 rows"
  )
  expect_warning(build_candidate_set(utility_unlabelled, allow_reversed_pairs = TRUE), NA)
})

test_that("Bayesian priors do not change the candidate set", {
  set.seed(1234)
  utility <- list(
    sq = "b_sq[normal_p(0.2, 0.1)] * sq[1]",
    alt1 = "b_x1_dummy[c(uniform_p(-1, 1), normal_p(0.5, 0.2))] * x1[c(1, 2, 3)](2:6) + b_x2[normal_p(0.4, 0.1)] * x2[c(0, 1)]",
    alt2 = "b_x1_dummy * x1 + b_x2 * x2"
  )

  keys <- profile_keys(build_candidate_set(utility), c("alt1", "alt2"))
  keys_full <- profile_keys(full_factorial_excluded(utility), c("alt1", "alt2"))
  keys_full <- keys_full[keys_full[, 1] != keys_full[, 2], ]

  expect_false(anyDuplicated(unordered_keys(keys)) > 0)
  expect_setequal(unordered_keys(keys), unordered_keys(keys_full))
})
