# file: tests/testthat/test-rlid_streak.R

# Load the testthat package and the function
library(testthat)

# Start the test suite
test_that("rlid_streak works with a basic numeric vector", {
  x <- c(1, 1, 2, 3, 3, 3, 1)

  # Test return = "all"
  result_all <- rlid_streak(x, return = "all")
  expect_type(result_all, "list")
  expect_equal(result_all$streak, c(1, 2, 1, 1, 2, 3, 1))
  expect_equal(result_all$rlid, c(1, 1, 2, 3, 3, 3, 4))

  # Test return = "streak"
  result_streak <- rlid_streak(x, return = "streak")
  expect_type(result_streak, "integer")
  expect_equal(result_streak, c(1, 2, 1, 1, 2, 3, 1))

  # Test return = "rlid"
  result_rlid <- rlid_streak(x, return = "rlid")
  expect_type(result_rlid, "integer")
  expect_equal(result_rlid, c(1, 1, 2, 3, 3, 3, 4))
})

test_that("index_0 parameter works correctly", {
  x <- c(1, 1, 2, 3, 3, 3, 1)

  # Test with return = "all" and index_0 = TRUE
  result_all_zero <- rlid_streak(x, return = "all", index_0 = TRUE)
  expect_equal(result_all_zero$rlid, c(0, 0, 1, 2, 2, 2, 3))
  expect_equal(result_all_zero$streak, c(1, 2, 1, 1, 2, 3, 1)) # streak should be unchanged

  # Test with return = "rlid" and index_0 = TRUE
  result_rlid_zero <- rlid_streak(x, return = "rlid", index_0 = TRUE)
  expect_equal(result_rlid_zero, c(0, 0, 1, 2, 2, 2, 3))
})

test_that("rlid_streak handles different data types", {
  # Character vector
  y <- c("A", "A", "B", "C", "C", "A")
  expect_equal(rlid_streak(y, return = "rlid"), c(1, 1, 2, 3, 3, 4))
  expect_equal(rlid_streak(y, return = "streak"), c(1, 2, 1, 1, 2, 1))

  # Logical vector
  z <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  expect_equal(rlid_streak(z, return = "rlid"), c(1, 1, 2, 3, 3, 3, 4))
  expect_equal(rlid_streak(z, return = "streak"), c(1, 2, 1, 1, 2, 3, 1))
})

test_that("rlid_streak handles edge cases", {
  # All unique values
  unique_vec <- 1:5
  expect_equal(rlid_streak(unique_vec, return = "rlid"), 1:5)
  expect_equal(rlid_streak(unique_vec, return = "streak"), rep(1, 5))

  # All identical values
  identical_vec <- rep(10, 5)
  expect_equal(rlid_streak(identical_vec, return = "rlid"), rep(1, 5))
  expect_equal(rlid_streak(identical_vec, return = "streak"), 1:5)

  # Single element vector
  single_vec <- 99
  expect_equal(rlid_streak(single_vec, return = "rlid"), 1)
  expect_equal(rlid_streak(single_vec, return = "streak"), 1)
})

test_that("rlid_streak handles empty vectors", {
  empty_vec <- numeric(0)
  expect_error(
    rlid_streak(empty_vec, return = "all"),
    "vec must have length > 0"
  )
  expect_error(
    rlid_streak(empty_vec, return = "rlid"),
    "vec must have length > 0"
  )
  expect_error(
    rlid_streak(empty_vec, return = "streak"),
    "vec must have length > 0"
  )
})

test_that("invalid return value throws an error", {
  x <- c(1, 2, 3)
  expect_error(rlid_streak(x, return = "invalid_option"), "should be one of")
})
