# file: tests/testthat/test-paretoset.R

library(testthat)
library(connoR)

# --- Tests for the internal paretoset_algorithm function ---

test_that("paretoset_algorithm identifies efficient set correctly for minimization", {
  # All objectives are assumed to be pre-transformed for minimization
  df_min <- data.frame(
    x = c(10, 20, 15, 5, 25, 10, 20),
    y = c(10, 5, 20, 15, 12, 10, 5)
  )

  expected_mask <- c(1, NA, NA, 1, NA, NA, NA)

  df_for_algo <- data.frame(
    x = c(10, 20, 15, 5, 25),
    y = c(10, 5, 20, 15, 12)
  )

  result <- paretoset_algorithm(df_for_algo)
  expect_equal(result, c(1, 1, NA, 1, NA))
})

test_that("paretoset_algorithm handles distinct vs non-distinct correctly", {
  df_with_dupes <- data.frame(
    x = c(10, 20, 15, 5, 25, 10, 20),
    y = c(10, 5, 20, 15, 12, 10, 5)
  )

  # When only_distinct is TRUE, it should only return one of each efficient point
  result_distinct <- paretoset_algorithm(df_with_dupes, only_distinct = TRUE)
  expect_equal(result_distinct, c(1, 1, NA, 1, NA, NA, NA)) # (10,10), (20,5), (5,15) are efficient

  # When only_distinct is FALSE, it should return all efficient points, including duplicates
  result_non_distinct <- paretoset_algorithm(
    df_with_dupes,
    only_distinct = FALSE
  )
  expect_equal(result_non_distinct, c(1, 1, NA, 1, NA, 1, 1))
})


# --- Tests for the internal paretoset_skyline function ---

test_that("paretoset_skyline calculates ranks correctly", {
  df_min <- data.frame(
    x = c(10, 6, 6, 4),
    y = c(4, 6, 8, 10)
  )

  expected_ranks <- c(1, 1, 2, 1)
  result <- paretoset_skyline(df_min)
  expect_equal(result, expected_ranks)
})

test_that("paretoset_skyline respects max_rank", {
  df_min <- data.frame(
    x = c(10, 6, 6, 4),
    y = c(4, 6, 8, 10)
  )

  expected_ranks <- c(1, 1, NA, 1)
  result <- paretoset_skyline(df_min, max_rank = 1)
  expect_equal(result, expected_ranks)
})


# --- Tests for the main paretoset function ---

test_that("paretoset correctly identifies Pareto set (minimization)", {
  data <- data.frame(
    x = c(10, 20, 15, 5, 25),
    y = c(10, 5, 20, 15, 12)
  )

  expected_result <- c(1, 1, NA, 1, NA)
  result <- paretoset(data, method = "pareto", optimization = c("min", "min"))
  expect_equal(result, expected_result)
})

test_that("paretoset correctly identifies Pareto set (maximization)", {
  data <- data.frame(
    profit = c(100, 120, 90, 110, 130),
    quality = c(8, 7, 9, 6, 8.5)
  )

  expected_result <- c(1, 1, 1, NA, 1)
  result <- paretoset(data, method = "pareto", optimization = c("max", "max"))
  expect_equal(result, expected_result)
})

test_that("paretoset calculates skyline ranks correctly (minimization)", {
  data_skyline <- data.frame(
    x = c(10, 20, 15, 5, 25),
    y = c(10, 5, 20, 15, 12)
  )

  expected_result <- c(1, 1, 2, 1, 2)
  result <- paretoset(
    data_skyline,
    method = "skyline",
    optimization = c("min", "min")
  )
  expect_equal(result, expected_result)
})

test_that("paretoset calculates skyline ranks correctly (maximization)", {
  data_skyline <- data.frame(
    profit = c(100, 120, 90, 110, 130),
    quality = c(8, 7, 9, 6, 8.5)
  )

  expected_result <- c(1, 1, 1, 2, 1)
  result <- paretoset(
    data_skyline,
    method = "skyline",
    optimization = c("max", "max")
  )
  expect_equal(result, expected_result)
})

test_that("paretoset handles invalid inputs with errors", {
  data <- data.frame(x = 1:5, y = 1:5)

  # Mismatched length of optimization vector
  expect_error(
    paretoset(data, optimization = "min"),
    "length\\(optimization\\) == ncol\\(df\\)"
  )

  # Non-numeric data
  data_with_char <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(paretoset(data_with_char), "all\\(sapply\\(df, is.numeric\\)\\)")
})
