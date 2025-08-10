# file: tests/testthat/test-color-conversion.R

library(testthat)
library(connoR)

# Helper function to check if two numeric vectors are approximately equal
is_approx_equal <- function(x, y, tolerance = 1e-6) {
  isTRUE(all.equal(x, y, tolerance = tolerance))
}

# --- Tests for rgba_to_hsla ---
test_that("rgba_to_hsla converts pure colors correctly", {
  # Pure Red (255, 0, 0)
  expect_equal(rgba_to_hsla(r = 255, g = 0, b = 0), c(h = 0, s = 1, l = 0.5, a = 1))
  # Pure Green (0, 255, 0)
  expect_equal(rgba_to_hsla(r = 0, g = 255, b = 0), c(h = 120, s = 1, l = 0.5, a = 1))
  # Pure Blue (0, 0, 255)
  expect_equal(rgba_to_hsla(r = 0, g = 0, b = 255), c(h = 240, s = 1, l = 0.5, a = 1))
})

test_that("rgba_to_hsla converts shades of gray and black/white correctly", {
  # Gray (128, 128, 128) - saturation should be 0
  expect_equal(rgba_to_hsla(r = 128, g = 128, b = 128), c(h = 0, s = 0, l = 128/255, a = 1))
  # Black (0, 0, 0)
  expect_equal(rgba_to_hsla(r = 0, g = 0, b = 0), c(h = 0, s = 0, l = 0, a = 1))
  # White (255, 255, 255)
  expect_equal(rgba_to_hsla(r = 255, g = 255, b = 255), c(h = 0, s = 0, l = 1, a = 1))
})

test_that("rgba_to_hsla handles percentage input correctly", {
  # Light blue with 50% opacity from example
  expected_output <- c(h = 210, s = 1, l = 0.75, a = 0.5)
  actual_output <- rgba_to_hsla(r = 0.5, g = 0.75, b = 1, a = 0.5, pct = TRUE)
  expect_true(is_approx_equal(actual_output, expected_output))

  # Pure green with percentage input
  expect_true(is_approx_equal(rgba_to_hsla(r = 0, g = 1, b = 0, pct = TRUE),
                              c(h = 120, s = 1, l = 0.5, a = 1)))
})

# --- Tests for hsla_to_rgba ---
test_that("hsla_to_rgba converts pure colors correctly", {
  # Pure Green from example
  expect_equal(hsla_to_rgba(h = 120, s = 1, l = 0.5), c(r = 0, g = 255, b = 0, a = 1))
  # Pure Red
  expect_equal(hsla_to_rgba(h = 0, s = 1, l = 0.5), c(r = 255, g = 0, b = 0, a = 1))
  # Pure Blue
  expect_equal(hsla_to_rgba(h = 240, s = 1, l = 0.5), c(r = 0, g = 0, b = 255, a = 1))
})

test_that("hsla_to_rgba converts shades of gray and black/white correctly", {
  # Gray (saturation=0, lightness=0.5)
  expect_equal(hsla_to_rgba(h = 0, s = 0, l = 0.5), c(r = 127.5, g = 127.5, b = 127.5, a = 1))
  # Black
  expect_equal(hsla_to_rgba(h = 0, s = 0, l = 0), c(r = 0, g = 0, b = 0, a = 1))
  # White
  expect_equal(hsla_to_rgba(h = 0, s = 0, l = 1), c(r = 255, g = 255, b = 255, a = 1))
})

test_that("hsla_to_rgba handles percentage output correctly", {
  # Pale orange from example (output as percentages)
  expected_output <- c(r = 0.94, g = 0.8, b = 0.66, a = 0.75)
  actual_output <- hsla_to_rgba(h = 30, s = 0.7, l = 0.8, a = 0.75, pct = TRUE)
  expect_true(is_approx_equal(actual_output, expected_output))
})

test_that("hsla_to_rgba converts specific colors correctly", {
  # Blue with 25% lightness from example
  expect_true(is_approx_equal(hsla_to_rgba(h = 240, s = 1, l = 0.25), c(r = 0, g = 0, b = 127.5, a = 1)))
})