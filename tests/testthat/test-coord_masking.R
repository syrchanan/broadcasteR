# Make sure the 'testthat' package is loaded
library(testthat)
library(connoR)

# --- Tests for coord_mask() ---
context("Test for coord_mask() function")

test_that("coord_mask() correctly masks two valid coordinates", {
  # Test with a standard pair of coordinates
  expect_equal(coord_mask(c(10, 20)), 5130)
  
  # Test with the maximum possible values
  expect_equal(coord_mask(c(255, 255)), 65535)
  
  # Test with zero values
  expect_equal(coord_mask(c(0, 0)), 0)
  
  # Test with a mix of max and min values
  expect_equal(coord_mask(c(255, 0)), 255)
  expect_equal(coord_mask(c(0, 255)), 65280)
})

test_that("coord_mask() throws an error for out-of-range coordinates", {
  # Test with a negative coordinate
  expect_error(coord_mask(c(-1, 100)), "Coordinates must be between 0 and 255")
  
  # Test with a coordinate greater than 255
  expect_error(coord_mask(c(256, 100)), "Coordinates must be between 0 and 255")
  
  # Test with both coordinates out of range
  expect_error(coord_mask(c(-10, 300)), "Coordinates must be between 0 and 255")
})

# --- Tests for coord_unmask() ---
context("Test for coord_unmask() function")

test_that("coord_unmask() correctly unmasks values back to original coordinates", {
  # Test a standard masked value
  masked_val <- coord_mask(c(10, 20))
  expect_equal(coord_unmask(masked_val), c(10, 20))
  
  # Test the maximum masked value
  masked_val_max <- coord_mask(c(255, 255))
  expect_equal(coord_unmask(masked_val_max), c(255, 255))
  
  # Test the minimum masked value
  masked_val_min <- coord_mask(c(0, 0))
  expect_equal(coord_unmask(masked_val_min), c(0, 0))
  
  # Test with swapped coordinates
  masked_val_swapped <- coord_mask(c(200, 100))
  expect_equal(coord_unmask(masked_val_swapped), c(200, 100))
})

test_that("coord_unmask() works with non-integer inputs due to bitwise truncation", {
  # The bitwise operations will automatically handle truncation, which is the expected behavior
  expect_equal(coord_unmask(1234.56), c(210, 4))
})

