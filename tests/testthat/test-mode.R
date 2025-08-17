# Make sure the 'testthat' package is loaded
library(testthat)
library(connoR)


# Test 1: Simple case with a clear mode
test_that("mode() returns the correct mode for a simple numeric vector", {
  x <- c(1, 2, 2, 3, 4, 4, 4, 5)
  expect_equal(mode(x), 4)
})

# Test 2: Handles ties by returning the first mode encountered
test_that("mode() handles ties correctly by returning the first occurring mode", {
  # In this case, both 2 and 3 appear twice, but 2 appears first
  x <- c(1, 2, 2, 3, 3, 4, 5)
  expect_equal(mode(x), 2)
})

# Test 3: Works with character vectors
test_that("mode() works correctly with a character vector", {
  x <- c("a", "b", "c", "c", "d", "a", "c")
  expect_equal(mode(x), "c")
})

# Test 4: Works with a single-element vector
test_that("mode() returns the single element for a vector of length 1", {
  x <- 5
  expect_equal(mode(x), 5)
})

# Test 5: Works with an empty vector (this might return an error, which is expected)
# The current function returns a NULL, which is a common and acceptable behavior.
test_that("mode() returns NULL for an empty vector", {
  x <- c()
  expect_true(is.null(mode(x)))
})

# Test 6: Works when all elements are unique (no mode)
test_that("mode() returns the first element when all are unique", {
  x <- 1:5
  expect_equal(mode(x), 1)
})
