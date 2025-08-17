#' @title Masks two 8-bit coordinates into a single 16-bit value
#'
#' @description This function takes a vector of two numeric coordinates,
#' typically x and y, and combines them into a single 16-bit integer
#' by using bit shifting. It includes validation to ensure the coordinates
#' are within the valid 8-bit range (0-255).
#' This function is designed for Advent of Code operations to determine 
#' unique coordinates from a set.
#'
#' @param coords A numeric vector of length 2, with each value representing
#' a coordinate (e.g., c(x, y)).
#' @return A single numeric value representing the masked coordinates.
#' @export
#' 
#' @examples
#' # Valid coordinates
#' coord_mask(c(10, 20))
#'
#' # Coordinates at the maximum value
#' coord_mask(c(255, 255))
coord_mask <- function(coords) {
  if (any(coords < 0) || any(coords > 255)) stop("Coordinates must be between 0 and 255 (inclusive).")
  Reduce(bitwOr, mapply(bitwShiftL, coords, c(0, 8)))
}

#' @title Unmasks a single 16-bit value into two 8-bit coordinates
#'
#' @description This function takes a single 16-bit integer, created by
#' the `coord_mask` function, and extracts the original two 8-bit coordinates.
#'
#' @param masked_value A single numeric value representing the masked coordinates.
#' @return A numeric vector of length 2, containing the original
#' coordinates (e.g., c(x, y)).
#' @export
#' 
#' @examples
#' # Create a masked value and then unmask it
#' masked_val <- coord_mask(c(100, 150))
#' coord_unmask(masked_val)
coord_unmask <- function(masked_value) {
  sapply(mapply(bitwShiftR, masked_value, ((1:2) - 1) * 8), bitwAnd, 0xFF)
}
