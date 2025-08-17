#' @title Calculate the statistical mode of a vector
#'
#' @description This function calculates the statistical mode of a vector, which is the value that appears most frequently.
#' If there are multiple modes (i.e., a tie in frequency), the function returns the first value from the original vector that is part of the tie.
#' If there are no values, NULL will be returned.
#'
#' @param vec A vector of any type (e.g., numeric, character, or factor).
#' @return The mode of the vector.
#' @export
#'
#' @examples
#' # A simple numeric vector
#' mode(c(1, 2, 2, 3, 4, 4, 4))
#'
#' # A character vector
#' mode(c("apple", "banana", "banana", "cherry"))
#'
#' # A vector with a tie for the mode
#' # The function returns the first value of the tie
#' mode(c(1, 2, 2, 3, 3, 4))
mode <- function(vec) {
  uvec <- unique(vec)
  uvec[which.max(tabulate(match(vec, uvec)))]
}