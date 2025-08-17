#' Generate Run-Length IDs and Streak Counts
#'
#' This function analyzes a vector to identify consecutive sequences of identical
#' values, generating two corresponding vectors: a run-length ID (`rlid`) that
#' identifies each unique block of values and a streak count that tracks the
#' position within each block.
#'
#' The `rlid` is a unique identifier for each sequence of repeated values. For
#' example, in the vector `c("a", "a", "b", "c", "c")`, the `rlid` would be
#' `c(1, 1, 2, 3, 3)`. The `streak` count is a cumulative count within each
#' block, so for the same vector, the `streak` would be `c(1, 2, 1, 1, 2)`.
#' This can be particularly useful for grouping data by consecutive values or
#' for counting events within a sequence.
#'
#' @param vec A vector of any type that can be processed by `rle()`, such as
#'   numeric, character, or logical.
#' @param return A character string specifying the desired output. Must be one of
#'   `"all"`, `"streak"`, or `"rlid"`.
#'   \itemize{
#'     \item `"all"` (default): Returns a list containing both the `streak` and `rlid` vectors.
#'     \item `"streak"`: Returns only the `streak` count vector.
#'     \item `"rlid"`: Returns only the `rlid` vector.
#'   }
#' @param index_0 A logical value. If `TRUE`, the `rlid` vector will be zero-indexed
#'   (starting from `0`) instead of one-indexed (starting from `1`). This
#'   parameter only affects the `rlid` output. Defaults to `FALSE`.
#'
#' @return
#' If `return = "all"`, a list with two numeric vectors: `streak` and `rlid`.
#' Otherwise, a single numeric vector corresponding to the requested output.
#' The length of the returned vector(s) will be the same as the input vector `vec`.
#'
#' @examples
#' # --- Standalone Vector Examples ---
#'
#' # Example 1: Basic usage with a numeric vector
#' x <- c(1, 1, 2, 3, 3, 3, 1)
#' rlid_streak(x, return = "all")
#' # Returns a list: $streak=c(1, 2, 1, 1, 2, 3, 1), $rlid=c(1, 1, 2, 3, 3, 3, 4)
#'
#' # Example 2: Return only the streak count
#' rlid_streak(x, return = "streak")
#' # Returns: 1 2 1 1 2 3 1
#'
#' # Example 3: Return only the run-length ID
#' rlid_streak(x, return = "rlid")
#' # Returns: 1 1 2 3 3 3 4
#'
#' # Example 4: Using index_0 = TRUE for zero-indexed rlid
#' rlid_streak(x, return = "all", index_0 = TRUE)
#' # Returns a list: $streak=c(1, 2, 1, 1, 2, 3, 1), $rlid=c(0, 0, 1, 2, 2, 2, 3)
#'
#' # Example 5: Usage with a character vector
#' y <- c("A", "A", "B", "C", "C", "A")
#' rlid_streak(y, return = "rlid")
#' # Returns: 1 1 2 3 3 4
#'
#' # --- Data Frame Examples (using dplyr) ---
#' \dontrun{
#' library(dplyr)
#'
#' # Use rlid_streak to group by consecutive values in a data frame
#' data <- tibble::tibble(
#'   id = c(1, 1, 2, 3, 3, 3, 4),
#'   value = c(10, 20, 30, 40, 50, 60, 70)
#' )
#'
#' # Group by consecutive 'id' values and calculate the mean of 'value' for each block
#' data %>%
#'   group_by(block_id = rlid_streak(id, return = "rlid")) %>%
#'   summarise(
#'     n = n(),
#'     avg_value = mean(value),
#'     .groups = 'drop'
#'   )
#' }
#' @export
rlid_streak <- function(vec, return = c("all", "streak", "rlid"), index_0 = F) {
  # arg matching
  return <- match.arg(return)
  stopifnot(
    "vec must have length > 0" = length(vec) != 0
  )

  # Get run-length encoding information
  rle_info <- rle(vec)
  len <- rle_info$lengths

  # Generate rlid vector
  rlid <- rep(seq_along(len), len)

  # Generate streak vector
  streak <- unlist(sapply(len, FUN = seq, from = 1))
  streak <- as.vector(streak)

  # Adjust rlid if index_0 is TRUE
  if (index_0) {
    rlid <- rlid - 1
  }

  # Return the requested output
  switch(
    return,
    all = list(streak = streak, rlid = rlid),
    streak = streak,
    rlid = rlid
  )
}
