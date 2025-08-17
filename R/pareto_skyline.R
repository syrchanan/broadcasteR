#' Internal Function: Pareto Efficient Algorithm
#'
#' This function identifies the Pareto efficient set from a given dataset.
#' It's an internal helper function used by `paretoset` and is not exported
#' for direct use by users. The algorithm iteratively removes dominated points
#' until only Pareto efficient points remain.
#'
#' @param df A data frame of numeric values. Each row represents an observation,
#'   and each column represents an objective function value. It's assumed that
#'   the objectives have already been transformed (e.g., by multiplying by -1
#'   for maximization problems) so that all objectives are treated as minimization.
#' @param only_distinct Logical. If `TRUE`, the algorithm ensures that only
#'   distinct Pareto efficient points are identified. If `FALSE`, duplicate
#'   points that are Pareto efficient are also retained.
#' @param optimization Character vector. Although passed from the wrapper,
#'   this internal function does not directly use this parameter as the
#'   `df` is assumed to be pre-transformed for minimization. Included for
#'   API consistency with the calling `paretoset` function.
#' @return A numeric vector of the same length as the number of rows in the
#'   original `df`. Values are `1` for Pareto efficient points and `NA` for
#'   dominated points. This vector can be used as a mask.
#' @keywords internal
#' @examples
#' # This function is internal and not meant for direct user calls.
#' # For demonstration, here's how it would operate on a pre-processed df:
#' # df_min <- data.frame(x = c(1, 2, 3), y = c(3, 2, 1))
#' # result <- paretoset_algorithm(df_min) # Internal call
#' @noRd
paretoset_algorithm <- function(df, only_distinct = T, optimization = NULL) {
  # init vars
  ncosts <- nrow(df)
  nobjs <- ncol(df)
  eff_mask <- c(1:ncosts)
  nxt_idx <- 1

  while (nxt_idx < nrow(df)) {
    # get current cost as vector
    df[nxt_idx, ] %>%
      purrr::transpose() %>%
      purrr::map(~ purrr::list_c(.x)) %>%
      purrr::pluck(1) -> cost

    # compare rowwise to see which dominate cost (either down or to left of it)
    df %>%
      purrr::transpose() %>%
      purrr::map(~ any(purrr::list_c(.x) < cost)) %>%
      purrr::list_c() -> curr_eff

    # if we don't care about distinct points, then keep all dupes
    if (!only_distinct) {
      no_smaller <- !curr_eff
      tibble::as_tibble(df[no_smaller, ]) %>%
        purrr::pmap(~ all(cost == c(...))) %>%
        purrr::list_c() -> equal_to
      curr_eff[no_smaller] <- curr_eff[no_smaller] | equal_to
    }

    # can't compare to itself
    curr_eff[nxt_idx] <- T

    # remove dominated pts
    eff_mask <- eff_mask[curr_eff]
    df <- df[curr_eff, ]

    # adjust index
    nxt_idx <- sum(curr_eff[1:nxt_idx]) + 1
  }

  eff_mask_global <- rep(NA, ncosts)
  eff_mask_global[eff_mask] <- 1

  return(eff_mask_global)
}

#' Internal Function: Pareto Skyline Algorithm
#'
#' This function calculates the Pareto skyline ranks for a given dataset.
#' It's an internal helper function used by `paretoset` and is not exported
#' for direct use by users. It repeatedly applies the `paretoset_algorithm`
#' to identify successive Pareto fronts (skyline ranks).
#'
#' @param df A data frame of numeric values. Each row represents an observation,
#'   and each column represents an objective function value. Similar to
#'   `paretoset_algorithm`, it's assumed objectives are pre-transformed for
#'   minimization.
#' @param only_distinct Logical. If `TRUE`, only distinct Pareto points are
#'   considered when identifying each skyline rank.
#' @param optimization Character vector. Passed from the wrapper but not
#'   directly used here as `df` is pre-transformed.
#' @param max_rank Numeric. The maximum Pareto rank to compute. If `NULL`,
#'   the algorithm continues until all points are ranked or no more points
#'   remain.
#' @return A numeric vector of the same length as the number of rows in the
#'   original `df`. Values represent the Pareto rank (1 for the first skyline,
#'   2 for the second, etc.) and `NA` for points beyond `max_rank` or those
#'   not included in any computed rank.
#' @keywords internal
#' @examples
#' # This function is internal and not meant for direct user calls.
#' # For demonstration:
#' # df_min <- data.frame(x = c(1, 2, 3, 4), y = c(4, 3, 2, 1))
#' # ranks <- paretoset_skyline(df_min, max_rank = 2) # Internal call
#' @noRd
paretoset_skyline <- function(
  df,
  only_distinct = T,
  optimization = NULL,
  max_rank = NULL
) {
  # init vars
  ncosts <- nrow(df)
  nobjs <- ncol(df)
  all_ranks <- rep(0, ncosts)
  rem_rows <- rep(T, ncosts)
  proc_rows <- rep(F, ncosts)
  rank <- 1

  message(">>> Beginning Skyline Identification")

  # while there are rows yet to be processed
  while (sum(rem_rows) > 0) {
    if (!is.null(max_rank) && rank > max_rank) {
      message(">>> Max rank reached, exiting")
      break
    }

    message(paste0(
      ">>> Searching for rank: ",
      rank,
      "; Rows remaining: ",
      sum(rem_rows)
    ))

    # get pareto front
    eff_mask <- paretoset_algorithm(df[rem_rows, ], only_distinct, optimization)
    eff_mask[is.na(eff_mask)] <- 0

    # update rows that have been processed
    # eff_mask will shrink each iter, so need to match shape
    proc_rows[!proc_rows] <- eff_mask

    # update ranks of those processed but not removed from remaining
    # eff_mask will shrink each iter, so need to match shape
    all_ranks[proc_rows & rem_rows] <- rank

    # update remaining rows by inverting eff_mask
    rem_rows[rem_rows] <- !eff_mask

    # update and proceed
    rank <- rank + 1
  }

  all_ranks[all_ranks == 0] <- NA

  return(all_ranks)
}

#' Identify Pareto Efficient Solutions or Skyline Ranks
#'
#' This function identifies Pareto efficient solutions or calculates Pareto
#' skyline ranks for multi-objective optimization problems. It supports both
#' minimization and maximization of objectives.
#'
#' @param df A data frame of numeric values, where rows represent observations
#'   and columns represent different objectives.
#' @param method Character string, either `"pareto"` to identify the Pareto
#'   efficient set, or `"skyline"` to calculate Pareto skyline ranks.
#'   Defaults to `"pareto"`.
#' @param distinct Logical. If `TRUE`, only distinct Pareto points are considered
#'   (for `"pareto"` method) or ranked (for `"skyline"` method). Defaults to `TRUE`.
#' @param optimization Character vector specifying the optimization direction
#'   for each column in `df`. Can be `"min"` for minimization or `"max"` for
#'   maximization. If `NULL`, all objectives are assumed to be minimized.
#'   The length of this vector must match the number of columns in `df`.
#' @param max_rank Numeric. Applies only when `method = "skyline"`. Specifies
#'   the maximum Pareto rank to compute. If `NULL`, all ranks are computed.
#' @return
#' If `method = "pareto"`, a numeric vector of the same length as the number of rows in `df`.
#' Values are `1` for Pareto efficient points and `NA` for dominated points.
#' If `method = "skyline"`, a numeric vector of the same length as the number of rows in `df`.
#' Values represent the Pareto rank (1 for the first skyline, 2 for the second, etc.)
#' and `NA` for points beyond `max_rank` or not ranked.
#' @examples
#' # Example with Pareto set identification (minimization)
#' data <- data.frame(
#'   x = c(10, 20, 15, 5, 25),
#'   y = c(10, 5, 20, 15, 12)
#' )
#' pareto_set_result <- paretoset(data, method = "pareto", optimization = c("min", "min"))
#' data$pareto_set <- pareto_set_result
#' print(data)
#'
#' # Example with Skyline ranks (maximization)
#' data_skyline <- data.frame(
#'   profit = c(100, 120, 90, 110, 130),
#'   quality = c(8, 7, 9, 6, 8.5)
#' )
#' skyline_ranks_result <- paretoset(
#'   data_skyline,
#'   method = "skyline",
#'   optimization = c("max", "max"),
#'   max_rank = 3
#' )
#' data_skyline$skyline_rank <- skyline_ranks_result
#' print(data_skyline)
#'
#' # Plotting example (requires ggplot2)
#' \dontrun{
#' library(ggplot2)
#' tibble::tibble(
#'   x = runif(1000, 100, 900),
#'   y = runif(1000, 100, 900)
#' ) %>%
#'   dplyr::bind_cols(
#'     pareto = paretoset(
#'       df = .,
#'       method = "skyline",
#'       distinct = TRUE,
#'       optimization = c("max", "max"),
#'       max_rank = 25
#'     )
#'   ) -> sample_data
#'
#' ggplot2::ggplot(sample_data) +
#'   ggplot2::geom_point(ggplot2::aes(x, y, color = pareto)) +
#'   ggplot2::scale_color_viridis_c(
#'     "Pareto Rank",
#'     direction = -1,
#'     guide = ggplot2::guide_colorbar(reverse = TRUE)
#'   )
#' }
#' @export
#' @importFrom dplyr as_tibble if_else bind_cols
#' @importFrom purrr map map_lgl list_c pmap transpose
#' @importFrom ggplot2 ggplot geom_point scale_color_viridis_c guide_colorbar
#' @importFrom tibble tibble
paretoset <- function(
  df,
  method = c("pareto", "skyline"),
  distinct = T,
  optimization = NULL,
  max_rank = NULL
) {
  # arg checking/setting
  method <- match.arg(method)

  if (is.null(optimization)) {
    optimization <- rep("min", ncol(df))
  }

  stopifnot(
    ncol(df) > 1,
    if (!is.null(max_rank)) is.numeric(max_rank) else T,
    is.logical(distinct),
    length(optimization) == ncol(df),
    all(optimization %in% c("min", "max", NULL)),
    all(sapply(df, is.numeric))
  )

  optimization_vals <- dplyr::if_else(optimization == "min", 1, -1)
  for (i in 1:ncol(df)) {
    df[, i] <- df[, i] * optimization_vals[i]
  }

  # run designated algorithm
  switch(
    method,
    "pareto" = paretoset_algorithm(df, distinct, optimization),
    "skyline" = paretoset_skyline(df, distinct, optimization, max_rank)
  )
}