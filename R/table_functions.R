#' Calculate Streaks
#'
#' @description This function takes a dataframe, which has an ordering column and a numeric
#' boolean indicator (0s and 1s), and calculates the streak of the indicator.
#' This allows for streak counting and analysis, which can help determine
#' relationships over time.
#'
#' @param df A dataframe object which has at least two columns.
#' @param bool_column A column of df which has a numeric boolean indication (0s and 1s)
#' of the streak to detect.
#' @param date_column A column of df which is used for ordering (be it a date, number, etc.).
#'
#' @return A copy of the dataframe object with a streak column added.
#'
#' @export
streak_counter <- function(df, bool_column, date_column) {
  date_ref <- enquote(date_column)
  bool_ref <- enquote(bool_column)
  df <- df[order(eval(date_ref)), ]
  df$streak <- with(
    df,
    ave(eval(bool_ref),
      data.table::rleid(eval(bool_ref)),
      FUN = cumsum
    )
  )
  return(df)
}
