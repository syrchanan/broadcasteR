#' @title Media Abbreviations
#'
#' @description This dataset gives translations between common media acronyms and abbreviations, to what they represent.
#'
#' @docType data
#'
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{abbreviation}{A commonly used abbreviation in the media world.}
#'   \item{meaning}{The longform explanation of what the short name represents.}
#' }
#' @source Nielsen Company.
"media_abbreviations"

#' @title Media Demo Conversions
#'
#' @description This dataset gives instructions on how to convert between certain media demo blocks.
#'
#' @docType data
#'
#' @format A data frame with 17 rows and 2 variables:
#' \describe{
#'   \item{target_demo}{A sex-agnostic demo age range.}
#'   \item{formula}{A string describing the operations needed to calculate the given demo by other demos.}
#' }
#' @source Nielsen Company.
"media_demo_conversions"
