#' Calculate the Universal Estimate
#'
#' @description The total number of persons or homes in a population.
#'
#' @param impressions The total number of households or persons exposed to an advertising schedule.
#' Measured in thousands (000s).
#'
#' @param rating The percentage of a specific population group which is tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param ue The total number of persons or homes in a population.
#' Measured in thousands (000s).
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_ue <- function(impressions = NA, rating = NA, ue = NA) {
  if (is.na(impressions)) {
    result <- ue * rating
  } else if (is.na(rating)) {
    result <- impressions / ue
  } else if (is.na(ue)) {
    result <- impressions / rating
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate the Coverage
#'
#' @description The percentage of homes or persons able to receive an individual network or channel.
#'
#' @param cvg_projection The projected coverage, measured in thousands (000s).
#'
#' @param cvg_ue The universal estimate of coverage, measured in thousands (000s).
#'
#' @param cvg_rating The percentage of homes or persons able to receive an individual network or channel. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_coverage <- function(cvg_projection = NA, cvg_ue = NA, cvg_rating = NA) {
  if (is.na(cvg_projection)) {
    result <- cvg_ue * cvg_rating
  } else if (is.na(cvg_ue)) {
    result <- cvg_projection / cvg_rating
  } else if (is.na(cvg_rating)) {
    result <- cvg_projection / cvg_ue
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Households Using Television (HUT)
#'
#' @description The percentage of Total Television Households that are viewing Television during a given period.
#'
#' @param hh_rating The percentage of a households which are tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param share The percentage of Households Viewing Television that are tuned to a particular
#' program/network during average minute of program or daypart. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @param hut The percentage of Total Television Households that are viewing Television during a given period. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_hut_1 <- function(hh_rating = NA, share = NA, hut = NA) {
  if (is.na(hh_rating)) {
    result <- hut * share
  } else if (is.na(share)) {
    result <- hh_rating / hut
  } else if (is.na(hut)) {
    result <- hh_rating / share
  } else {
    result <- NA
  }

  return(result)
}

#' @param hh_in_use The number of households with their TV sets in use.
#'
#' @param total_hh_ue The total Universal Estimate for households.
#'
#' @rdname media_hut_1
#'
#' @export
media_hut_2 <- function(hh_in_use = NA, total_hh_ue = NA, hut = NA) {
  if (is.na(hh_in_use)) {
    result <- total_hh_ue * hut
  } else if (is.na(total_hh_ue)) {
    result <- hh_in_use / hut
  } else if (is.na(hut)) {
    result <- hh_in_use / total_hh_ue
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Persons Using Television (PUT)
#'
#' @description The percentage of Total Persons in a particular demographic group that are viewing Television during a given time period.
#'
#' @param persons_viewing The number of persons viewing TV.
#'
#' @param total_person_ue The total Universal Estimate of persons who watch TV.
#'
#' @param put The percentage of Total Persons in a particular demographic group that are viewing Television during a given time period. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_put_1 <- function(persons_viewing = NA, total_person_ue = NA, put = NA) {
  if (is.na(persons_viewing)) {
    result <- total_person_ue * put
  } else if (is.na(total_person_ue)) {
    result <- persons_viewing / put
  } else if (is.na(put)) {
    result <- persons_viewing / total_person_ue
  } else {
    result <- NA
  }

  return(result)
}

#' @param demo_rating The rating percentage of a program within a certain demographic. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @param demo_share The share percentage of a program within a specific demographic. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @rdname media_put_1
#'
#' @export
media_put_2 <- function(demo_rating = NA, demo_share = NA, put = NA) {
  if (is.na(demo_rating)) {
    result <- demo_share * put
  } else if (is.na(demo_share)) {
    result <- demo_rating / put
  } else if (is.na(put)) {
    result <- demo_rating / demo_share
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate the Share
#'
#' @description The Percent of Households Viewing Television that are tuned to a particular program/network during average minute of program or daypart
#'
#' @param rating The percentage of a specific population group which is tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param hut The percentage of Total Television Households that are viewing Television during a given period. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @param share The Percent of Households Viewing Television that are tuned to a particular program/network during average minute of program or daypart. Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_share <- function(rating = NA, hut = NA, share = NA) {
  if (is.na(rating)) {
    result <- hut * share
  } else if (is.na(hut)) {
    result <- rating / share
  } else if (is.na(share)) {
    result <- rating / hut
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate the Average Audience (AA)
#'
#' @description Average Audience reflects viewing for an average minute to a program and is an average of the audience at the specific minute (Min. 1, Min. 2, Min. 3, etc…).
#'
#' Note: Impressions can be added together across demos, dayparts or stations/sources (ex: M18-49 + F18-49 = P18-49)
#'
#' @param rating The percentage of a specific population group which is tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param ue The total number of persons or homes in a population.
#' Measured in thousands (000s).
#'
#' @param aa The average viewing for a program. Can be expressed as a projection (000s) or as a rating (%).
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_aa_1 <- function(rating = NA, ue = NA, aa = NA) {
  if (is.na(aa)) {
    result <- rating * ue
  } else if (is.na(ue)) {
    result <- aa / rating
  } else if (is.na(rating)) {
    result <- aa / ue
  } else {
    result <- NA
  }

  return(result)
}

#' @param vpvh A measure of a program’s audience composition (or profile) relative to the Households tuned to the program.
#'
#' @param hh The projection of the household universal estimate. Measured in thousands (000s).
#'
#' @rdname media_aa_1
#'
#' @export
media_aa_2 <- function(vpvh = NA, hh = NA, aa = NA) {
  if (is.na(aa)) {
    result <- vpvh * hh
  } else if (is.na(hh)) {
    result <- aa / vpvh
  } else if (is.na(vpvh)) {
    result <- aa / hh
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate the Average Audience (AA) Rating
#'
#' @description The estimate size of the Television audience relative to the total universe, expressed as a percentage. The percent of all TV Households or persons tuned to a specific station.
#'
#' Ratings are NOT additive across different Demos; Ratings must be weight-average when combined.
#'
#' @param share The Percent of Households Viewing Television that are tuned to a particular program/network during average minute of program or daypart. Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param hut The percentage of Total Television Households that are viewing Television during a given period. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @param aa_rating The percent of all TV Households or persons tuned to a specific station. Typically expressed as a percentage (%), but returned as a decimal.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_aa_rating_1 <- function(share = NA, hut = NA, aa_rating = NA) {
  if (is.na(aa_rating)) {
    result <- share * hut
  } else if (is.na(hut)) {
    result <- aa_rating / share
  } else if (is.na(share)) {
    result <- aa_rating / hut
  } else {
    result <- NA
  }

  return(result)
}

#' @param aa The projection of average viewing for a program. Measured in thousands (000s).
#'
#' @param ue The total number of persons or homes in a population.
#' Measured in thousands (000s).
#'
#' @rdname media_aa_rating_1
#'
#' @export
media_aa_rating_2 <- function(aa = NA, ue = NA, aa_rating = NA) {
  if (is.na(aa)) {
    result <- aa_rating * ue
  } else if (is.na(ue)) {
    result <- aa / aa_rating
  } else if (is.na(aa_rating)) {
    result <- aa / ue
  } else {
    result <- NA
  }

  return(result)
}

#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @param num_spots The number of advertising/program spots.
#'
#' @rdname media_aa_rating_1
#'
#' @export
media_aa_rating_3 <- function(grps = NA, num_spots = NA, aa_rating = NA) {
  if (is.na(grps)) {
    result <- aa_rating * num_spots
  } else if (is.na(num_spots)) {
    result <- grps / aa_rating
  } else if (is.na(aa_rating)) {
    result <- grps / num_spots
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Gross Impressions
#'
#' @description The total number of Households, or Persons, exposed to an advertising schedule.
#'
#' @param gross_imp The total number of Households, or Persons, exposed to an advertising schedule. Measured in thousands (000s).
#'
#' @param ue The total number of persons or homes in a population.
#' Measured in thousands (000s).
#'
#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_gross_imp <- function(gross_imp = NA, ue = NA, grps = NA) {
  if (is.na(gross_imp)) {
    result <- ue * grps
  } else if (is.na(ue)) {
    result <- gross_imp / grps
  } else if (is.na(grps)) {
    result <- gross_imp / ue
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Gross Rating Points (GRPs)
#'
#' @description The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' Note:
#' * The sum of all rating points in a given schedule
#' * Takes into account duplication
#' * Can exceed 100
#' * Describes the amount of media weight
#'
#' @param impressions The total number of Households, or Persons, exposed to an advertising schedule. Measured in thousands (000s).
#'
#' @param ue The total number of persons or homes in a population.
#' Measured in thousands (000s).
#'
#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_grp_1 <- function(impressions = NA, ue = NA, grps = NA) {
  if (is.na(impressions)) {
    result <- ue * grps
  } else if (is.na(ue)) {
    result <- impressions / grps
  } else if (is.na(grps)) {
    result <- impressions / ue
  } else {
    result <- NA
  }

  return(result)
}

#' @param rating The percentage of a specific population group which is tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param num_spots The number of advertising/program spots.
#'
#' @rdname media_grp_1
#'
#' @export
media_grp_2 <- function(rating = NA, num_spots = NA, grps = NA) {
  if (is.na(grps)) {
    result <- rating * num_spots
  } else if (is.na(rating)) {
    result <- grps / num_spots
  } else if (is.na(num_spots)) {
    result <- grps / rating
  } else {
    result <- NA
  }

  return(result)
}

#' @param reach The number of different households or persons who are exposed at least once to a program, daypart, or advertising schedule over a given period of time; Also referred to as Cume, Unduplicated Audience or Net Audience.
#'
#' @param frequency The average number of times that each Household (or Person) is exposed to an advertising schedule or campaign. (Expressed as an absolute number).
#'
#' @rdname media_grp_1
#'
#' @export
media_grp_3 <- function(reach = NA, frequency = NA, grps = NA) {
  if (is.na(grps)) {
    result <- reach * frequency
  } else if (is.na(reach)) {
    result <- grps / frequency
  } else if (is.na(frequency)) {
    result <- grps / reach
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Reach
#'
#' @description The number of different households or persons who are exposed at least once to a program, daypart, or advertising schedule over a given period of time; Also referred to as Cume, Unduplicated Audience or Net Audience.
#'
#' Note:
#'
#' * Does not take into account duplication
#' * Usually expressed as a percent
#' * Can never exceed 100%
#' * A one time rating is a reach
#'
#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @param frequency The average number of times that each Household (or Person) is exposed to an advertising schedule or campaign. (Expressed as an absolute number).
#'
#' @param reach The number of different households or persons who are exposed at least once to a program, daypart, or advertising schedule over a given period of time.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_reach <- function(grps = NA, frequency = NA, reach = NA) {
  if (is.na(grps)) {
    result <- reach * frequency
  } else if (is.na(reach)) {
    result <- grps / frequency
  } else if (is.na(frequency)) {
    result <- grps / reach
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Frequency
#'
#' @description The average number of times that each Household (or Person) is exposed to an advertising schedule or campaign. (Expressed as an absolute number).
#'
#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @param frequency The average number of times that each Household (or Person) is exposed to an advertising schedule or campaign.
#'
#' @param reach The number of different households or persons who are exposed at least once to a program, daypart, or advertising schedule over a given period of time.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_frequency <- function(grps = NA, frequency = NA, reach = NA) {
  if (is.na(grps)) {
    result <- reach * frequency
  } else if (is.na(reach)) {
    result <- grps / frequency
  } else if (is.na(frequency)) {
    result <- grps / reach
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Viewers per Viewing Household (VPVHs)
#'
#' @description A measure of a program’s audience composition (or profile) relative to the Households tuned to the program.
#'
#' Note:
#' * Shows the audience skew of a program/network
#' * Can be expressed in Hundreds (.10) or Thousands (.100)
#' * Is additive between demographics
#' * Used in estimating a program’s target audience
#'
#' @param persons The projected number of persons viewing a program. Expressed in the thousands (000s).
#'
#' @param hh The projected number of households viewing a program. Expressed in the thousands (000s).
#'
#' @param vpvh A measure of a program’s audience composition (or profile) relative to the Households tuned to the program.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_vpvh <- function(persons = NA, hh = NA, vpvh = NA) {
  if (is.na(persons)) {
    result <- hh * vpvh
  } else if (is.na(hh)) {
    result <- persons / vpvh
  } else if (is.na(vpvh)) {
    result <- persons / hh
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Average Hours of Viewing
#'
#' @description HUT / PUT converted to the average hours of viewing per home or per person.
#'
#' @param duration Duration of period.
#'
#' @param hut_put The percentage of Total Persons / Households in a particular demographic group that are viewing Television during a given time period.
#'
#' @param avg_hours HUT / PUT converted to the average hours of viewing per home or per person.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_avg_hours <- function(duration = NA, hut_put = NA, avg_hours = NA) {
  if (is.na(avg_hours)) {
    result <- duration * hut_put
  } else if (is.na(duration)) {
    result <- avg_hours / hut_put
  } else if (is.na(hut_put)) {
    result <- avg_hours / duration
  } else {
    result <- NA
  }

  return(result)
}

#' Calculate Cost per Thousand (CPM)
#'
#' @description The cost of delivering One Thousand Impressions within a defined population group.
#'
#' Note:
#' * Basic Formula of negotiation among cable networks
#' * Measures efficiencies of media schedule
#' * Allows for cross media evaluation
#'
#' In other words, the CPM can be boiled down to a tongue-in-cheek formula:
#'       CPM = \eqn{\frac{Bucks}{Schmucks}}
#'
#' @param cost Media cost (in dollars).
#'
#' @param gross_aa Gross audience (AA) who watched the program or advertisement. Measured in thousands (000s).
#'
#' @param cpm The cost of delivering One Thousand Impressions within a defined population group.
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_cpm <- function(cost = NA, gross_aa = NA, cpm = NA) {
  if (is.na(cost)) {
    result <- gross_aa * cpm
  } else if (is.na(gross_aa)) {
    result <- cost / cpm
  } else if (is.na(cpm)) {
    result <- cost / gross_aa
  } else {
    result <- NA
  }

  return(result)
}

#' Cost Per Point (CPP)
#'
#' @description The cost to deliver a single rating point (1% of the defined population).
#'
#' @param u_cost Average unit cost (in dollars).
#'
#' @param rating The percentage of a specific population group which is tuned to the Average Minute of a Program or Daypart.
#'
#' * One Rating Point = 1% of the Population.
#' * Five Rating Points = 5% of the Population
#' * Ten Rating Points = 10% of the Population
#'
#' Typically is expressed as a percentage (%), but input as a decimal.
#'
#' @param cpp The cost to deliver a single rating point (1% of the defined population).
#'
#' @return The parameter whose input was left as `NA`.
#'
#' @export
media_cpp_1 <- function(u_cost = NA, rating = NA, cpp = NA) {
  if (is.na(u_cost)) {
    result <- rating * cpp
  } else if (is.na(rating)) {
    result <- u_cost / cpp
  } else if (is.na(cpp)) {
    result <- u_cost / rating
  } else {
    result <- NA
  }

  return(result)
}

#' @param s_cost Total Schedule Cost (in dollars).
#'
#' @param grps The sum of all ratings for all programs in an advertising schedule. One rating point equals one percent of total audience (universe).
#'
#' @rdname media_cpp_1
#'
#' @export
media_cpp_2 <- function(s_cost = NA, grps = NA, cpp = NA) {
  if (is.na(s_cost)) {
    result <- grps * cpp
  } else if (is.na(grps)) {
    result <- s_cost / cpp
  } else if (is.na(cpp)) {
    result <- s_cost / grps
  } else {
    result <- NA
  }

  return(result)
}

#' Get Source
#'
#' @description The source of all formulas and definitions in this package.
#'
#' @export
media_attribution <- function() {
  cat("https://thevab.com/storage/app/media/Toolkit/mediaterminologyformulas.pdf")
}
