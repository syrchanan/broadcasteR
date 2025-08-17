# file: tests/testthat/test-media-formulas.R

# Load the testthat package and the functions
library(testthat)
library(connoR)

test_that("media_ue calculates missing values correctly", {
  # Test when impressions is NA
  expect_equal(media_ue(impressions = NA, rating = 0.5, ue = 2000), 1000)
  # Test when rating is NA
  expect_equal(media_ue(impressions = 1000, rating = NA, ue = 2000), 0.5)
  # Test when ue is NA
  expect_equal(media_ue(impressions = 1000, rating = 0.5, ue = NA), 2000)
  # Test error condition
  expect_error(
    media_ue(impressions = 1000, rating = 0.5, ue = 2000),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_ue(impressions = NA, rating = NA, ue = NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_coverage calculates missing values correctly", {
  # Test when cvg_projection is NA
  expect_equal(
    media_coverage(cvg_projection = NA, cvg_ue = 2000, cvg_rating = 0.8),
    1600
  )
  # Test when cvg_ue is NA
  expect_equal(
    media_coverage(cvg_projection = 1600, cvg_ue = NA, cvg_rating = 0.8),
    2000
  )
  # Test when cvg_rating is NA
  expect_equal(
    media_coverage(cvg_projection = 1600, cvg_ue = 2000, cvg_rating = NA),
    0.8
  )
  # Test error condition
  expect_error(
    media_coverage(10, 20, 30),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_coverage(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_hut_1 calculates missing values correctly", {
  # Test when hh_rating is NA
  expect_equal(media_hut_1(hh_rating = NA, share = 0.25, hut = 0.5), 0.125)
  # Test when share is NA
  expect_equal(media_hut_1(hh_rating = 0.125, share = NA, hut = 0.5), 0.25)
  # Test when hut is NA
  expect_equal(media_hut_1(hh_rating = 0.125, share = 0.25, hut = NA), 0.5)
  # Test error condition
  expect_error(media_hut_1(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_hut_1(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_hut_2 calculates missing values correctly", {
  # Test when hh_in_use is NA
  expect_equal(media_hut_2(hh_in_use = NA, total_hh_ue = 5000, hut = 0.6), 3000)
  # Test when total_hh_ue is NA
  expect_equal(media_hut_2(hh_in_use = 3000, total_hh_ue = NA, hut = 0.6), 5000)
  # Test when hut is NA
  expect_equal(media_hut_2(hh_in_use = 3000, total_hh_ue = 5000, hut = NA), 0.6)
  # Test error condition
  expect_error(media_hut_2(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_hut_2(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_put_1 calculates missing values correctly", {
  # Test when persons_viewing is NA
  expect_equal(
    media_put_1(persons_viewing = NA, total_person_ue = 8000, put = 0.4),
    3200
  )
  # Test when total_person_ue is NA
  expect_equal(
    media_put_1(persons_viewing = 3200, total_person_ue = NA, put = 0.4),
    8000
  )
  # Test when put is NA
  expect_equal(
    media_put_1(persons_viewing = 3200, total_person_ue = 8000, put = NA),
    0.4
  )
  # Test error condition
  expect_error(media_put_1(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_put_1(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_put_2 calculates missing values correctly", {
  # Test when demo_rating is NA
  expect_equal(
    media_put_2(demo_rating = NA, demo_share = 0.2, put = 0.35),
    0.07
  )
  # Test when demo_share is NA
  expect_equal(
    media_put_2(demo_rating = 0.07, demo_share = NA, put = 0.35),
    0.2
  )
  # Test when put is NA
  expect_equal(
    media_put_2(demo_rating = 0.07, demo_share = 0.2, put = NA),
    0.35
  )
  # Test error condition
  expect_error(media_put_2(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_put_2(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_share calculates missing values correctly", {
  # Test when rating is NA
  expect_equal(media_share(rating = NA, hut = 0.5, share = 0.25), 0.125)
  # Test when hut is NA
  expect_equal(media_share(rating = 0.125, hut = NA, share = 0.25), 0.5)
  # Test when share is NA
  expect_equal(media_share(rating = 0.125, hut = 0.5, share = NA), 0.25)
  # Test error condition
  expect_error(media_share(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_share(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_aa_1 calculates missing values correctly", {
  # Test when aa is NA
  expect_equal(media_aa_1(rating = 0.2, ue = 5000, aa = NA), 1000)
  # Test when ue is NA
  expect_equal(media_aa_1(rating = 0.2, ue = NA, aa = 1000), 5000)
  # Test when rating is NA
  expect_equal(media_aa_1(rating = NA, ue = 5000, aa = 1000), 0.2)
  # Test error condition
  expect_error(media_aa_1(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_aa_1(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_aa_2 calculates missing values correctly", {
  # Test when aa is NA
  expect_equal(media_aa_2(vpvh = 2.5, hh = 5000, aa = NA), 12500)
  # Test when hh is NA
  expect_equal(media_aa_2(vpvh = 2.5, hh = NA, aa = 12500), 5000)
  # Test when vpvh is NA
  expect_equal(media_aa_2(vpvh = NA, hh = 5000, aa = 12500), 2.5)
  # Test error condition
  expect_error(media_aa_2(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_aa_2(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_aa_rating_1 calculates missing values correctly", {
  # Test when aa_rating is NA
  expect_equal(
    media_aa_rating_1(share = 0.25, hut = 0.5, aa_rating = NA),
    0.125
  )
  # Test when hut is NA
  expect_equal(
    media_aa_rating_1(share = 0.25, hut = NA, aa_rating = 0.125),
    0.5
  )
  # Test when share is NA
  expect_equal(
    media_aa_rating_1(share = NA, hut = 0.5, aa_rating = 0.125),
    0.25
  )
  # Test error condition
  expect_error(
    media_aa_rating_1(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_aa_rating_1(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_aa_rating_2 calculates missing values correctly", {
  # Test when aa is NA
  expect_equal(media_aa_rating_2(aa = NA, ue = 5000, aa_rating = 0.2), 1000)
  # Test when ue is NA
  expect_equal(media_aa_rating_2(aa = 1000, ue = NA, aa_rating = 0.2), 5000)
  # Test when aa_rating is NA
  expect_equal(media_aa_rating_2(aa = 1000, ue = 5000, aa_rating = NA), 0.2)
  # Test error condition
  expect_error(
    media_aa_rating_2(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_aa_rating_2(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_aa_rating_3 calculates missing values correctly", {
  # Test when grps is NA
  expect_equal(media_aa_rating_3(grps = NA, num_spots = 10, aa_rating = 0.5), 5)
  # Test when num_spots is NA
  expect_equal(media_aa_rating_3(grps = 5, num_spots = NA, aa_rating = 0.5), 10)
  # Test when aa_rating is NA
  expect_equal(media_aa_rating_3(grps = 5, num_spots = 10, aa_rating = NA), 0.5)
  # Test error condition
  expect_error(
    media_aa_rating_3(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_aa_rating_3(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_gross_imp calculates missing values correctly", {
  # Test when gross_imp is NA
  expect_equal(media_gross_imp(gross_imp = NA, ue = 1000, grps = 50), 50000)
  # Test when ue is NA
  expect_equal(media_gross_imp(gross_imp = 50000, ue = NA, grps = 50), 1000)
  # Test when grps is NA
  expect_equal(media_gross_imp(gross_imp = 50000, ue = 1000, grps = NA), 50)
  # Test error condition
  expect_error(
    media_gross_imp(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_gross_imp(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_grp_1 calculates missing values correctly", {
  # Test when impressions is NA
  expect_equal(media_grp_1(impressions = NA, ue = 1000, grps = 50), 50000)
  # Test when ue is NA
  expect_equal(media_grp_1(impressions = 50000, ue = NA, grps = 50), 1000)
  # Test when grps is NA
  expect_equal(media_grp_1(impressions = 50000, ue = 1000, grps = NA), 50)
  # Test error condition
  expect_error(media_grp_1(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_grp_1(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_grp_2 calculates missing values correctly", {
  # Test when grps is NA
  expect_equal(media_grp_2(rating = 0.5, num_spots = 10, grps = NA), 5)
  # Test when rating is NA
  expect_equal(media_grp_2(rating = NA, num_spots = 10, grps = 5), 0.5)
  # Test when num_spots is NA
  expect_equal(media_grp_2(rating = 0.5, num_spots = NA, grps = 5), 10)
  # Test error condition
  expect_error(media_grp_2(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_grp_2(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_grp_3 calculates missing values correctly", {
  # Test when grps is NA
  expect_equal(media_grp_3(reach = 20, frequency = 2.5, grps = NA), 50)
  # Test when reach is NA
  expect_equal(media_grp_3(reach = NA, frequency = 2.5, grps = 50), 20)
  # Test when frequency is NA
  expect_equal(media_grp_3(reach = 20, frequency = NA, grps = 50), 2.5)
  # Test error condition
  expect_error(media_grp_3(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_grp_3(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_reach calculates missing values correctly", {
  # Test when grps is NA
  expect_equal(media_reach(grps = NA, frequency = 2.5, reach = 20), 50)
  # Test when reach is NA
  expect_equal(media_reach(grps = 50, frequency = 2.5, reach = NA), 20)
  # Test when frequency is NA
  expect_equal(media_reach(grps = 50, frequency = NA, reach = 20), 2.5)
  # Test error condition
  expect_error(media_reach(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_reach(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_frequency calculates missing values correctly", {
  # Test when grps is NA
  expect_equal(media_frequency(grps = NA, frequency = 2.5, reach = 20), 50)
  # Test when reach is NA
  expect_equal(media_frequency(grps = 50, frequency = 2.5, reach = NA), 20)
  # Test when frequency is NA
  expect_equal(media_frequency(grps = 50, frequency = NA, reach = 20), 2.5)
  # Test error condition
  expect_error(
    media_frequency(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_frequency(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_vpvh calculates missing values correctly", {
  # Test when persons is NA
  expect_equal(media_vpvh(persons = NA, hh = 5000, vpvh = 2.5), 12500)
  # Test when hh is NA
  expect_equal(media_vpvh(persons = 12500, hh = NA, vpvh = 2.5), 5000)
  # Test when vpvh is NA
  expect_equal(media_vpvh(persons = 12500, hh = 5000, vpvh = NA), 2.5)
  # Test error condition
  expect_error(media_vpvh(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_vpvh(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_avg_hours calculates missing values correctly", {
  # Test when avg_hours is NA
  expect_equal(
    media_avg_hours(duration = 24, hut_put = 0.5, avg_hours = NA),
    12
  )
  # Test when duration is NA
  expect_equal(
    media_avg_hours(duration = NA, hut_put = 0.5, avg_hours = 12),
    24
  )
  # Test when hut_put is NA
  expect_equal(
    media_avg_hours(duration = 24, hut_put = NA, avg_hours = 12),
    0.5
  )
  # Test error condition
  expect_error(
    media_avg_hours(1, 2, 3),
    "Leave only one parameter equal to NA."
  )
  expect_error(
    media_avg_hours(NA, NA, NA),
    "Leave only one parameter equal to NA."
  )
})

test_that("media_cpm calculates missing values correctly", {
  # Test when cost is NA
  expect_equal(media_cpm(cost = NA, gross_aa = 50, cpm = 20), 1000)
  # Test when gross_aa is NA
  expect_equal(media_cpm(cost = 1000, gross_aa = NA, cpm = 20), 50)
  # Test when cpm is NA
  expect_equal(media_cpm(cost = 1000, gross_aa = 50, cpm = NA), 20)
  # Test error condition
  expect_error(media_cpm(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_cpm(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_cpp_1 calculates missing values correctly", {
  # Test when u_cost is NA
  expect_equal(media_cpp_1(u_cost = NA, rating = 0.5, cpp = 200), 100)
  # Test when rating is NA
  expect_equal(media_cpp_1(u_cost = 100, rating = NA, cpp = 200), 0.5)
  # Test when cpp is NA
  expect_equal(media_cpp_1(u_cost = 100, rating = 0.5, cpp = NA), 200)
  # Test error condition
  expect_error(media_cpp_1(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_cpp_1(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_cpp_2 calculates missing values correctly", {
  # Test when s_cost is NA
  expect_equal(media_cpp_2(s_cost = NA, grps = 50, cpp = 200), 10000)
  # Test when grps is NA
  expect_equal(media_cpp_2(s_cost = 10000, grps = NA, cpp = 200), 50)
  # Test when cpp is NA
  expect_equal(media_cpp_2(s_cost = 10000, grps = 50, cpp = NA), 200)
  # Test error condition
  expect_error(media_cpp_2(1, 2, 3), "Leave only one parameter equal to NA.")
  expect_error(media_cpp_2(NA, NA, NA), "Leave only one parameter equal to NA.")
})

test_that("media_attribution prints the correct source", {
  # Capture the output and check if it matches the expected string
  expect_output(
    media_attribution(),
    "https://thevab.com/storage/app/media/Toolkit/mediaterminologyformulas.pdf"
  )
})
