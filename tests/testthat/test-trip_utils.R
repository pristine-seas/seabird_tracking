library(testthat)
library(dplyr)
library(lubridate)

test_that("make_datetime combines date and time strings correctly", {
  df <- data.frame(date_gmt = "2026-05-20", time_gmt = "14:30:00")
  res <- make_datetime(df)
  
  expect_true("datetime_gmt" %in% names(res))
  expect_s3_class(res$datetime_gmt, "POSIXct")
  expect_equal(res$datetime_gmt, as.POSIXct("2026-05-20 14:30:00", tz = "UTC"))
})

test_that("identify_colony_visits calculates distances and flags attendance", {
  df <- data.frame(
    longitude = c(0, 0.1), 
    latitude = c(0, 0.1),
    lon_colony = c(0, 0),
    lat_colony = c(0, 0)
  )
  
  res <- identify_colony_visits(df, radius_m = 5000)
  
  expect_true(res$at_colony[1]) # Exactly at colony
  expect_false(res$at_colony[2]) # Far away
  expect_type(res$dist_to_colony_m, "double")
})

test_that("segment_trips creates sequential trip IDs", {
  df <- data.frame(
    track_id = "B1",
    datetime_gmt = as.POSIXct("2026-05-20 10:00:00", tz="UTC") + (0:4)*3600,
    at_colony = c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  
  res <- segment_trips(df)
  
  # Expected pattern: NA (at colony), 1, 1 (trip 1), NA (at colony), 2 (trip 2)
  expect_equal(res$trip_id, c(NA, 1, 1, NA, 2))
})

test_that("classify_trip_phase calculates speed and categorizes behavior", {
  df <- data.frame(
    track_id = "B1",
    trip_id = c(1, 1, 1),
    datetime_gmt = as.POSIXct("2026-05-20 12:00:00", tz="UTC") + c(0, 3600, 7200),
    longitude = c(0, 0, 1), # Second step is a massive jump (commuting)
    latitude = c(0, 0, 0)   # First step is no movement (foraging/resting)
  )
  
  res <- classify_trip_phase(df, commute_speed_threshold = 5, forage_speed_threshold = 1)
  
  expect_equal(res$phase[1], "foraging")
  expect_equal(res$phase[2], "commuting")
  expect_equal(res$phase[3], "unknown") # Last point has no forward trajectory
})

test_that("summarize_trips aggregates data correctly", {
  df <- data.frame(
    track_id = c("B1", "B1", "B1"),
    trip_id = c(1, 1, NA),
    datetime_gmt = as.POSIXct("2026-05-20 12:00:00", tz="UTC") + c(0, 3600, 7200),
    dist_to_colony_m = c(1000, 2000, 0),
    phase = c("commuting", "foraging", "colony")
  )
  
  res <- summarize_trips(df)
  
  expect_equal(nrow(res), 1) # Only 1 valid trip
  expect_equal(res$duration_h, 1) # 3600 seconds = 1 hour
  expect_equal(res$n_fixes, 2)
  expect_equal(res$max_dist_to_colony_m, 2000)
})