library(testthat)
library(dplyr)

test_that("calc_trip_distance calculates straight-line trip distance", {
  trip_data <- data.frame(
    track_id = c("A", "A", "A", "B", "B"),
    trip_id = c(1, 1, 1, 1, 1),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00"
      ),
      tz = "UTC"
    ),
    longitude = c(0, 0.5, 1, 0, 0),
    latitude = c(0, 0, 0, 0, 1)
  )

  result <- calc_trip_distance(trip_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("trip_distance_m" %in% names(result))
  expect_true(all(result$trip_distance_m > 0))

  expected_a <- geosphere::distHaversine(c(0, 0), c(1, 0))
  observed_a <- result$trip_distance_m[result$track_id == "A"]

  expect_equal(observed_a, expected_a, tolerance = 1)
})


test_that("calc_trip_distance ignores colony rows with NA trip_id", {
  trip_data <- data.frame(
    track_id = c("A", "A", "A", "A"),
    trip_id = c(NA, 1, 1, NA),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    longitude = c(0, 0, 1, 2),
    latitude = c(0, 0, 0, 0)
  )

  result <- calc_trip_distance(trip_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$trip_id, 1)
})


test_that("calc_trip_distance errors when required columns are missing", {
  bad_data <- data.frame(
    track_id = "A",
    trip_id = 1,
    longitude = 0,
    latitude = 0
  )

  expect_error(
    calc_trip_distance(bad_data),
    "Missing:"
  )
})


test_that("calc_trip_duration calculates duration in hours", {
  trip_data <- data.frame(
    track_id = c("A", "A", "A", "B", "B"),
    trip_id = c(1, 1, 1, 1, 1),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 03:00:00",
        "2024-01-02 00:00:00",
        "2024-01-02 05:00:00"
      ),
      tz = "UTC"
    )
  )

  result <- calc_trip_duration(trip_data, units = "hours")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("trip_duration" %in% names(result))

  duration_a <- result$trip_duration[result$track_id == "A"]
  duration_b <- result$trip_duration[result$track_id == "B"]

  expect_equal(duration_a, 3)
  expect_equal(duration_b, 5)
})


test_that("calc_trip_duration supports minutes", {
  trip_data <- data.frame(
    track_id = c("A", "A"),
    trip_id = c(1, 1),
    datetime_gmt = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 01:30:00"),
      tz = "UTC"
    )
  )

  result <- calc_trip_duration(trip_data, units = "mins")

  expect_equal(result$trip_duration, 90)
})


test_that("calc_trip_duration errors for invalid units", {
  trip_data <- data.frame(
    track_id = c("A", "A"),
    trip_id = c(1, 1),
    datetime_gmt = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 01:00:00"),
      tz = "UTC"
    )
  )

  expect_error(
    calc_trip_duration(trip_data, units = "weeks"),
    "`units` must be one of"
  )
})


test_that("calc_trip_duration errors when required columns are missing", {
  bad_data <- data.frame(
    track_id = "A",
    trip_id = 1
  )

  expect_error(
    calc_trip_duration(bad_data),
    "Missing:"
  )
})


test_that("calc_path_length sums step distances within each trip", {
  track_data <- data.frame(
    track_id = c("A", "A", "A"),
    trip_id = c(1, 1, 1),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00"
      ),
      tz = "UTC"
    ),
    longitude = c(0, 1, 2),
    latitude = c(0, 0, 0)
  )

  result <- calc_path_length(track_data)

  expected <- geosphere::distHaversine(c(0, 0), c(1, 0)) +
    geosphere::distHaversine(c(1, 0), c(2, 0))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("path_length_m" %in% names(result))
  expect_equal(result$path_length_m, expected, tolerance = 1)
})


test_that("calc_path_length can calculate by bird only when trip_id_col is NULL", {
  track_data <- data.frame(
    track_id = c("A", "A", "A"),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00"
      ),
      tz = "UTC"
    ),
    longitude = c(0, 1, 2),
    latitude = c(0, 0, 0)
  )

  result <- calc_path_length(
    track_data,
    trip_id_col = NULL
  )

  expect_equal(nrow(result), 1)
  expect_true("track_id" %in% names(result))
  expect_false("trip_id" %in% names(result))
  expect_true(result$path_length_m > 0)
})


test_that("calc_path_length ignores rows with NA trip_id when trip_id_col is used", {
  track_data <- data.frame(
    track_id = c("A", "A", "A", "A"),
    trip_id = c(NA, 1, 1, NA),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    longitude = c(0, 0, 1, 2),
    latitude = c(0, 0, 0, 0)
  )

  result <- calc_path_length(track_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$trip_id, 1)
})


test_that("calc_path_length errors when required columns are missing", {
  bad_data <- data.frame(
    track_id = "A",
    trip_id = 1,
    longitude = 0
  )

  expect_error(
    calc_path_length(bad_data),
    "Missing:"
  )
})


test_that("calc_max_distance_from_colony calculates maximum distance by trip", {
  trip_data <- data.frame(
    track_id = c("A", "A", "A"),
    trip_id = c(1, 1, 1),
    longitude = c(0, 1, 2),
    latitude = c(0, 0, 0)
  )

  result <- calc_max_distance_from_colony(
    trip_data,
    colony_coords = c(lon = 0, lat = 0)
  )

  expected <- geosphere::distHaversine(c(0, 0), c(2, 0))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("max_distance_from_colony_m" %in% names(result))
  expect_equal(result$max_distance_from_colony_m, expected, tolerance = 1)
})


test_that("calc_max_distance_from_colony can calculate by bird only", {
  trip_data <- data.frame(
    track_id = c("A", "A", "A"),
    longitude = c(0, 1, 2),
    latitude = c(0, 0, 0)
  )

  result <- calc_max_distance_from_colony(
    trip_data,
    colony_coords = c(lon = 0, lat = 0),
    trip_id_col = NULL
  )

  expect_equal(nrow(result), 1)
  expect_true("track_id" %in% names(result))
  expect_false("trip_id" %in% names(result))
  expect_true(result$max_distance_from_colony_m > 0)
})


test_that("calc_max_distance_from_colony errors for invalid colony coordinates", {
  trip_data <- data.frame(
    track_id = c("A", "A"),
    trip_id = c(1, 1),
    longitude = c(0, 1),
    latitude = c(0, 0)
  )

  expect_error(
    calc_max_distance_from_colony(
      trip_data,
      colony_coords = c(x = 0, y = 0)
    ),
    "`colony_coords` must be a named numeric vector"
  )
})


test_that("calc_max_distance_from_colony errors when required columns are missing", {
  bad_data <- data.frame(
    track_id = "A",
    trip_id = 1,
    longitude = 0
  )

  expect_error(
    calc_max_distance_from_colony(
      bad_data,
      colony_coords = c(lon = 0, lat = 0)
    ),
    "Missing:"
  )
})
