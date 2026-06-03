library(testthat)
library(dplyr)

test_that("summarize_movement runs end-to-end on unsegmented track data", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geosphere")

  track <- data.frame(
    track_id = c(
      "A", "A", "A", "A", "A",
      "B", "B", "B", "B", "B"
    ),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00",
        "2024-01-01 04:00:00",
        "2024-01-02 00:00:00",
        "2024-01-02 01:00:00",
        "2024-01-02 02:00:00",
        "2024-01-02 03:00:00",
        "2024-01-02 04:00:00"
      ),
      tz = "UTC"
    ),
    at_colony = c(
      TRUE, FALSE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, FALSE, TRUE
    ),
    longitude = c(
      0, 0, 1, 2, 0,
      10, 10, 11, 12, 10
    ),
    latitude = c(
      0, 0, 0, 0, 0,
      10, 10, 10, 10, 10
    )
  )

  result <- summarize_movement(
    track = track,
    colony_coords = c(lon = 0, lat = 0),
    already_segmented = FALSE,
    classify_phases = TRUE,
    include_spatial = TRUE
  )

  expect_type(result, "list")

  expect_true("processed_track" %in% names(result))
  expect_true("trip_summary" %in% names(result))
  expect_true("trip_metrics" %in% names(result))
  expect_true("individual_metrics" %in% names(result))
  expect_true("population_metrics" %in% names(result))
  expect_true("centroids" %in% names(result))
  expect_true("foraging_ranges" %in% names(result))

  expect_s3_class(result$processed_track, "data.frame")
  expect_s3_class(result$trip_summary, "data.frame")
  expect_s3_class(result$trip_metrics, "data.frame")
  expect_s3_class(result$individual_metrics, "data.frame")
  expect_s3_class(result$population_metrics, "data.frame")

  expect_true("trip_id" %in% names(result$processed_track))
  expect_true("phase" %in% names(result$processed_track))

  expect_true("trip_distance_m" %in% names(result$trip_metrics))
  expect_true("trip_duration" %in% names(result$trip_metrics))
  expect_true("path_length_m" %in% names(result$trip_metrics))
  expect_true("max_distance_from_colony_m" %in% names(result$trip_metrics))

  expect_equal(nrow(result$trip_metrics), 2)
  expect_equal(nrow(result$individual_metrics), 2)
  expect_equal(result$population_metrics$n_individuals, 2)

  expect_s3_class(result$centroids, "sf")
})


test_that("summarize_movement works when track is already segmented", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geosphere")

  track <- data.frame(
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
    longitude = c(0, 0, 1, 0),
    latitude = c(0, 0, 0, 0)
  )

  result <- summarize_movement(
    track = track,
    colony_coords = c(lon = 0, lat = 0),
    already_segmented = TRUE,
    classify_phases = TRUE,
    include_spatial = FALSE
  )

  expect_type(result, "list")
  expect_true("trip_id" %in% names(result$processed_track))
  expect_true("phase" %in% names(result$processed_track))

  expect_equal(nrow(result$trip_metrics), 1)
  expect_equal(nrow(result$individual_metrics), 1)

  expect_null(result$centroids)
  expect_null(result$foraging_ranges)
})


test_that("summarize_movement can run without phase classification", {
  skip_if_not_installed("geosphere")

  track <- data.frame(
    track_id = c("A", "A", "A", "A"),
    datetime_gmt = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    at_colony = c(TRUE, FALSE, FALSE, TRUE),
    longitude = c(0, 0, 1, 0),
    latitude = c(0, 0, 0, 0)
  )

  result <- summarize_movement(
    track = track,
    colony_coords = c(lon = 0, lat = 0),
    already_segmented = FALSE,
    classify_phases = FALSE,
    include_spatial = FALSE
  )

  expect_type(result, "list")
  expect_true("trip_id" %in% names(result$processed_track))
  expect_false("phase" %in% names(result$processed_track))

  expect_equal(nrow(result$trip_metrics), 1)
  expect_equal(nrow(result$individual_metrics), 1)
})


test_that("summarize_movement errors when track is not a data frame", {
  expect_error(
    summarize_movement(
      track = "not a data frame",
      colony_coords = c(lon = 0, lat = 0)
    ),
    "`track` must be a data frame"
  )
})


test_that("summarize_movement errors for invalid colony coordinates", {
  track <- data.frame(
    track_id = c("A", "A"),
    datetime_gmt = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 01:00:00"),
      tz = "UTC"
    ),
    at_colony = c(TRUE, FALSE),
    longitude = c(0, 1),
    latitude = c(0, 0)
  )

  expect_error(
    summarize_movement(
      track = track,
      colony_coords = c(x = 0, y = 0)
    ),
    "`colony_coords` must be a named numeric vector"
  )
})


test_that("summarize_movement errors when required columns are missing", {
  track <- data.frame(
    track_id = c("A", "A"),
    datetime_gmt = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 01:00:00"),
      tz = "UTC"
    ),
    longitude = c(0, 1)
  )

  expect_error(
    summarize_movement(
      track = track,
      colony_coords = c(lon = 0, lat = 0),
      already_segmented = FALSE
    ),
    "Missing:"
  )
})


test_that("summarize_movement errors when already_segmented is TRUE but trip_id is missing", {
  track <- data.frame(
    track_id = c("A", "A"),
    datetime_gmt = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 01:00:00"),
      tz = "UTC"
    ),
    longitude = c(0, 1),
    latitude = c(0, 0)
  )

  expect_error(
    summarize_movement(
      track = track,
      colony_coords = c(lon = 0, lat = 0),
      already_segmented = TRUE
    ),
    "Missing:"
  )
})


test_that("summarize_movement respects custom column names", {
  skip_if_not_installed("geosphere")

  track <- data.frame(
    bird = c("A", "A", "A", "A"),
    time = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    colony = c(TRUE, FALSE, FALSE, TRUE),
    lon = c(0, 0, 1, 0),
    lat = c(0, 0, 0, 0)
  )

  result <- summarize_movement(
    track = track,
    colony_coords = c(lon = 0, lat = 0),
    already_segmented = FALSE,
    classify_phases = TRUE,
    bird_id_col = "bird",
    trip_id_col = "trip",
    datetime_col = "time",
    colony_flag_col = "colony",
    lon_col = "lon",
    lat_col = "lat",
    phase_col = "behavior",
    include_spatial = FALSE
  )

  expect_true("trip" %in% names(result$processed_track))
  expect_true("behavior" %in% names(result$processed_track))
  expect_true("trip_distance_m" %in% names(result$trip_metrics))
  expect_equal(nrow(result$trip_metrics), 1)
})
