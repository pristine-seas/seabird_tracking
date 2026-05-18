library(testthat)
library(dplyr)

test_that("calc_track_centroid returns an sf object with one centroid per bird", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "B", "B"),
    longitude = c(0, 2, 10, 12),
    latitude = c(0, 0, 10, 10)
  )

  result <- calc_track_centroid(track_data)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_true("track_id" %in% names(result))
  expect_true("n_points" %in% names(result))
  expect_equal(sort(result$n_points), c(2, 2))
})


test_that("calc_track_centroid can calculate centroids by bird and trip", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "A", "A"),
    trip_id = c(1, 1, 2, 2),
    longitude = c(0, 2, 10, 12),
    latitude = c(0, 0, 10, 10)
  )

  result <- calc_track_centroid(
    track_data,
    trip_id_col = "trip_id"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_true("trip_id" %in% names(result))
  expect_equal(sort(result$trip_id), c(1, 2))
})


test_that("calc_track_centroid works with existing sf input", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A"),
    longitude = c(0, 2),
    latitude = c(0, 0)
  )

  track_sf <- sf::st_as_sf(
    track_data,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  result <- calc_track_centroid(track_sf)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
})


test_that("calc_track_centroid errors when required columns are missing", {
  skip_if_not_installed("sf")

  bad_data <- data.frame(
    track_id = "A",
    longitude = 0
  )

  expect_error(
    calc_track_centroid(bad_data),
    "Missing:"
  )
})


test_that("calc_foraging_range returns convex hull range by bird", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "A", "B", "B", "B"),
    longitude = c(0, 1, 0, 10, 11, 10),
    latitude = c(0, 0, 1, 10, 10, 11)
  )

  result <- calc_foraging_range(
    track_data,
    method = "convex_hull"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_true("foraging_range_km2" %in% names(result))
  expect_true(all(result$foraging_range_km2 >= 0))
})


test_that("calc_foraging_range can filter to foraging phase only", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "A", "A"),
    longitude = c(0, 1, 0, 10),
    latitude = c(0, 0, 1, 10),
    phase = c("foraging", "foraging", "foraging", "commuting")
  )

  result <- calc_foraging_range(
    track_data,
    phase_col = "phase",
    foraging_value = "foraging",
    method = "convex_hull"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_equal(result$n_points, 3)
})


test_that("calc_foraging_range supports bounding box method", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "A"),
    longitude = c(0, 2, 0),
    latitude = c(0, 0, 2)
  )

  result <- calc_foraging_range(
    track_data,
    method = "bbox"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true("foraging_range_km2" %in% names(result))
  expect_true(result$foraging_range_km2 >= 0)
})


test_that("calc_foraging_range works with existing sf input", {
  skip_if_not_installed("sf")

  track_data <- data.frame(
    track_id = c("A", "A", "A"),
    longitude = c(0, 1, 0),
    latitude = c(0, 0, 1)
  )

  track_sf <- sf::st_as_sf(
    track_data,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  result <- calc_foraging_range(track_sf)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
})


test_that("calc_foraging_range errors when required columns are missing", {
  skip_if_not_installed("sf")

  bad_data <- data.frame(
    track_id = "A",
    longitude = 0
  )

  expect_error(
    calc_foraging_range(bad_data),
    "Missing:"
  )
})


test_that("summarize_individual_metrics summarizes trip metrics by bird", {
  trip_metrics <- data.frame(
    track_id = c("A", "A", "B"),
    trip_id = c(1, 2, 1),
    trip_distance_m = c(100, 300, 500),
    trip_duration = c(1, 3, 5),
    path_length_m = c(120, 350, 600),
    max_distance_from_colony_m = c(80, 200, 400)
  )

  result <- summarize_individual_metrics(trip_metrics)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("n_trips" %in% names(result))
  expect_true("mean_trip_distance_m" %in% names(result))
  expect_true("total_trip_distance_m" %in% names(result))
  expect_true("mean_trip_duration" %in% names(result))
  expect_true("total_path_length_m" %in% names(result))
  expect_true("max_distance_from_colony_m" %in% names(result))

  a <- result[result$track_id == "A", ]

  expect_equal(a$n_trips, 2)
  expect_equal(a$mean_trip_distance_m, 200)
  expect_equal(a$total_trip_distance_m, 400)
  expect_equal(a$mean_trip_duration, 2)
  expect_equal(a$total_trip_duration, 4)
  expect_equal(a$total_path_length_m, 470)
  expect_equal(a$max_distance_from_colony_m, 200)
})


test_that("summarize_individual_metrics works when only one metric column exists", {
  trip_metrics <- data.frame(
    track_id = c("A", "A", "B"),
    trip_id = c(1, 2, 1),
    trip_distance_m = c(100, 300, 500)
  )

  result <- summarize_individual_metrics(trip_metrics)

  expect_equal(nrow(result), 2)
  expect_true("mean_trip_distance_m" %in% names(result))
  expect_false("mean_trip_duration" %in% names(result))
})


test_that("summarize_individual_metrics errors when required columns are missing", {
  bad_data <- data.frame(
    track_id = c("A", "A"),
    trip_distance_m = c(100, 200)
  )

  expect_error(
    summarize_individual_metrics(bad_data),
    "Missing:"
  )
})


test_that("summarize_individual_metrics errors when no metric columns exist", {
  bad_data <- data.frame(
    track_id = c("A", "A"),
    trip_id = c(1, 2)
  )

  expect_error(
    summarize_individual_metrics(bad_data),
    "No metric columns found"
  )
})


test_that("summarize_population_metrics summarizes individual-level metrics", {
  individual_metrics <- data.frame(
    track_id = c("A", "B", "C"),
    n_trips = c(2, 3, 4),
    mean_trip_distance_m = c(100, 200, 300),
    total_trip_duration = c(5, 10, 15)
  )

  result <- summarize_population_metrics(individual_metrics)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  expect_true("n_individuals" %in% names(result))
  expect_true("n_trips_mean" %in% names(result))
  expect_true("n_trips_sd" %in% names(result))
  expect_true("n_trips_min" %in% names(result))
  expect_true("n_trips_max" %in% names(result))

  expect_equal(result$n_individuals, 3)
  expect_equal(result$n_trips_mean, 3)
  expect_equal(result$n_trips_min, 2)
  expect_equal(result$n_trips_max, 4)

  expect_equal(result$mean_trip_distance_m_mean, 200)
})


test_that("summarize_population_metrics errors when bird ID column is missing", {
  bad_data <- data.frame(
    n_trips = c(2, 3),
    mean_trip_distance_m = c(100, 200)
  )

  expect_error(
    summarize_population_metrics(bad_data),
    "Missing:"
  )
})


test_that("summarize_population_metrics errors when no numeric metric columns exist", {
  bad_data <- data.frame(
    track_id = c("A", "B"),
    sex = c("F", "M")
  )

  expect_error(
    summarize_population_metrics(bad_data),
    "No numeric metric columns found"
  )
})
