# tests/testthat/test-clean_filters.R

testthat::test_that("remove_duplicate_fixes removes duplicate fixes using Date and Time", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdA", "BirdB"),
    Date = c("01/01/2024", "01/01/2024", "01/01/2024", "01/01/2024"),
    Time = c("00:00:00", "00:00:00", "01:00:00", "00:00:00"),
    Latitude = c(21.50, 21.50, 21.60, 22.00),
    Longitude = c(-158.20, -158.20, -158.30, -159.00),
    Speed = c(10, 10, 12, 8),
    Distance = c(100, 100, 120, 80),
    Type = c(0, 0, 0, 0),
    Essential = c(1, 1, 1, 1)
  )

  out <- remove_duplicate_fixes(
    df = wtsh,
    id_col = "ID",
    datetime_col = NULL,
    date_col = "Date",
    time_col = "Time"
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_false("..datetime" %in% names(out))
  testthat::expect_equal(sum(out$ID == "BirdA"), 2)
  testthat::expect_equal(sum(out$ID == "BirdB"), 1)
})


testthat::test_that("remove_duplicate_fixes removes duplicate fixes using datetime_col", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdA", "BirdB"),
    datetime = as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 00:00:00"
      ),
      tz = "UTC"
    ),
    Latitude = c(21.50, 21.50, 21.60, 22.00),
    Longitude = c(-158.20, -158.20, -158.30, -159.00),
    Speed = c(10, 10, 12, 8)
  )

  out <- remove_duplicate_fixes(
    df = wtsh,
    id_col = "ID",
    datetime_col = "datetime"
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_false("..datetime" %in% names(out))
})


testthat::test_that("remove_duplicate_fixes errors when ID column is missing", {
  wtsh <- data.frame(
    Date = "01/01/2024",
    Time = "00:00:00",
    Latitude = 21.50,
    Longitude = -158.20
  )

  testthat::expect_error(
    remove_duplicate_fixes(wtsh, id_col = "ID"),
    "Missing ID column"
  )
})


testthat::test_that("remove_duplicate_fixes errors when datetime column is missing", {
  wtsh <- data.frame(
    ID = "BirdA",
    Latitude = 21.50,
    Longitude = -158.20
  )

  testthat::expect_error(
    remove_duplicate_fixes(
      wtsh,
      id_col = "ID",
      datetime_col = "datetime"
    ),
    "Missing datetime column"
  )
})


testthat::test_that("remove_duplicate_fixes errors when Date and Time are missing", {
  wtsh <- data.frame(
    ID = "BirdA",
    Latitude = 21.50,
    Longitude = -158.20
  )

  testthat::expect_error(
    remove_duplicate_fixes(
      wtsh,
      id_col = "ID",
      datetime_col = NULL,
      date_col = "Date",
      time_col = "Time"
    ),
    "Must provide either datetime_col OR both date_col and time_col"
  )
})


testthat::test_that("filter_speed_outliers removes rows above max_speed", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdB", "BirdB"),
    Speed = c(10, 25, 5, 100),
    Latitude = c(21.50, 21.51, 21.52, 21.53),
    Longitude = c(-158.20, -158.21, -158.22, -158.23)
  )

  out <- filter_speed_outliers(
    df = wtsh,
    max_speed = 20,
    speed_col = "Speed",
    method = "remove"
  )

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(out$Speed <= 20))
})


testthat::test_that("filter_speed_outliers keeps NA speeds when removing outliers", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdB"),
    Speed = c(10, NA, 100),
    Latitude = c(21.50, 21.51, 21.52),
    Longitude = c(-158.20, -158.21, -158.22)
  )

  out <- filter_speed_outliers(
    df = wtsh,
    max_speed = 20,
    speed_col = "Speed",
    method = "remove"
  )

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(any(is.na(out$Speed)))
  testthat::expect_false(any(out$Speed > 20, na.rm = TRUE))
})


testthat::test_that("filter_speed_outliers flags rows above max_speed", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdB"),
    Speed = c(10, 25, 5),
    Latitude = c(21.50, 21.51, 21.52),
    Longitude = c(-158.20, -158.21, -158.22)
  )

  out <- filter_speed_outliers(
    df = wtsh,
    max_speed = 20,
    speed_col = "Speed",
    method = "flag"
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true("..speed_outlier" %in% names(out))
  testthat::expect_equal(out$..speed_outlier, c(FALSE, TRUE, FALSE))
})


testthat::test_that("filter_speed_outliers errors when speed column is missing", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA"),
    Latitude = c(21.50, 21.51),
    Longitude = c(-158.20, -158.21)
  )

  testthat::expect_error(
    filter_speed_outliers(
      df = wtsh,
      max_speed = 20,
      speed_col = "Speed",
      method = "remove"
    ),
    "Missing speed column"
  )
})


testthat::test_that("filter_on_land_or_invalid_points removes invalid coordinates", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdB", "BirdB", "BirdC"),
    Latitude = c(21.50, 95, 21.52, NA, -91),
    Longitude = c(-158.20, -158.21, 181, -158.23, -158.24),
    Speed = c(10, 12, 15, 8, 7)
  )

  out <- filter_on_land_or_invalid_points(
    df = wtsh,
    lat_col = "Latitude",
    lon_col = "Longitude",
    polygon = NULL,
    method = "remove"
  )

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$ID, "BirdA")
  testthat::expect_true(all(out$Latitude >= -90 & out$Latitude <= 90))
  testthat::expect_true(all(out$Longitude >= -180 & out$Longitude <= 180))
})


testthat::test_that("filter_on_land_or_invalid_points flags invalid coordinates", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdA", "BirdB"),
    Latitude = c(21.50, 95, 21.52),
    Longitude = c(-158.20, -158.21, 181),
    Speed = c(10, 12, 15)
  )

  out <- filter_on_land_or_invalid_points(
    df = wtsh,
    lat_col = "Latitude",
    lon_col = "Longitude",
    polygon = NULL,
    method = "flag"
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true("..invalid_coord" %in% names(out))
  testthat::expect_true("..on_excluded_area" %in% names(out))
  testthat::expect_true("..spatial_outlier" %in% names(out))
  testthat::expect_equal(out$..invalid_coord, c(FALSE, TRUE, TRUE))
  testthat::expect_equal(out$..on_excluded_area, c(FALSE, FALSE, FALSE))
  testthat::expect_equal(out$..spatial_outlier, c(FALSE, TRUE, TRUE))
})


testthat::test_that("filter_on_land_or_invalid_points removes points inside exclusion polygon", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB", "BirdC"),
    Latitude = c(0.5, 2.0, -2.0),
    Longitude = c(0.5, 2.0, -2.0),
    Speed = c(10, 12, 15)
  )

  polygon_coords <- matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )

  exclusion_polygon <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(polygon_coords)),
      crs = 4326
    )
  )

  out <- filter_on_land_or_invalid_points(
    df = wtsh,
    lat_col = "Latitude",
    lon_col = "Longitude",
    polygon = exclusion_polygon,
    method = "remove"
  )

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_false("BirdA" %in% out$ID)
})


testthat::test_that("filter_on_land_or_invalid_points flags points inside exclusion polygon", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB", "BirdC"),
    Latitude = c(0.5, 2.0, -2.0),
    Longitude = c(0.5, 2.0, -2.0),
    Speed = c(10, 12, 15)
  )

  polygon_coords <- matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )

  exclusion_polygon <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(polygon_coords)),
      crs = 4326
    )
  )

  out <- filter_on_land_or_invalid_points(
    df = wtsh,
    lat_col = "Latitude",
    lon_col = "Longitude",
    polygon = exclusion_polygon,
    method = "flag"
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true(out$..on_excluded_area[1])
  testthat::expect_true(out$..spatial_outlier[1])
  testthat::expect_false(out$..spatial_outlier[2])
  testthat::expect_false(out$..spatial_outlier[3])
})


testthat::test_that("filter_on_land_or_invalid_points errors when coordinate columns are missing", {
  wtsh <- data.frame(
    ID = "BirdA",
    Speed = 10
  )

  testthat::expect_error(
    filter_on_land_or_invalid_points(
      df = wtsh,
      lat_col = "Latitude",
      lon_col = "Longitude",
      method = "remove"
    ),
    "Missing columns"
  )
})


testthat::test_that("filter_on_land_or_invalid_points errors when polygon is not sf", {
  wtsh <- data.frame(
    ID = "BirdA",
    Latitude = 21.50,
    Longitude = -158.20,
    Speed = 10
  )

  testthat::expect_error(
    filter_on_land_or_invalid_points(
      df = wtsh,
      lat_col = "Latitude",
      lon_col = "Longitude",
      polygon = data.frame(x = 1),
      method = "remove"
    ),
    "polygon must be an sf object"
  )
})


testthat::test_that("flag_low_quality_fixes adds QA columns for all checks", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB", "BirdC", "BirdD"),
    Speed = c(10, 100, 15, NA),
    Distance = c(100, 200, 9999, 50),
    Type = c(0, 0, 1, 0),
    Essential = c(1, 0, 1, NA),
    Latitude = c(21.50, 21.51, 21.52, 21.53),
    Longitude = c(-158.20, -158.21, -158.22, -158.23)
  )

  out <- flag_low_quality_fixes(
    df = wtsh,
    speed_col = "Speed",
    max_speed = 80,
    distance_col = "Distance",
    max_distance = 1000,
    fix_type_col = "Type",
    valid_fix_types = 0,
    essential_col = "Essential",
    valid_essential_values = 1,
    prefix = "..qa_"
  )

  testthat::expect_true("..qa_high_speed" %in% names(out))
  testthat::expect_true("..qa_high_distance" %in% names(out))
  testthat::expect_true("..qa_invalid_type" %in% names(out))
  testthat::expect_true("..qa_low_essential" %in% names(out))
  testthat::expect_true("..qa_any" %in% names(out))

  testthat::expect_equal(out$..qa_high_speed, c(FALSE, TRUE, FALSE, TRUE))
  testthat::expect_equal(out$..qa_high_distance, c(FALSE, FALSE, TRUE, FALSE))
  testthat::expect_equal(out$..qa_invalid_type, c(FALSE, FALSE, TRUE, FALSE))
  testthat::expect_equal(out$..qa_low_essential, c(FALSE, TRUE, FALSE, TRUE))
  testthat::expect_equal(out$..qa_any, c(FALSE, TRUE, TRUE, TRUE))
})


testthat::test_that("flag_low_quality_fixes can skip selected QA checks", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB"),
    Speed = c(10, 100),
    Distance = c(100, 9999),
    Type = c(0, 1),
    Essential = c(1, 0)
  )

  out <- flag_low_quality_fixes(
    df = wtsh,
    speed_col = "Speed",
    max_speed = 80,
    distance_col = NULL,
    max_distance = NULL,
    fix_type_col = NULL,
    valid_fix_types = NULL,
    essential_col = NULL,
    valid_essential_values = NULL
  )

  testthat::expect_true("..qa_high_speed" %in% names(out))
  testthat::expect_false("..qa_high_distance" %in% names(out))
  testthat::expect_false("..qa_invalid_type" %in% names(out))
  testthat::expect_false("..qa_low_essential" %in% names(out))
  testthat::expect_true("..qa_any" %in% names(out))
  testthat::expect_equal(out$..qa_high_speed, c(FALSE, TRUE))
})


testthat::test_that("flag_low_quality_fixes warns when no QA checks are applied", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB"),
    Speed = c(10, 100)
  )

  testthat::expect_warning(
    out <- flag_low_quality_fixes(
      df = wtsh,
      speed_col = NULL,
      max_speed = NULL,
      distance_col = NULL,
      max_distance = NULL,
      fix_type_col = NULL,
      valid_fix_types = NULL,
      essential_col = NULL,
      valid_essential_values = NULL
    ),
    "No QA checks were applied"
  )

  testthat::expect_equal(out, wtsh)
})


testthat::test_that("flag_low_quality_fixes warns and skips missing optional columns", {
  wtsh <- data.frame(
    ID = c("BirdA", "BirdB"),
    Speed = c(10, 100)
  )

  testthat::expect_warning(
    out <- flag_low_quality_fixes(
      df = wtsh,
      speed_col = "Speed",
      max_speed = 80,
      distance_col = "Distance",
      max_distance = 1000,
      fix_type_col = "Type",
      valid_fix_types = 0,
      essential_col = "Essential",
      valid_essential_values = 1
    ),
    "column not found"
  )

  testthat::expect_true("..qa_high_speed" %in% names(out))
  testthat::expect_true("..qa_any" %in% names(out))
  testthat::expect_equal(out$..qa_high_speed, c(FALSE, TRUE))
})
