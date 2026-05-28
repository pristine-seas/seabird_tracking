library(testthat)

test_that("standardize_gps_columns renames and formats columns correctly", {
  raw_df <- data.frame(
    BirdCode = c("A1", "A1"),
    UTC_DateTime = c("2026-05-20 12:00:00", "2026-05-20 13:00:00"),
    Lon_DD = c("175.1", "175.2"), # String to test numeric coercion
    Lat_DD = c("-20.1", "-20.2"),
    stringsAsFactors = FALSE
  )
  
  std <- standardize_gps_columns(raw_df)
  
  # Check standard columns exist
  expect_true(all(c("bird_id", "timestamp", "lon", "lat", "trip_id", "phase") %in% names(std)))
  
  # Check type coercion
  expect_s3_class(std$timestamp, "POSIXct")
  expect_type(std$lon, "double")
  expect_type(std$lat, "double")
  
  # Check initialized extras
  expect_true(all(is.na(std$trip_id)))
})

test_that("standardize_gps_columns handles missing columns based on flags", {
  missing_df <- data.frame(BirdCode = "A1", Lat_DD = -20.1) # Missing lon and time
  
  # Should warn and add NAs by default
  expect_warning(
    res <- standardize_gps_columns(missing_df, add_missing_cols = TRUE),
    "not found in data. Adding as NA column"
  )
  expect_true(all(is.na(res$timestamp)))
  
  # Should error if add_missing_cols is FALSE
  expect_error(
    standardize_gps_columns(missing_df, add_missing_cols = FALSE),
    "not found in data"
  )
})