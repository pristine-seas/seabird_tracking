library(testthat)
library(dplyr)
library(sf)
library(readr)
source("~/Desktop/Shearwater/R/utils_validation.R")
source("~/Desktop/Shearwater/R/utils_schema.R")
describe("validate_gps_data()", {
  # Mock a perfectly sound structural tracking dataset
  valid_df <- data.frame(
    track_id  = c("bird-01", "bird-01", "bird-02"),
    timestamp = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-01 10:00:00"), tz = "UTC"),
    latitude  = c(-34.52, -34.55, 12.01),
    longitude = c(115.02, 115.06, -45.32),
    stringsAsFactors = FALSE
  )

  it("returns a valid class flag and standard structural fields on valid telemetry data", {
    res <- validate_gps_data(valid_df)
    expect_s3_class(res, "validated_gps_data")
    expect_true(res$valid)
    expect_equal(length(res$errors), 0)
  })

  it("catches latitudinal/longitudinal anomalies outside realistic bounds", {
    bad_coords <- valid_df
    bad_coords$latitude[1] <- 105.0 # Exceeds 90 degrees
    res <- validate_gps_data(bad_coords)
    expect_false(res$valid)
    expect_match(res$errors, "Latitude values outside")
  })

  it("triggers warning flags for chronological telemetry sequencing errors within track ids", {
    out_of_order <- valid_df
    out_of_order$timestamp[1] <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC") # Swapped order
    res <- validate_gps_data(out_of_order)
    expect_true(res$valid) # It remains functionally valid unless strict = TRUE
    expect_match(res$warnings, "timestamps not in ascending order")
  })

  it("promotes warnings to hard termination errors when execution is run in strict mode", {
    out_of_order <- valid_df
    out_of_order$timestamp[1] <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
    res <- validate_gps_data(out_of_order, strict = TRUE)
    expect_false(res$valid)
    expect_match(res$errors, "Strict mode: treating warnings as errors")
  })
})


describe("coerce_track_tbl()", {
  # Mock up an alias map messy tracking table
  aliased_df <- data.frame(
    id  = c(1, 2),
    ts  = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 13:00:00"), tz = "UTC"),
    lat = c(-32.1, -32.2),
    lon = c(114.5, 114.6)
  )

  it("renames telemetry elements dynamically based on standard vocabulary mapping", {
    res_obj <- coerce_track_tbl(aliased_df, validate = TRUE)
    # Extracts underlying coerced table from S3 verification wrapper
    df_coerced <- res_obj$data

    expect_true(res_obj$valid)
    expect_named(df_coerced, c("track_id", "timestamp", "latitude", "longitude"))
  })

  it("gracefully falls back to regular unvalidated data frames if verification is bypassed", {
    res_raw <- coerce_track_tbl(aliased_df, validate = FALSE, as_tibble = FALSE)
    expect_s3_class(res_raw, "data.frame")
    expect_false(inherits(res_raw, "tbl_df"))
    expect_named(res_raw, c("track_id", "timestamp", "latitude", "longitude"))
  })
})
