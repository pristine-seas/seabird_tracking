library(testthat)
library(dplyr)
library(sf)
library(readr)
source("~/Desktop/Shearwater/R/utils_validation.R")
describe("assert_required_cols()", {
  it("passes silently when all required columns are present", {
    df <- data.frame(track_id = 1, timestamp = 2)
    expect_silent(assert_required_cols(df, c("track_id", "timestamp")))
  })

  it("raises an informative error when columns are missing", {
    df <- data.frame(track_id = 1, latitude = 2)
    expect_error(
      assert_required_cols(df, c("track_id", "timestamp")),
      "Required column\\(s\\) missing from data: timestamp"
    )
  })

  it("fails if the primary input is not a data frame", {
    expect_error(assert_required_cols(c(1, 2, 3), "col1"), "`data` must be a data frame")
  })
})


describe("assert_crs()", {
  # Setup spatial structures for testing
  pt_4326  <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  pt_32632 <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32632))
  pt_naive  <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = NA_crs_))

  it("passes silently if a valid CRS is assigned and no expectation is enforced", {
    expect_silent(assert_crs(pt_4326))
  })

  it("passes silently if the CRS matches the expected EPSG", {
    expect_silent(assert_crs(pt_4326, expected_crs = 4326))
  })

  it("fails explicitly when a spatial layer lacks a defined CRS coordinate layer", {
    expect_error(assert_crs(pt_naive), "Spatial object has no CRS assigned")
  })

  it("fails with structural diagnostics when there is a projection mismatch", {
    expect_error(assert_crs(pt_4326, expected_crs = 32632), "CRS mismatch")
  })
})


describe("assert_datetime_tz()", {
  it("passes when a column has a fully qualified time zone attribute", {
    df <- data.frame(ts = as.POSIXct("2026-05-20 12:00:00", tz = "UTC"))
    expect_silent(assert_datetime_tz(df, "ts"))
  })

  it("fails when parsing a naive, local-system-assumed timestamp", {
    # Coercing without a string timezone defaults to native system locale ("")
    df_naive <- data.frame(ts = as.POSIXct("2026-05-20 12:00:00", tz = ""))
    expect_error(assert_datetime_tz(df_naive, "ts"), "has no explicit timezone")
  })

  it("successfully parses valid string datetimes and checks timezone safety", {
    df_char <- data.frame(ts = "2026-05-20 12:00:00", stringsAsFactors = FALSE)
    # R parses native character vectors into local tz context, causing validation failure
    expect_error(assert_datetime_tz(df_char, "ts"), "has no explicit timezone")
  })
})
