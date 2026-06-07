library(testthat)
library(dplyr)

# Tests moved out of test-top_level_wrappers.R because clean_tracks()
# now belongs in its own wrapper file: R/clean_tracks.R.

describe("clean_tracks()", {
  make_raw_tracks <- function() {
    data.frame(
      track_id = c("A", "A", "B"),
      timestamp = as.POSIXct(
        c("2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-01 10:30:00"),
        tz = "UTC"
      ),
      latitude = c(-20.1, -20.2, -21.1),
      longitude = c(175.1, 175.2, 176.1),
      Speed = c(10, 20, 15),
      quality_flag = c("ok", "ok", "ok"),
      stringsAsFactors = FALSE
    )
  }

  make_clean_stage <- function(data) {
    data$datetime_regular <- data$timestamp
    data$lat <- data$latitude
    data$lon <- data$longitude
    data
  }

  it("runs the cleaning stages and attaches a cleaning log", {
    raw <- make_raw_tracks()

    testthat::local_mocked_bindings(
      coerce_track_tbl = function(data, ...) data,
      standardize_gps_columns = function(raw_data, ...) raw_data,
      flag_low_quality_fixes = function(df, ...) {
        df$quality_flag <- "ok"
        df
      },
      remove_duplicate_fixes = function(df, ...) df,
      filter_speed_outliers = function(df, ...) df,
      filter_on_land_or_invalid_points = function(df, ...) df,
      regularize_tracks = function(df, ...) make_clean_stage(df),
      interpolate_tracks = function(df, ...) df
    )

    res <- clean_tracks(raw, verbose = FALSE)

    expect_s3_class(res, "data.frame")
    expect_true(!is.null(attr(res, "cleaning_log")))
    expect_equal(nrow(res), nrow(raw))
    expect_true(all(c("datetime_regular", "lat", "lon") %in% names(res)))
  })

  it("uses standardize_gps_columns when col_map is supplied", {
    raw <- make_raw_tracks()
    raw$tag <- raw$track_id
    raw$track_id <- NULL

    testthat::local_mocked_bindings(
      coerce_track_tbl = function(data, ...) {
        stop("coerce_track_tbl should not be called when col_map is supplied")
      },
      standardize_gps_columns = function(raw_data, col_map = NULL, ...) {
        raw_data$track_id <- raw_data$tag
        raw_data$tag <- NULL
        raw_data
      },
      flag_low_quality_fixes = function(df, ...) df,
      remove_duplicate_fixes = function(df, ...) df,
      filter_speed_outliers = function(df, ...) df,
      filter_on_land_or_invalid_points = function(df, ...) df,
      regularize_tracks = function(df, ...) make_clean_stage(df),
      interpolate_tracks = function(df, ...) df
    )

    res <- clean_tracks(
      raw,
      col_map = c(track_id = "tag"),
      verbose = FALSE
    )

    expect_true("track_id" %in% names(res))
    expect_false("tag" %in% names(res))
  })

  it("can drop records flagged as low quality", {
    raw <- make_raw_tracks()

    testthat::local_mocked_bindings(
      coerce_track_tbl = function(data, ...) data,
      standardize_gps_columns = function(raw_data, ...) raw_data,
      flag_low_quality_fixes = function(df, ...) {
        df$quality_flag <- c("ok", "bad", "ok")
        df
      },
      remove_duplicate_fixes = function(df, ...) df,
      filter_speed_outliers = function(df, ...) df,
      filter_on_land_or_invalid_points = function(df, ...) df,
      regularize_tracks = function(df, ...) make_clean_stage(df),
      interpolate_tracks = function(df, ...) df
    )

    res <- clean_tracks(raw, drop_flagged = TRUE, verbose = FALSE)

    expect_true(all(res$quality_flag == "ok" | is.na(res$quality_flag)))
    expect_lt(nrow(res), nrow(raw))
  })

  it("errors on invalid scalar arguments", {
    raw <- make_raw_tracks()

    expect_error(clean_tracks("not a data frame"), "data frame")
    expect_error(clean_tracks(raw, max_speed = -1), "max_speed|positive", ignore.case = TRUE)
    expect_error(clean_tracks(raw, interval_minutes = 0), "interval_minutes|positive", ignore.case = TRUE)
    expect_error(clean_tracks(raw, max_gap_minutes = 0), "max_gap_minutes|positive", ignore.case = TRUE)
    expect_error(clean_tracks(raw, verbose = NA), "verbose|TRUE or FALSE", ignore.case = TRUE)
  })
})
