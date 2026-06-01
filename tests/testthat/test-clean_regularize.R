library(testthat)

# A minimal, well-formed two-bird dataset with 30-minute fixes
make_clean_df <- function() {
  data.frame(
    ID       = rep(c("BirdA", "BirdB"), each = 6),
    Date     = rep("01/01/2023", 12),
    Time     = c(
      "00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00",  # BirdA
      "00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00"  # BirdB
    ),
    Latitude  = c(10.0, 10.1, 10.2, 10.3, 10.4, 10.5,
                  20.0, 20.1, 20.2, 20.3, 20.4, 20.5),
    Longitude = c(30.0, 30.1, 30.2, 30.3, 30.4, 30.5,
                  40.0, 40.1, 40.2, 40.3, 40.4, 40.5),
    stringsAsFactors = FALSE
  )
}

# A single-bird dataset with a deliberate gap (rows 4 and 5 are missing)
make_gap_df <- function() {
  data.frame(
    ID        = rep("BirdA", 4),
    Date      = rep("01/01/2023", 4),
    Time      = c("00:00:00", "00:30:00", "02:00:00", "02:30:00"),  # 1-hr gap
    Latitude  = c(10.0, 10.1, 10.4, 10.5),
    Longitude = c(30.0, 30.1, 30.4, 30.5),
    stringsAsFactors = FALSE
  )
}

# Custom column names variant
make_custom_cols_df <- function() {
  df <- make_clean_df()
  names(df) <- c("bird", "date_utc", "time_utc", "lat", "lon")
  df
}

test_that("regularize_tracks: returns a data.frame", {
  out <- regularize_tracks(make_clean_df())
  expect_s3_class(out, "data.frame")
})

test_that("regularize_tracks: output contains required columns", {
  out <- regularize_tracks(make_clean_df())
  expect_true("datetime_regular" %in% names(out))
  expect_true("lat"              %in% names(out))
  expect_true("lon"              %in% names(out))
  expect_true("is_observed"      %in% names(out))
  expect_true("ID"               %in% names(out))
})

test_that("regularize_tracks: id column name is preserved", {
  out <- regularize_tracks(make_custom_cols_df(),
                           id_col = "bird", date_col = "date_utc",
                           time_col = "time_utc", lat_col = "lat", lon_col = "lon")
  expect_true("bird" %in% names(out))
  expect_false("ID"  %in% names(out))
})

test_that("regularize_tracks: datetime_regular is POSIXct in UTC", {
  out <- regularize_tracks(make_clean_df())
  expect_s3_class(out$datetime_regular, "POSIXct")
  expect_equal(attr(out$datetime_regular, "tzone"), "UTC")
})

test_that("regularize_tracks: correct number of grid rows for clean 30-min data", {
  # 6 fixes at 30-min intervals → 6 grid points per bird, 2 birds → 12 rows
  out <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  expect_equal(nrow(out), 12)
})

test_that("regularize_tracks: all is_observed TRUE for perfectly spaced data", {
  out <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  expect_true(all(out$is_observed))
})

test_that("regularize_tracks: all is_observed is logical", {
  out <- regularize_tracks(make_clean_df())
  expect_type(out$is_observed, "logical")
})

test_that("regularize_tracks: no NAs in lat/lon when data is complete", {
  out <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  expect_false(any(is.na(out$lat)))
  expect_false(any(is.na(out$lon)))
})

test_that("regularize_tracks: grid points appear in ascending time order per bird", {
  out <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  for (bird in unique(out$ID)) {
    dts <- out$datetime_regular[out$ID == bird]
    expect_true(all(diff(as.numeric(dts)) > 0))
  }
})

test_that("regularize_tracks: gap in data produces NA rows in output", {
  out <- regularize_tracks(make_gap_df(), interval_minutes = 30)
  # Grid spans 00:00 to 02:30; the 01:00 and 01:30 slots should be NA
  expect_true(any(!out$is_observed))
  expect_true(any(is.na(out$lat)))
})

test_that("regularize_tracks: interval_minutes controls grid spacing", {
  out_30 <- regularize_tracks(make_gap_df(), interval_minutes = 30)
  out_60 <- regularize_tracks(make_gap_df(), interval_minutes = 60)
  # Coarser interval → fewer rows
  expect_gt(nrow(out_30), nrow(out_60))
})

test_that("regularize_tracks: tolerance_minutes defaults to half the interval", {
  # With tolerance = 15 min, a fix 14 min off-grid should match; 16 min should not.
  df <- data.frame(
    ID        = "BirdA",
    Date      = "01/01/2023",
    Time      = c("00:00:00", "00:44:00"),   # second fix is 14 min before 01:00 grid point
    Latitude  = c(10.0, 10.5),
    Longitude = c(30.0, 30.5),
    stringsAsFactors = FALSE
  )
  out <- regularize_tracks(df, interval_minutes = 60, tolerance_minutes = 15)
  # Grid points: 00:00 and 01:00
  expect_true(out$is_observed[1])   # 00:00 exact match
  expect_false(out$is_observed[2])  # 00:44 is 16 min from 01:00 — outside default 30-min tolerance
})

test_that("regularize_tracks: explicit tolerance_minutes is respected", {
  df <- data.frame(
    ID        = "BirdA",
    Date      = "01/01/2023",
    Time      = c("00:00:00", "00:50:00"),
    Latitude  = c(10.0, 10.5),
    Longitude = c(30.0, 30.5),
    stringsAsFactors = FALSE
  )
  # With 60-min interval and 15-min tolerance, 00:50 is 10 min from 01:00 → match
  out_match <- regularize_tracks(df, interval_minutes = 60, tolerance_minutes = 15)
  expect_true(out_match$is_observed[2])

  # With 5-min tolerance, 00:50 is 10 min from 01:00 → no match
  out_no_match <- regularize_tracks(df, interval_minutes = 60, tolerance_minutes = 5)
  expect_false(out_no_match$is_observed[2])
})

test_that("regularize_tracks: single-bird single-fix input returns one row", {
  df <- data.frame(ID = "BirdA", Date = "01/01/2023", Time = "12:00:00",
                   Latitude = 10.0, Longitude = 30.0, stringsAsFactors = FALSE)
  out <- regularize_tracks(df, interval_minutes = 30)
  expect_equal(nrow(out), 1)
  expect_true(out$is_observed[1])
})

test_that("regularize_tracks: handles multiple birds independently", {
  out <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  birds <- unique(out$ID)
  expect_equal(sort(birds), c("BirdA", "BirdB"))
  # Each bird should have the same number of rows
  counts <- table(out$ID)
  expect_equal(counts[["BirdA"]], counts[["BirdB"]])
})

test_that("regularize_tracks: errors on missing required column", {
  df <- make_clean_df()
  df$Latitude <- NULL
  expect_error(regularize_tracks(df), "Missing columns")
})

test_that("regularize_tracks: errors on completely unparseable datetime", {
  df <- make_clean_df()
  df$Date <- "not-a-date"
  expect_error(regularize_tracks(df), "datetime parsing failed")
})

test_that("regularize_tracks: custom column names work end-to-end", {
  df <- make_custom_cols_df()
  out <- regularize_tracks(df, id_col = "bird", date_col = "date_utc",
                           time_col = "time_utc", lat_col = "lat", lon_col = "lon")
  expect_s3_class(out, "data.frame")
  expect_true(all(out$is_observed))
})

# Helper: produce regularized output with a known gap ready for interpolation
make_regularized_gap <- function(interval_minutes = 30, max_gap_minutes = 60) {
  regularize_tracks(make_gap_df(), interval_minutes = interval_minutes)
}

test_that("interpolate_tracks: returns a data.frame", {
  reg <- make_regularized_gap()
  out <- interpolate_tracks(reg)
  expect_s3_class(out, "data.frame")
})

test_that("interpolate_tracks: adds is_interpolated column", {
  reg <- make_regularized_gap()
  out <- interpolate_tracks(reg)
  expect_true("is_interpolated" %in% names(out))
})

test_that("interpolate_tracks: is_interpolated is logical", {
  reg <- make_regularized_gap()
  out <- interpolate_tracks(reg)
  expect_type(out$is_interpolated, "logical")
})

test_that("interpolate_tracks: row count is unchanged", {
  reg <- make_regularized_gap()
  out <- interpolate_tracks(reg)
  expect_equal(nrow(out), nrow(reg))
})

test_that("interpolate_tracks: fills NA positions across small gaps", {
  reg <- make_regularized_gap(interval_minutes = 30)
  # Default max_gap is 60 min; our gap is exactly 60 min
  out <- interpolate_tracks(reg, max_gap_minutes = 60)
  expect_false(any(is.na(out$lat)))
  expect_false(any(is.na(out$lon)))
})

test_that("interpolate_tracks: does NOT fill gaps exceeding max_gap_minutes", {
  reg <- make_regularized_gap(interval_minutes = 30)
  # Gap is 60 min; set limit to 30 min → should remain NA
  out <- interpolate_tracks(reg, max_gap_minutes = 30)
  expect_true(any(is.na(out$lat)))
})

test_that("interpolate_tracks: is_interpolated TRUE only where gap was filled", {
  reg <- make_regularized_gap(interval_minutes = 30)
  out <- interpolate_tracks(reg, max_gap_minutes = 60)
  # Originally observed rows should not be flagged as interpolated
  expect_true(all(!out$is_interpolated[reg$is_observed]))
  # At least one row should be flagged as interpolated
  expect_true(any(out$is_interpolated))
})

test_that("interpolate_tracks: interpolated coordinates lie between anchor values", {
  reg <- make_regularized_gap(interval_minutes = 30)
  out <- interpolate_tracks(reg, max_gap_minutes = 60)
  interp_rows <- which(out$is_interpolated)
  for (r in interp_rows) {
    # Find surrounding observed/non-NA rows
    before <- max(which(!is.na(out$lat[seq_len(r - 1)])))
    after  <- min(which(!is.na(out$lat[(r + 1):nrow(out)]))) + r
    expect_gte(out$lat[r], min(out$lat[c(before, after)]) - 1e-9)
    expect_lte(out$lat[r], max(out$lat[c(before, after)]) + 1e-9)
  }
})

test_that("interpolate_tracks: no changes when data has no gaps", {
  reg <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  out <- interpolate_tracks(reg)
  expect_false(any(out$is_interpolated))
  expect_equal(out$lat, reg$lat)
  expect_equal(out$lon, reg$lon)
})

test_that("interpolate_tracks: errors when datetime_col is not POSIXct", {
  reg <- make_regularized_gap()
  reg$datetime_regular <- as.character(reg$datetime_regular)  # break the type
  expect_error(interpolate_tracks(reg), "POSIXct")
})

test_that("interpolate_tracks: errors on missing required column", {
  reg <- make_regularized_gap()
  reg$lat <- NULL
  expect_error(interpolate_tracks(reg), "Missing columns")
})

test_that("interpolate_tracks: custom column names work end-to-end", {
  reg <- make_regularized_gap()
  names(reg)[names(reg) == "ID"] <- "bird"
  out <- interpolate_tracks(reg, id_col = "bird")
  expect_s3_class(out, "data.frame")
  expect_true("is_interpolated" %in% names(out))
})

test_that("interpolate_tracks: does not modify originally observed positions", {
  reg <- make_regularized_gap(interval_minutes = 30)
  out <- interpolate_tracks(reg, max_gap_minutes = 60)
  obs <- reg$is_observed
  expect_equal(out$lat[obs], reg$lat[obs])
  expect_equal(out$lon[obs], reg$lon[obs])
})

test_that("interpolate_tracks: leading and trailing NAs (no anchor on one side) stay NA", {
  # A gap at the very start of a track has no 'before' anchor — should remain NA
  df <- data.frame(
    ID        = "BirdA",
    Date      = "01/01/2023",
    Time      = c("01:00:00", "01:30:00", "02:00:00"),  # starts at 01:00, not 00:00
    Latitude  = c(10.2, 10.3, 10.4),
    Longitude = c(30.2, 30.3, 30.4),
    stringsAsFactors = FALSE
  )
  reg <- regularize_tracks(df, interval_minutes = 30)
  # Force a leading NA by manually inserting one (simulates a track that starts late)
  reg_leading <- rbind(
    data.frame(ID = "BirdA", datetime_regular = reg$datetime_regular[1] - 1800,
               lat = NA_real_, lon = NA_real_, is_observed = FALSE,
               stringsAsFactors = FALSE),
    reg
  )
  out <- interpolate_tracks(reg_leading, max_gap_minutes = 60)
  expect_true(is.na(out$lat[1]))
})

# Helper: build a fully processed (regularized + interpolated) dataset
make_processed_df <- function() {
  reg  <- regularize_tracks(make_gap_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  interp
}

test_that("summarize_data_gaps: returns a data.frame", {
  out <- summarize_data_gaps(make_processed_df())
  expect_s3_class(out, "data.frame")
})

test_that("summarize_data_gaps: one row per unique bird", {
  # Two-bird dataset
  reg    <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp)
  expect_equal(nrow(out), 2)
})

test_that("summarize_data_gaps: output contains all required columns", {
  expected_cols <- c(
    "n_fixes_total", "n_observed", "n_interpolated", "n_missing",
    "pct_observed", "pct_interpolated", "pct_missing",
    "n_gaps", "max_gap_minutes", "mean_gap_minutes",
    "track_start", "track_end", "track_duration_hours"
  )
  out <- summarize_data_gaps(make_processed_df())
  for (col in expected_cols) {
    expect_true(col %in% names(out), info = paste("Missing column:", col))
  }
})

test_that("summarize_data_gaps: id column name is preserved", {
  reg    <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp, id_col = "ID")
  expect_true("ID" %in% names(out))
})

test_that("summarize_data_gaps: n_observed + n_interpolated + n_missing == n_fixes_total", {
  out <- summarize_data_gaps(make_processed_df())
  for (i in seq_len(nrow(out))) {
    expect_equal(
      out$n_observed[i] + out$n_interpolated[i] + out$n_missing[i],
      out$n_fixes_total[i]
    )
  }
})

test_that("summarize_data_gaps: percentages sum to 100", {
  out <- summarize_data_gaps(make_processed_df())
  for (i in seq_len(nrow(out))) {
    total_pct <- out$pct_observed[i] + out$pct_interpolated[i] + out$pct_missing[i]
    expect_equal(total_pct, 100, tolerance = 0.2)   # allow small rounding
  }
})

test_that("summarize_data_gaps: n_gaps > 0 when gap exists in raw data", {
  out <- summarize_data_gaps(make_processed_df())
  expect_gt(out$n_gaps[out$ID == "BirdA"], 0)
})

test_that("summarize_data_gaps: n_gaps == 0 for complete tracks", {
  reg    <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp)
  expect_equal(out$n_gaps[out$ID == "BirdA"], 0)
  expect_equal(out$n_gaps[out$ID == "BirdB"], 0)
})

test_that("summarize_data_gaps: max_gap_minutes >= mean_gap_minutes", {
  out <- summarize_data_gaps(make_processed_df())
  for (i in seq_len(nrow(out))) {
    expect_gte(out$max_gap_minutes[i], out$mean_gap_minutes[i])
  }
})

test_that("summarize_data_gaps: track_start < track_end", {
  out <- summarize_data_gaps(make_processed_df())
  expect_true(all(out$track_start < out$track_end))
})

test_that("summarize_data_gaps: track_start and track_end are POSIXct", {
  out <- summarize_data_gaps(make_processed_df())
  expect_s3_class(out$track_start, "POSIXct")
  expect_s3_class(out$track_end,   "POSIXct")
})

test_that("summarize_data_gaps: track_duration_hours is consistent with start/end", {
  out <- summarize_data_gaps(make_processed_df())
  for (i in seq_len(nrow(out))) {
    expected_hrs <- as.numeric(
      difftime(out$track_end[i], out$track_start[i], units = "hours")
    )
    expect_equal(out$track_duration_hours[i], round(expected_hrs, 2), tolerance = 0.01)
  }
})

test_that("summarize_data_gaps: no interpolation on fully observed track → n_interpolated == 0", {
  reg    <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp)
  expect_equal(out$n_interpolated[out$ID == "BirdA"], 0)
  expect_equal(out$n_interpolated[out$ID == "BirdB"], 0)
})

test_that("summarize_data_gaps: n_missing == 0 after successful interpolation", {
  out <- summarize_data_gaps(make_processed_df())  # 60-min gap, 60-min max → filled
  expect_equal(out$n_missing[out$ID == "BirdA"], 0)
})

test_that("summarize_data_gaps: n_missing > 0 when gap exceeds max_gap_minutes", {
  reg    <- regularize_tracks(make_gap_df(),  interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 30)  # gap too large → stays NA
  out    <- summarize_data_gaps(interp)
  expect_gt(out$n_missing[out$ID == "BirdA"], 0)
})

test_that("summarize_data_gaps: errors on missing required column", {
  processed <- make_processed_df()
  processed$is_observed <- NULL
  expect_error(summarize_data_gaps(processed), "Missing columns")
})

test_that("summarize_data_gaps: pct values are between 0 and 100", {
  out <- summarize_data_gaps(make_processed_df())
  expect_true(all(out$pct_observed     >= 0 & out$pct_observed     <= 100))
  expect_true(all(out$pct_interpolated >= 0 & out$pct_interpolated <= 100))
  expect_true(all(out$pct_missing      >= 0 & out$pct_missing      <= 100))
})

test_that("full pipeline runs without error on clean two-bird data", {
  reg    <- regularize_tracks(make_clean_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("full pipeline: interpolated gap reflected in summarize_data_gaps", {
  reg    <- regularize_tracks(make_gap_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 60)
  out    <- summarize_data_gaps(interp)
  # One gap was filled, so n_interpolated > 0 and n_missing == 0
  expect_gt(out$n_interpolated[1], 0)
  expect_equal(out$n_missing[1], 0)
})

test_that("full pipeline: unfilled gap reflected correctly in summarize_data_gaps", {
  reg    <- regularize_tracks(make_gap_df(), interval_minutes = 30)
  interp <- interpolate_tracks(reg, max_gap_minutes = 10)  # too tight → no fill
  out    <- summarize_data_gaps(interp)
  expect_equal(out$n_interpolated[1], 0)
  expect_gt(out$n_missing[1], 0)
})

# Small slice around the known 14-min gap on 2013-08-16 (06:00–06:45)
# Also includes the duplicate position just before the gap.
make_wtsh_gap_slice <- function() {
  data.frame(
    ID   = "WTSH_001",
    Date = "8/16/2013",
    Time = c(
      "5:56:56",   # last fix before duplicate / gap
      "6:04:16",   # duplicate position (Distance == 0 in raw data)
      "6:18:34",   # first fix after ~14-min gap
      "6:33:40",   # next fix (~15 min later)
      "6:36:53",
      "6:40:05",
      "6:43:18"
    ),
    Latitude  = c(21.573864, 21.573864, 21.573835, 21.573843,
                  21.573860, 21.573839, 21.573845),
    Longitude = c(-158.276520, -158.276520, -158.276459, -158.276520,
                  -158.276520, -158.276474, -158.276520),
    stringsAsFactors = FALSE
  )
}

# Slice crossing midnight (2013-08-15 23:57 → 2013-08-16 00:06)
make_wtsh_midnight_slice <- function() {
  data.frame(
    ID   = "WTSH_001",
    Date = c("8/15/2013", "8/15/2013", "8/16/2013", "8/16/2013"),
    Time = c("23:54:13", "23:57:22", "0:00:33", "0:03:42"),
    Latitude  = c(21.573870, 21.573698, 21.573814, 21.573940),
    Longitude = c(-158.276413, -158.276505, -158.276276, -158.276505),
    stringsAsFactors = FALSE
  )
}

# Slice containing the known bad fix on 2013-08-17 at 15:58:37
# (coordinate ~95 km away from surrounding valid fixes)
make_wtsh_bad_fix_slice <- function() {
  data.frame(
    ID   = "WTSH_001",
    Date = rep("8/17/2013", 5),
    Time = c("15:54:24", "15:58:35", "15:58:37", "16:10:11", "16:26:35"),
    Latitude  = c(21.359301, 21.359777, 22.179874, 21.359800, 21.360550),
    Longitude = c(-158.779190, -158.781311, -159.063065, -158.788177, -158.785599),
    stringsAsFactors = FALSE
  )
}

# A two-bird slice so we can test per-bird handling with real-format dates
make_wtsh_two_bird_slice <- function() {
  bird_a <- data.frame(
    ID   = "WTSH_001",
    Date = rep("8/15/2013", 4),
    Time = c("20:01:59", "20:05:33", "20:09:26", "20:12:32"),
    Latitude  = c(21.573956, 21.574306, 21.574120, 21.574350),
    Longitude = c(-158.276489, -158.276245, -158.276413, -158.276184),
    stringsAsFactors = FALSE
  )
  bird_b <- data.frame(
    ID   = "WTSH_002",
    Date = rep("8/15/2013", 4),
    Time = c("20:01:59", "20:05:33", "20:09:26", "20:12:32"),
    Latitude  = c(21.574000, 21.574100, 21.574200, 21.574300),
    Longitude = c(-158.277000, -158.277100, -158.277200, -158.277300),
    stringsAsFactors = FALSE
  )
  rbind(bird_a, bird_b)
}

test_that("WTSH: single-digit month/day date strings parse without error", {
  # "8/15/2013" not "08/15/2013" — R's %m handles both, but worth confirming
  df  <- make_wtsh_gap_slice()
  expect_no_error(regularize_tracks(df, id_col = "ID", interval_minutes = 3))
})

test_that("WTSH: datetime_regular values are not all NA after parsing", {
  df  <- make_wtsh_gap_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  expect_false(all(is.na(out$datetime_regular)))
})

test_that("WTSH: midnight crossing produces a continuous ascending grid", {
  df  <- make_wtsh_midnight_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  dts <- out$datetime_regular
  expect_true(all(diff(as.numeric(dts)) > 0),
              info = "Grid timestamps should be strictly increasing across midnight")
})

test_that("WTSH: midnight crossing — all four fixes matched as observed", {
  # Fixes are ~3 min apart; with a 3-min grid each should land within tolerance
  df  <- make_wtsh_midnight_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3,
                           tolerance_minutes = 1.5)
  expect_gte(sum(out$is_observed), 4L)
})

test_that("WTSH: function errors informatively when ID column is absent", {
  df <- make_wtsh_gap_slice()
  df$ID <- NULL   # simulate raw file without ID column
  expect_error(
    regularize_tracks(df, id_col = "ID", interval_minutes = 3),
    "Missing columns"
  )
})

test_that("WTSH: two-bird slice produces exactly two rows in gap summary", {
  df     <- make_wtsh_two_bird_slice()
  reg    <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  interp <- interpolate_tracks(reg, max_gap_minutes = 10)
  out    <- summarize_data_gaps(interp, id_col = "ID")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$ID, c("WTSH_001", "WTSH_002"))
})

test_that("WTSH: ~14-min gap on 2013-08-16 is flagged as missing at 3-min grid", {
  # Gap runs from 06:04:16 to 06:18:34 — approximately 4–5 empty grid cells
  # at a 3-min interval.
  df  <- make_wtsh_gap_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  expect_true(any(!out$is_observed),
              info = "Grid cells in the ~14-min gap should be unobserved")
  expect_true(any(is.na(out$lat)))
})

test_that("WTSH: gap is filled when max_gap_minutes is generous enough", {
  df     <- make_wtsh_gap_slice()
  reg    <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  interp <- interpolate_tracks(reg, max_gap_minutes = 20)
  expect_false(any(is.na(interp$lat)),
               info = "All NA positions should be interpolated with a 20-min limit")
  expect_true(any(interp$is_interpolated))
})

test_that("WTSH: gap is NOT filled when max_gap_minutes is too short", {
  df     <- make_wtsh_gap_slice()
  reg    <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  interp <- interpolate_tracks(reg, max_gap_minutes = 5)
  expect_true(any(is.na(interp$lat)),
              info = "Gap > 5 min should remain as NA")
  expect_equal(sum(interp$is_interpolated), 0L)
})

test_that("WTSH: gap summary reports correct n_gaps and non-zero max_gap_minutes", {
  df     <- make_wtsh_gap_slice()
  reg    <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  interp <- interpolate_tracks(reg, max_gap_minutes = 5)   # leave gap unfilled
  out    <- summarize_data_gaps(interp, id_col = "ID")
  expect_gte(out$n_gaps[1], 1L)
  expect_gt(out$max_gap_minutes[1], 0)
})

test_that("WTSH: irregular fix spacing still produces a strictly regular output grid", {
  # Real WTSH fixes are not exactly N minutes apart; output grid must be uniform
  df  <- make_wtsh_gap_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  steps_secs <- diff(as.numeric(out$datetime_regular))
  expect_true(all(abs(steps_secs - 180) < 1),   # 3 min = 180 s, allow 1-s rounding
              info = "All grid steps should be 3 minutes (180 s)")
})

test_that("WTSH: bad fix at 15:58:37 is present in regularized output if within tolerance", {
  # The bad fix (lat ~22.18, lon ~-159.06) is a real GPS error.
  # regularize_tracks() does not filter it — that is filter_speed_outliers()'s job.
  # This test confirms the function does NOT silently drop the point.
  df  <- make_wtsh_bad_fix_slice()
  out <- regularize_tracks(df, id_col = "ID", interval_minutes = 3,
                           tolerance_minutes = 2)
  # At least one observed fix should have the extreme latitude
  obs_lats <- out$lat[out$is_observed]
  expect_true(any(obs_lats > 22, na.rm = TRUE),
              info = "regularize_tracks() should retain the bad fix; filtering belongs elsewhere")
})

test_that("WTSH: track_duration_hours > 0 for multi-day track", {
  # Use the midnight-crossing slice which spans two calendar days
  df     <- make_wtsh_midnight_slice()
  reg    <- regularize_tracks(df, id_col = "ID", interval_minutes = 3)
  interp <- interpolate_tracks(reg, max_gap_minutes = 10)
  out    <- summarize_data_gaps(interp, id_col = "ID")
  expect_gt(out$track_duration_hours[1], 0)
})
