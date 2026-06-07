describe("validate_gps_data() additional edge cases", {
  valid_df <- data.frame(
    track_id  = c("bird-01", "bird-01", "bird-02"),
    timestamp = as.POSIXct(
      c("2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-01 10:00:00"),
      tz = "UTC"
    ),
    latitude  = c(-34.52, -34.55, 12.01),
    longitude = c(115.02, 115.06, -45.32),
    stringsAsFactors = FALSE
  )

  it("accepts alternate standard schema names when explicit columns are supplied", {
    alt_df <- data.frame(
      bird_id = c("A", "A", "B"),
      datetime_gmt = as.POSIXct(
        c("2026-01-01 00:00:00", "2026-01-01 01:00:00", "2026-01-01 00:30:00"),
        tz = "UTC"
      ),
      lat = c(-20.1, -20.2, -21.1),
      lon = c(175.1, 175.2, 176.1),
      stringsAsFactors = FALSE
    )

    res <- validate_gps_data(
      alt_df,
      id_col = "bird_id",
      datetime_col = "datetime_gmt",
      lat_col = "lat",
      lon_col = "lon"
    )

    expect_s3_class(res, "validated_gps_data")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(alt_df))
    expect_equal(names(res), names(alt_df))

    schema <- attr(res, "gps_schema")
    expect_equal(schema$id_col, "bird_id")
    expect_equal(schema$datetime_col, "datetime_gmt")
    expect_equal(schema$lat_col, "lat")
    expect_equal(schema$lon_col, "lon")
  })

  it("catches missing required structural columns", {
    missing_id <- valid_df
    missing_id$track_id <- NULL

    expect_error(
      validate_gps_data(missing_id),
      "Missing required ID column|Expected one of",
      ignore.case = TRUE
    )
  })

  it("catches non-parseable datetime values", {
    bad_time <- valid_df
    bad_time$timestamp <- c("not-a-date", "also-not-a-date", "still-not-a-date")

    expect_error(
      validate_gps_data(bad_time),
      "Datetime column could not be parsed|POSIXct",
      ignore.case = TRUE
    )
  })

  it("flags missing coordinate values as validation errors in strict mode", {
    missing_coords <- valid_df
    missing_coords$latitude[2] <- NA_real_

    expect_error(
      validate_gps_data(missing_coords),
      "Coordinates contain missing or out-of-range",
      ignore.case = TRUE
    )
  })

  it("allows missing coordinate values as warnings when strict is FALSE", {
    missing_coords <- valid_df
    missing_coords$latitude[2] <- NA_real_

    expect_warning(
      res <- validate_gps_data(missing_coords, strict = FALSE),
      "Coordinates contain missing or out-of-range",
      ignore.case = TRUE
    )

    expect_s3_class(res, "validated_gps_data")
    expect_equal(nrow(res), nrow(missing_coords))
  })

  it("warns about chronological telemetry sequencing errors when strict is FALSE", {
    out_of_order <- valid_df
    out_of_order$timestamp[1] <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")

    expect_warning(
      res <- validate_gps_data(out_of_order, strict = FALSE),
      "ascending order|track IDs|timestamp",
      ignore.case = TRUE
    )

    expect_s3_class(res, "validated_gps_data")
    expect_equal(nrow(res), nrow(out_of_order))
  })

  it("promotes chronological warning issues to hard errors when strict is TRUE", {
    out_of_order <- valid_df
    out_of_order$timestamp[1] <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")

    expect_error(
      validate_gps_data(out_of_order, strict = TRUE),
      "Strict mode: treating warnings as errors|ascending order",
      ignore.case = TRUE
    )
  })

  it("preserves the original input data columns and rows in the validated object", {
    res <- validate_gps_data(valid_df)

    expect_s3_class(res, "validated_gps_data")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(valid_df))
    expect_equal(names(res), names(valid_df))
    expect_equal(
      as.data.frame(res),
      valid_df,
      ignore_attr = TRUE
    )
  })

  it("stores schema metadata as an attribute", {
    res <- validate_gps_data(valid_df)

    schema <- attr(res, "gps_schema")

    expect_type(schema, "list")
    expect_equal(schema$id_col, "track_id")
    expect_equal(schema$datetime_col, "timestamp")
    expect_equal(schema$lat_col, "latitude")
    expect_equal(schema$lon_col, "longitude")
    expect_true(schema$strict)
  })
})

describe("coerce_track_tbl() additional edge cases", {
  it("uses user-provided column mapping to override nonstandard telemetry names", {
    raw <- data.frame(
      bird = c("A", "B"),
      observed_at = as.POSIXct(
        c("2026-01-01 10:00:00", "2026-01-01 11:00:00"),
        tz = "UTC"
      ),
      y_coord = c(-20.1, -20.2),
      x_coord = c(175.1, 175.2)
    )

    res <- coerce_track_tbl(
      raw,
      col_map = list(
        bird = "track_id",
        observed_at = "timestamp",
        y_coord = "latitude",
        x_coord = "longitude"
      ),
      validate = FALSE,
      as_tibble = FALSE
    )

    expect_s3_class(res, "data.frame")
    expect_named(res, c("track_id", "timestamp", "latitude", "longitude"))

    validated <- coerce_track_tbl(
      raw,
      col_map = list(
        bird = "track_id",
        observed_at = "timestamp",
        y_coord = "latitude",
        x_coord = "longitude"
      ),
      validate = TRUE,
      as_tibble = FALSE
    )

    expect_s3_class(validated, "validated_gps_data")
    expect_s3_class(validated, "data.frame")
    expect_named(validated, c("track_id", "timestamp", "latitude", "longitude"))
  })
  it("errors when col_map is malformed", {
    raw <- data.frame(
      id = "A",
      ts = Sys.time(),
      lat = -20,
      lon = 175
    )

    expect_error(
      coerce_track_tbl(raw, col_map = c("track_id", "timestamp")),
      "col_map|named",
      ignore.case = TRUE
    )
  })

  it("keeps nonstandard extra columns after standardized telemetry columns", {
    raw <- data.frame(
      id = c("A", "A"),
      ts = as.POSIXct(
        c("2026-01-01 10:00:00", "2026-01-01 11:00:00"),
        tz = "UTC"
      ),
      lat = c(-20.1, -20.2),
      lon = c(175.1, 175.2),
      device = c("gps-1", "gps-1")
    )

    res <- coerce_track_tbl(raw, validate = FALSE, as_tibble = FALSE)

    expect_named(res, c("track_id", "timestamp", "latitude", "longitude", "device"))
    expect_equal(res$device, c("gps-1", "gps-1"))
  })
})
