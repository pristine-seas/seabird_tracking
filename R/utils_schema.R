# =============================================================================
# utils_schema.R
# GPS schema standardization and validation utilities
# =============================================================================

#' Validate GPS Tracking Data
#'
#' Checks that a GPS tracking table contains the required identity, datetime,
#' longitude, and latitude fields and that those fields can be safely used by
#' downstream package functions.
#'
#' @param data A data frame, tibble, or sf object.
#' @param strict Logical. If TRUE, validation problems error. If FALSE, problems
#'   that can be tolerated are warnings.
#' @param id_col Optional ID column name.
#' @param datetime_col Optional datetime column name.
#' @param lon_col Optional longitude column name.
#' @param lat_col Optional latitude column name.
#'
#' @return The input object marked with class `validated_gps_data`.
#' @export
validate_gps_data <- function(data,
                              strict = TRUE,
                              id_col = NULL,
                              datetime_col = NULL,
                              lon_col = NULL,
                              lat_col = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame, tibble, or sf object.", call. = FALSE)
  }

  if (!is.logical(strict) || length(strict) != 1 || is.na(strict)) {
    stop("`strict` must be TRUE or FALSE.", call. = FALSE)
  }

  detect_col <- function(candidates, label) {
    hit <- candidates[candidates %in% names(data)]

    if (length(hit) == 0) {
      stop(
        "Missing required ", label, " column. Expected one of: ",
        paste(candidates, collapse = ", "),
        call. = FALSE
      )
    }

    hit[[1]]
  }

  id_col <- id_col %||% detect_col(c("bird_id", "track_id", "id", "ID"), "ID")

  datetime_col <- datetime_col %||% detect_col(
    c("timestamp", "datetime_gmt", "datetime", "time", "DateTime"),
    "datetime"
  )

  lon_col <- lon_col %||% detect_col(
    c("lon", "longitude", "Longitude", "x", "lng"),
    "longitude"
  )

  lat_col <- lat_col %||% detect_col(
    c("lat", "latitude", "Latitude", "y"),
    "latitude"
  )

  required_cols <- c(id_col, datetime_col, lon_col, lat_col)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(is.na(data[[id_col]]) | data[[id_col]] == "")) {
    msg <- "ID column contains missing or blank values."

    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  dt <- data[[datetime_col]]

  if (!inherits(dt, "POSIXct") && !inherits(dt, "POSIXt")) {
    parsed_dt <- .parse_timestamp(dt)

    if (all(is.na(parsed_dt))) {
      if (strict) {
        stop("Datetime column could not be parsed as POSIXct.", call. = FALSE)
      } else {
        warning("Datetime column could not be parsed as POSIXct.", call. = FALSE)
      }
    }

    if (any(is.na(parsed_dt)) && !all(is.na(parsed_dt))) {
      msg <- "Datetime column contains unparseable values."

      if (strict) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }

    dt_check <- parsed_dt
  } else {
    dt_check <- as.POSIXct(dt, tz = "UTC")

    if (any(is.na(dt_check))) {
      msg <- "Datetime column contains missing values."

      if (strict) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }

  lon <- suppressWarnings(as.numeric(data[[lon_col]]))
  lat <- suppressWarnings(as.numeric(data[[lat_col]]))

  if (all(is.na(lon))) {
    msg <- "Longitude column could not be converted to numeric values."

    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  if (all(is.na(lat))) {
    msg <- "Latitude column could not be converted to numeric values."

    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  bad_coord <- is.na(lon) |
    is.na(lat) |
    lon < -180 |
    lon > 180 |
    lat < -90 |
    lat > 90

  if (any(bad_coord)) {
    msg <- "Coordinates contain missing or out-of-range longitude/latitude values."

    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  order_problem <- FALSE

  if (length(dt_check) > 1 && !all(is.na(dt_check))) {
    split_idx <- split(seq_along(dt_check), data[[id_col]])

    order_problem <- any(vapply(split_idx, function(idx) {
      idx <- idx[!is.na(dt_check[idx])]

      if (length(idx) <= 1) {
        return(FALSE)
      }

      any(diff(as.numeric(dt_check[idx])) < 0)
    }, logical(1)))
  }

  if (order_problem) {
    msg <- "Timestamps are not in ascending order within track IDs."

    if (strict) {
      stop(
        "Strict mode: treating warnings as errors. ",
        msg,
        call. = FALSE
      )
    } else {
      warning(msg, call. = FALSE)
    }
  }

  attr(data, "gps_schema") <- list(
    id_col = id_col,
    datetime_col = datetime_col,
    lon_col = lon_col,
    lat_col = lat_col,
    strict = strict
  )

  if (!inherits(data, "validated_gps_data")) {
    class(data) <- c("validated_gps_data", class(data))
  }

  data
}


#' Coerce Tracking Data to a Standard Track Table
#'
#' @param data A data frame or tibble.
#' @param col_map Optional list or named character vector mapping source names
#'   to standard names.
#' @param validate Logical. If TRUE, run `validate_gps_data()`.
#' @param as_tibble Logical. If TRUE, return a tibble when tibble is installed.
#'
#' @return A standardized data frame or tibble.
#' @export
coerce_track_tbl <- function(data,
                             col_map = NULL,
                             validate = TRUE,
                             as_tibble = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  default_map <- list(
    id = "track_id",
    tid = "track_id",
    track = "track_id",
    bird_id = "track_id",
    animal_id = "track_id",
    tag_id = "track_id",
    time = "timestamp",
    ts = "timestamp",
    datetime = "timestamp",
    datetime_gmt = "timestamp",
    date_time = "timestamp",
    lat = "latitude",
    Latitude = "latitude",
    y = "latitude",
    lon = "longitude",
    lng = "longitude",
    Longitude = "longitude",
    x = "longitude"
  )

  if (!is.null(col_map)) {
    if (is.character(col_map)) {
      if (is.null(names(col_map)) || any(names(col_map) == "")) {
        stop("`col_map` must be named when supplied as a character vector.", call. = FALSE)
      }

      col_map <- as.list(col_map)
    } else if (!is.list(col_map)) {
      stop("`col_map` must be a named character vector or list.", call. = FALSE)
    }

    if (is.null(names(col_map)) || any(names(col_map) == "")) {
      stop("`col_map` must be named.", call. = FALSE)
    }

    default_map <- c(default_map, col_map)
  }

  for (old_name in intersect(names(default_map), names(data))) {
    new_name <- default_map[[old_name]]

    if (!is.character(new_name) || length(new_name) != 1) {
      stop("Each `col_map` value must be a single character string.", call. = FALSE)
    }

    names(data)[names(data) == old_name] <- new_name
  }

  standard_cols <- c("track_id", "timestamp", "latitude", "longitude")
  standard_present <- intersect(standard_cols, names(data))
  other_cols <- setdiff(names(data), standard_present)
  data <- data[, c(standard_present, other_cols), drop = FALSE]

  if ("timestamp" %in% names(data) &&
      !inherits(data$timestamp, "POSIXct")) {
    data$timestamp <- .parse_timestamp(data$timestamp)
  }

  for (coord_col in c("latitude", "longitude")) {
    if (coord_col %in% names(data)) {
      data[[coord_col]] <- suppressWarnings(as.numeric(data[[coord_col]]))
    }
  }

  if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
    data <- tibble::as_tibble(data)
  }

  if (validate) {
    data <- validate_gps_data(data, strict = FALSE)
  }

  data
}


#' Standardize GPS Column Names
#'
#' @param raw_data A data frame or tibble.
#' @param col_map Optional named character vector mapping standard names to
#'   source column names.
#' @param add_missing_cols Logical. If TRUE, missing mapped columns are added as
#'   NA columns.
#'
#' @return A tibble with standardized GPS columns.
#' @export
standardize_gps_columns <- function(raw_data,
                                    col_map = NULL,
                                    add_missing_cols = TRUE) {
  if (!is.data.frame(raw_data)) {
    stop("raw_data must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.logical(add_missing_cols) ||
      length(add_missing_cols) != 1 ||
      is.na(add_missing_cols)) {
    stop("add_missing_cols must be TRUE or FALSE.", call. = FALSE)
  }

  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)

  if (is.null(col_map)) {
    col_map <- .default_col_map(names(raw_data))
  }

  .validate_col_map(col_map)

  added_missing <- character(0)

  for (std_name in names(col_map)) {
    src_name <- col_map[[std_name]]

    if (std_name %in% names(raw_data)) {
      next
    }

    if (src_name %in% names(raw_data)) {
      names(raw_data)[names(raw_data) == src_name] <- std_name
    } else if (add_missing_cols) {
      raw_data[[std_name]] <- NA
      added_missing <- c(
        added_missing,
        paste0("'", src_name, "' (mapped to '", std_name, "')")
      )
    } else {
      stop(
        "Source column '", src_name, "' (mapped to '", std_name,
        "') not found in data.",
        call. = FALSE
      )
    }
  }

  if (length(added_missing) > 0) {
    warning(
      "Source column(s) ",
      paste(added_missing, collapse = ", "),
      " not found in data. Adding as NA column.",
      call. = FALSE
    )
  }

  if ("timestamp" %in% names(raw_data) &&
      !inherits(raw_data$timestamp, "POSIXct")) {
    raw_data$timestamp <- .parse_timestamp(raw_data$timestamp)
  }

  for (coord_col in c("lon", "lat")) {
    if (coord_col %in% names(raw_data)) {
      raw_data[[coord_col]] <- suppressWarnings(as.numeric(raw_data[[coord_col]]))
    }
  }

  schema_extras <- c("trip_id", "phase", "quality_flag")

  for (col in schema_extras) {
    if (!col %in% names(raw_data)) {
      raw_data[[col]] <- NA_character_
    }
  }

  # Only validate fully detected schemas. If missing columns were intentionally
  # added as NA, returning the standardized table is useful and should not error.
  if (length(added_missing) == 0 && exists("validate_gps_data", mode = "function")) {
    validate_gps_data(raw_data, strict = FALSE)
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(raw_data))
  }

  raw_data
}


.default_col_map <- function(src_names) {
  aliases <- list(
    bird_id = c(
      "bird_id", "birdid", "id", "individual", "bird", "ring", "band_id",
      "animal_id", "ind_id", "track_id", "birdcode", "bird_code", "tag",
      "tag_id", "ID", "BirdCode"
    ),
    timestamp = c(
      "timestamp", "datetime", "date_time", "date_time_utc", "utc",
      "utc_datetime", "utcdatetime", "utc_datetime_gmt", "gmt", "obs_time",
      "fix_time", "datetime_gmt", "datetime_utc", "time", "UTC_DateTime"
    ),
    lon = c(
      "lon", "longitude", "long", "x", "lon_dd", "longitude_dd", "lng",
      "Lon_DD", "Longitude"
    ),
    lat = c(
      "lat", "latitude", "y", "lat_dd", "latitude_dd", "Lat_DD", "Latitude"
    )
  )

  src_lower <- tolower(src_names)
  col_map <- character(0)

  for (std_name in names(aliases)) {
    alias_lower <- tolower(aliases[[std_name]])
    matched <- src_names[src_lower %in% alias_lower]

    if (length(matched) > 0) {
      col_map[[std_name]] <- matched[[1]]
    } else {
      col_map[[std_name]] <- std_name
    }
  }

  col_map
}


.validate_col_map <- function(col_map) {
  if (!is.character(col_map)) {
    stop("col_map must be a named character vector.", call. = FALSE)
  }

  if (is.null(names(col_map)) || any(names(col_map) == "")) {
    stop("col_map must be a named character vector.", call. = FALSE)
  }

  required <- c("bird_id", "timestamp", "lon", "lat")
  missing <- setdiff(required, names(col_map))

  if (length(missing) > 0) {
    stop(
      "col_map must include mappings for: ",
      paste(required, collapse = ", "),
      ". Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.parse_timestamp <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }

  x_chr <- as.character(x)

  if (requireNamespace("lubridate", quietly = TRUE)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_chr,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy",
        "Ymd HMS", "Ymd HM", "Ymd"
      ),
      tz = "UTC"
    ))

    return(as.POSIXct(parsed, tz = "UTC"))
  }

  suppressWarnings(as.POSIXct(x_chr, tz = "UTC"))
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' @export
as.data.frame.validated_gps_data <- function(x,
                                             row.names = NULL,
                                             optional = FALSE,
                                             ...) {
  class(x) <- setdiff(class(x), "validated_gps_data")
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}
