#' Standardize GPS Column Names
#'
#' Renames common GPS tracking columns to a standard schema used throughout the
#' package. The function can auto-detect common column names or use a supplied
#' column map.
#'
#' @param raw_data A data frame or tibble containing raw GPS tracking data.
#' @param col_map Optional named character vector mapping standard names to
#'   source column names.
#' @param add_missing_cols Logical. If TRUE, missing mapped columns are added as
#'   NA columns. If FALSE, missing mapped columns cause an error.
#'
#' @return A tibble with standardized GPS columns.
#'
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

  for (std_name in names(col_map)) {
    src_name <- col_map[[std_name]]

    if (std_name %in% names(raw_data)) {
      next
    }

    if (src_name %in% names(raw_data)) {
      names(raw_data)[names(raw_data) == src_name] <- std_name
    } else {
      if (add_missing_cols) {
        warning(
          "Source column '", src_name, "' (mapped to '", std_name,
          "') not found in data. Adding as NA column.",
          call. = FALSE
        )
        raw_data[[std_name]] <- NA
      } else {
        stop(
          "Source column '", src_name, "' (mapped to '", std_name,
          "') not found in data.",
          call. = FALSE
        )
      }
    }
  }

  if ("timestamp" %in% names(raw_data) &&
      !inherits(raw_data$timestamp, "POSIXct")) {
    raw_data$timestamp <- .parse_timestamp(raw_data$timestamp)
  }

  for (coord_col in c("lon", "lat")) {
    if (coord_col %in% names(raw_data)) {
      raw_data[[coord_col]] <- suppressWarnings(
        as.numeric(raw_data[[coord_col]])
      )
    }
  }

  schema_extras <- c("trip_id", "phase", "quality_flag")

  for (col in schema_extras) {
    if (!col %in% names(raw_data)) {
      raw_data[[col]] <- NA_character_
    }
  }

  # Important:
  # validate_gps_data() may return an object with class "validated_gps_data".
  # We call it as a check, but we do NOT assign its return value to raw_data.
  if (exists("validate_gps_data", mode = "function")) {
    validate_gps_data(raw_data)
  }

  tibble::as_tibble(raw_data)
}


.default_col_map <- function(src_names) {
  aliases <- list(
    bird_id = c(
      "bird_id",
      "birdid",
      "id",
      "individual",
      "bird",
      "ring",
      "band_id",
      "animal_id",
      "ind_id",
      "track_id",
      "birdcode",
      "bird_code",
      "tag",
      "tag_id"
    ),
    timestamp = c(
      "timestamp",
      "datetime",
      "date_time",
      "date_time_utc",
      "utc",
      "utc_datetime",
      "utcdatetime",
      "utc_datetime_gmt",
      "gmt",
      "obs_time",
      "fix_time",
      "datetime_gmt",
      "datetime_utc"
    ),
    lon = c(
      "lon",
      "longitude",
      "long",
      "x",
      "lon_dd",
      "longitude_dd",
      "lng"
    ),
    lat = c(
      "lat",
      "latitude",
      "y",
      "lat_dd",
      "latitude_dd"
    )
  )

  src_lower <- tolower(src_names)
  col_map <- character(0)

  for (std_name in names(aliases)) {
    matched <- src_names[src_lower %in% aliases[[std_name]]]

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
      ".\nMissing: ",
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

  formats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M:%S",
    "%Y-%m-%d"
  )

  parsed <- as.POSIXct(rep(NA, length(x)), tz = "UTC")

  for (fmt in formats) {
    attempt <- as.POSIXct(x, format = fmt, tz = "UTC")

    if (sum(!is.na(attempt)) > sum(!is.na(parsed))) {
      parsed <- attempt
    }
  }

  n_failed <- sum(is.na(parsed) & !is.na(x))

  if (n_failed > 0) {
    warning(
      n_failed,
      " timestamp(s) could not be parsed and were set to NA.",
      call. = FALSE
    )
  }

  parsed
}
