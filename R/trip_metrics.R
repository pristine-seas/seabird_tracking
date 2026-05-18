

############################################################
# calc_trip_distance()
# Calculate straight-line trip distance from first trip point
# to last trip point.
############################################################

#' Calculate Straight-Line Trip Distance
#'
#' Calculates the straight-line distance between the first and last point
#' of each trip. This is different from total path length because it only
#' measures the direct distance from trip start to trip end.
#'
#' @param trip_data A data frame containing trip-segmented tracking data.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Character. Column identifying each trip.
#' @param datetime_col Character. Datetime column used to order points.
#' @param lon_col Character. Longitude column.
#' @param lat_col Character. Latitude column.
#' @param output_col Character. Name of output distance column.
#'
#' @return A data frame with one row per bird-trip and straight-line trip distance in meters.
#' @export
calc_trip_distance <- function(trip_data,
                               bird_id_col = "track_id",
                               trip_id_col = "trip_id",
                               datetime_col = "datetime_gmt",
                               lon_col = "longitude",
                               lat_col = "latitude",
                               output_col = "trip_distance_m") {

  needed <- c(bird_id_col, trip_id_col, datetime_col, lon_col, lat_col)
  missing_cols <- setdiff(needed, names(trip_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  trip_data %>%
    dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
    dplyr::arrange(.data[[bird_id_col]], .data[[trip_id_col]], .data[[datetime_col]]) %>%
    dplyr::group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    dplyr::summarise(
      start_lon = dplyr::first(.data[[lon_col]]),
      start_lat = dplyr::first(.data[[lat_col]]),
      end_lon   = dplyr::last(.data[[lon_col]]),
      end_lat   = dplyr::last(.data[[lat_col]]),
      n_points  = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!output_col := geosphere::distHaversine(
        cbind(start_lon, start_lat),
        cbind(end_lon, end_lat)
      )
    )
}

############################################################
# calc_trip_duration()
# Calculate duration of each trip from departure to return.
############################################################

#' Calculate Trip Duration
#'
#' Calculates the duration of each trip using the first and last timestamp
#' within each trip.
#'
#' @param trip_data A data frame containing trip-segmented tracking data.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Character. Column identifying each trip.
#' @param datetime_col Character. Datetime column.
#' @param units Character. Time units for duration. One of `"secs"`, `"mins"`, `"hours"`, or `"days"`.
#' @param output_col Character. Name of output duration column.
#'
#' @return A data frame with one row per bird-trip and trip duration.
#' @export
calc_trip_duration <- function(trip_data,
                               bird_id_col = "track_id",
                               trip_id_col = "trip_id",
                               datetime_col = "datetime_gmt",
                               units = "hours",
                               output_col = "trip_duration") {

  needed <- c(bird_id_col, trip_id_col, datetime_col)
  missing_cols <- setdiff(needed, names(trip_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  valid_units <- c("secs", "mins", "hours", "days")

  if (!units %in% valid_units) {
    stop("`units` must be one of: ", paste(valid_units, collapse = ", "))
  }

  assert_datetime_tz(trip_data, datetime_col)

  trip_data %>%
    dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
    dplyr::arrange(.data[[bird_id_col]], .data[[trip_id_col]], .data[[datetime_col]]) %>%
    dplyr::group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    dplyr::summarise(
      trip_start = min(.data[[datetime_col]], na.rm = TRUE),
      trip_end   = max(.data[[datetime_col]], na.rm = TRUE),
      n_points   = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!output_col := as.numeric(
        difftime(trip_end, trip_start, units = units)
      )
    )
}

############################################################
# calc_path_length()
# Calculate total traveled path length by summing step distances.
############################################################

#' Calculate Path Length
#'
#' Calculates total traveled path length by summing the distance between
#' consecutive GPS points within each bird or bird-trip group.
#'
#' @param track_data A data frame containing ordered GPS tracking data.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Optional character. Column identifying trips. If `NULL`,
#'   path length is calculated by bird only.
#' @param datetime_col Character. Datetime column used to order points.
#' @param lon_col Character. Longitude column.
#' @param lat_col Character. Latitude column.
#' @param output_col Character. Name of output path length column.
#'
#' @return A data frame with total path length in meters.
#' @export
calc_path_length <- function(track_data,
                             bird_id_col = "track_id",
                             trip_id_col = "trip_id",
                             datetime_col = "datetime_gmt",
                             lon_col = "longitude",
                             lat_col = "latitude",
                             output_col = "path_length_m") {

  needed <- c(bird_id_col, datetime_col, lon_col, lat_col)

  if (!is.null(trip_id_col)) {
    needed <- c(needed, trip_id_col)
  }

  missing_cols <- setdiff(needed, names(track_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  group_cols <- bird_id_col

  if (!is.null(trip_id_col)) {
    group_cols <- c(group_cols, trip_id_col)
  }

  track_data %>%
    dplyr::filter(
      !is.na(.data[[lon_col]]),
      !is.na(.data[[lat_col]])
    ) %>%
    {
      if (!is.null(trip_id_col)) {
        dplyr::filter(., !is.na(.data[[trip_id_col]]))
      } else {
        .
      }
    } %>%
    dplyr::arrange(.data[[bird_id_col]], .data[[datetime_col]]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      next_lon = dplyr::lead(.data[[lon_col]]),
      next_lat = dplyr::lead(.data[[lat_col]]),
      step_m = geosphere::distHaversine(
        cbind(.data[[lon_col]], .data[[lat_col]]),
        cbind(next_lon, next_lat)
      )
    ) %>%
    dplyr::summarise(
      !!output_col := sum(step_m, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    )
}

############################################################
# calc_max_distance_from_colony()
# Find maximum distance from colony during each trip or individual track.
############################################################

#' Calculate Maximum Distance From Colony
#'
#' Calculates the maximum distance reached from the colony for each trip
#' or individual bird.
#'
#' @param trip_data A data frame containing GPS tracking data.
#' @param colony_coords Named numeric vector with `lon` and `lat`, for example
#'   `c(lon = -175.3, lat = -19.8)`.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Optional character. Column identifying trips. If `NULL`,
#'   maximum distance is calculated by bird only.
#' @param lon_col Character. Longitude column.
#' @param lat_col Character. Latitude column.
#' @param output_col Character. Name of output distance column.
#'
#' @return A data frame with maximum distance from colony in meters.
#' @export
calc_max_distance_from_colony <- function(trip_data,
                                          colony_coords,
                                          bird_id_col = "track_id",
                                          trip_id_col = "trip_id",
                                          lon_col = "longitude",
                                          lat_col = "latitude",
                                          output_col = "max_distance_from_colony_m") {

  needed <- c(bird_id_col, lon_col, lat_col)

  if (!is.null(trip_id_col)) {
    needed <- c(needed, trip_id_col)
  }

  missing_cols <- setdiff(needed, names(trip_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  if (!is.numeric(colony_coords) ||
      !all(c("lon", "lat") %in% names(colony_coords))) {
    stop("`colony_coords` must be a named numeric vector with names `lon` and `lat`.")
  }

  group_cols <- bird_id_col

  if (!is.null(trip_id_col)) {
    group_cols <- c(group_cols, trip_id_col)
  }

  trip_data %>%
    dplyr::filter(
      !is.na(.data[[lon_col]]),
      !is.na(.data[[lat_col]])
    ) %>%
    {
      if (!is.null(trip_id_col)) {
        dplyr::filter(., !is.na(.data[[trip_id_col]]))
      } else {
        .
      }
    } %>%
    dplyr::mutate(
      distance_from_colony_m = geosphere::distHaversine(
        cbind(.data[[lon_col]], .data[[lat_col]]),
        matrix(
          c(colony_coords[["lon"]], colony_coords[["lat"]]),
          nrow = nrow(.),
          ncol = 2,
          byrow = TRUE
        )
      )
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      !!output_col := max(distance_from_colony_m, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    )
}
