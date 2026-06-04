#' Combine Date and Time Columns into a Datetime Object
#'
#' Combines separate date and time columns into a single POSIXct datetime column.
#'
#' @param track A data frame or tibble containing tracking records.
#' @param date_col Character. Name of the column containing dates.
#'   Default is `"date_gmt"`.
#' @param time_col Character. Name of the column containing times.
#'   Default is `"time_gmt"`.
#' @param tz Character. Time zone. Default is `"UTC"`.
#' @param output_col Character. Name of the new datetime column.
#'   Default is `"datetime_gmt"`.
#'
#' @return A data frame with a new POSIXct datetime column.
#'
#' @export
make_datetime <- function(track,
                          date_col = "date_gmt",
                          time_col = "time_gmt",
                          tz = "UTC",
                          output_col = "datetime_gmt") {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(date_col, time_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  track |>
    dplyr::mutate(
      !!output_col := lubridate::ymd_hms(
        paste(.data[[date_col]], .data[[time_col]]),
        tz = tz
      )
    )
}


#' Identify and Flag Colony Attendance Periods
#'
#' Computes the great-circle distance from telemetry positions to colony
#' coordinates and flags points that fall within a defined colony radius.
#'
#' @param track A data frame or tibble containing track coordinates.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param colony_lon_col Character. Colony longitude column.
#'   Default is `"lon_colony"`.
#' @param colony_lat_col Character. Colony latitude column.
#'   Default is `"lat_colony"`.
#' @param radius_m Numeric. Colony attendance radius in meters.
#'   Default is `200`.
#' @param distance_col Character. Name of calculated distance column.
#'   Default is `"dist_to_colony_m"`.
#' @param colony_flag_col Character. Name of logical colony flag column.
#'   Default is `"at_colony"`.
#'
#' @return A data frame with distance-to-colony and colony-attendance columns.
#'
#' @export
identify_colony_visits <- function(track,
                                   lon_col = "longitude",
                                   lat_col = "latitude",
                                   colony_lon_col = "lon_colony",
                                   colony_lat_col = "lat_colony",
                                   radius_m = 200,
                                   distance_col = "dist_to_colony_m",
                                   colony_flag_col = "at_colony") {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(lon_col, lat_col, colony_lon_col, colony_lat_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(radius_m) || length(radius_m) != 1 || radius_m < 0) {
    stop("`radius_m` must be a single non-negative number.", call. = FALSE)
  }

  point_coords <- cbind(track[[lon_col]], track[[lat_col]])
  colony_coords <- cbind(track[[colony_lon_col]], track[[colony_lat_col]])

  dists <- geosphere::distHaversine(point_coords, colony_coords)

  track |>
    dplyr::mutate(
      !!distance_col := dists,
      !!colony_flag_col := .data[[distance_col]] <= radius_m
    )
}


#' Segment Tracking Series into Foraging Trips
#'
#' Assigns sequential trip IDs to chronological sequences of points away from
#' the colony. Points at the colony receive `NA` for the trip ID.
#'
#' @param track A data frame or tibble containing colony-flagged tracking points.
#' @param bird_id_col Character. Individual or track ID column.
#'   Default is `"track_id"`.
#' @param datetime_col Character. Datetime column.
#'   Default is `"datetime_gmt"`.
#' @param colony_flag_col Character. Logical colony-attendance column.
#'   Default is `"at_colony"`.
#' @param trip_id_col Character. Output trip ID column.
#'   Default is `"trip_id"`.
#'
#' @return An ordered data frame with a trip ID column.
#'
#' @export
segment_trips <- function(track,
                          bird_id_col = "track_id",
                          datetime_col = "datetime_gmt",
                          colony_flag_col = "at_colony",
                          trip_id_col = "trip_id") {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(bird_id_col, datetime_col, colony_flag_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  track |>
    dplyr::arrange(.data[[bird_id_col]], .data[[datetime_col]]) |>
    dplyr::group_by(.data[[bird_id_col]]) |>
    dplyr::mutate(
      prev_at_colony = dplyr::lag(.data[[colony_flag_col]], default = TRUE),
      departed = .data$prev_at_colony & !.data[[colony_flag_col]],
      trip_counter = cumsum(.data$departed),
      !!trip_id_col := dplyr::if_else(
        .data[[colony_flag_col]],
        NA_integer_,
        as.integer(.data$trip_counter)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -dplyr::all_of(c("prev_at_colony", "departed", "trip_counter"))
    )
}


#' Classify Behavioral Flight Phases
#'
#' Classifies tracking points into simple rule-based phases using point-to-point
#' speed calculations.
#'
#' @param track A data frame or tibble containing segmented tracks.
#' @param bird_id_col Character. Individual or track ID column.
#'   Default is `"track_id"`.
#' @param trip_id_col Character. Trip ID column. Default is `"trip_id"`.
#' @param datetime_col Character. Datetime column.
#'   Default is `"datetime_gmt"`.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param phase_col Character. Output phase column. Default is `"phase"`.
#' @param commute_speed_threshold Numeric. Speeds at or above this value are
#'   labeled `"commuting"`. Default is `5`.
#' @param forage_speed_threshold Numeric. Speeds at or below this value are
#'   labeled `"foraging"`. Default is `1`.
#'
#' @return A data frame with speed calculations and a behavioral phase column.
#'
#' @export
classify_trip_phase <- function(track,
                                bird_id_col = "track_id",
                                trip_id_col = "trip_id",
                                datetime_col = "datetime_gmt",
                                lon_col = "longitude",
                                lat_col = "latitude",
                                phase_col = "phase",
                                commute_speed_threshold = 5,
                                forage_speed_threshold = 1) {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(bird_id_col, trip_id_col, datetime_col, lon_col, lat_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- track |>
    dplyr::arrange(.data[[bird_id_col]], .data[[datetime_col]]) |>
    dplyr::group_by(.data[[bird_id_col]], .data[[trip_id_col]]) |>
    dplyr::mutate(
      next_lon = dplyr::lead(.data[[lon_col]]),
      next_lat = dplyr::lead(.data[[lat_col]]),
      next_time = dplyr::lead(.data[[datetime_col]]),
      step_m = geosphere::distHaversine(
        cbind(.data[[lon_col]], .data[[lat_col]]),
        cbind(.data$next_lon, .data$next_lat)
      ),
      dt_s = as.numeric(
        difftime(.data$next_time, .data[[datetime_col]], units = "secs")
      ),
      speed_m_s = .data$step_m / .data$dt_s
    ) |>
    dplyr::ungroup()

  out |>
    dplyr::mutate(
      !!phase_col := dplyr::case_when(
        is.na(.data[[trip_id_col]]) ~ "colony",
        is.na(.data$speed_m_s) ~ "unknown",
        .data$speed_m_s >= commute_speed_threshold ~ "commuting",
        .data$speed_m_s <= forage_speed_threshold ~ "foraging",
        TRUE ~ "mixed"
      )
    ) |>
    dplyr::select(-dplyr::all_of(c("next_lon", "next_lat", "next_time")))
}


#' Label Day, Night, Dawn, and Dusk Periods
#'
#' Adds diel period labels to telemetry points using sunrise and sunset times
#' calculated from the track coordinates.
#'
#' @param track A data frame or tibble containing tracking points.
#' @param datetime_col Character. Datetime column.
#'   Default is `"datetime_gmt"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param timezone Character. Time zone used for solar calculations.
#'   Default is `"UTC"`.
#' @param output_col Character. Output diel-period column.
#'   Default is `"diel_period"`.
#' @param twilight_buffer_mins Numeric. Minutes around sunrise and sunset to
#'   label as dawn or dusk. Default is `45`.
#'
#' @return A data frame with a diel-period column.
#'
#' @export
label_day_night_period <- function(track,
                                   datetime_col = "datetime_gmt",
                                   lat_col = "latitude",
                                   lon_col = "longitude",
                                   timezone = "UTC",
                                   output_col = "diel_period",
                                   twilight_buffer_mins = 45) {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(datetime_col, lat_col, lon_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dates_only <- as.Date(
    lubridate::with_tz(track[[datetime_col]], tzone = timezone)
  )

  sun_tbl <- suncalc::getSunlightTimes(
    date = unique(dates_only),
    lat = mean(track[[lat_col]], na.rm = TRUE),
    lon = mean(track[[lon_col]], na.rm = TRUE),
    keep = c("sunrise", "sunset"),
    tz = timezone
  ) |>
    dplyr::mutate(
      dawn_start = .data$sunrise - lubridate::minutes(twilight_buffer_mins),
      dawn_end = .data$sunrise + lubridate::minutes(twilight_buffer_mins),
      dusk_start = .data$sunset - lubridate::minutes(twilight_buffer_mins),
      dusk_end = .data$sunset + lubridate::minutes(twilight_buffer_mins)
    ) |>
    dplyr::select(
      .data$date,
      .data$dawn_start,
      .data$dawn_end,
      .data$dusk_start,
      .data$dusk_end
    )

  track |>
    dplyr::mutate(
      date_only = as.Date(
        lubridate::with_tz(.data[[datetime_col]], tzone = timezone)
      )
    ) |>
    dplyr::left_join(sun_tbl, by = c("date_only" = "date")) |>
    dplyr::mutate(
      !!output_col := dplyr::case_when(
        .data[[datetime_col]] >= .data$dawn_start &
          .data[[datetime_col]] <= .data$dawn_end ~ "dawn",
        .data[[datetime_col]] >= .data$dusk_start &
          .data[[datetime_col]] <= .data$dusk_end ~ "dusk",
        .data[[datetime_col]] > .data$dawn_end &
          .data[[datetime_col]] < .data$dusk_start ~ "day",
        TRUE ~ "night"
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(
        c("date_only", "dawn_start", "dawn_end", "dusk_start", "dusk_end")
      )
    )
}


#' Summarize Trips
#'
#' Collapses tracking points into trip-level summaries, including duration,
#' number of fixes, maximum distance from colony, diel proportions, and
#' behavioral phase proportions when those columns are available.
#'
#' @param track A data frame or tibble containing trip-segmented tracking data.
#' @param bird_id_col Character. Individual or track ID column.
#'   Default is `"track_id"`.
#' @param trip_id_col Character. Trip ID column. Default is `"trip_id"`.
#' @param datetime_col Character. Datetime column.
#'   Default is `"datetime_gmt"`.
#' @param distance_col Character. Distance-to-colony column.
#'   Default is `"dist_to_colony_m"`.
#' @param phase_col Character. Behavioral phase column. Default is `"phase"`.
#'
#' @return A data frame with one row per bird-trip.
#'
#' @export
summarize_trips <- function(track,
                            bird_id_col = "track_id",
                            trip_id_col = "trip_id",
                            datetime_col = "datetime_gmt",
                            distance_col = "dist_to_colony_m",
                            phase_col = "phase") {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  needed <- c(bird_id_col, trip_id_col, datetime_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  has_distance <- distance_col %in% names(track)
  has_diel <- "diel_period" %in% names(track)
  has_phase <- phase_col %in% names(track)

  track |>
    dplyr::filter(!is.na(.data[[trip_id_col]])) |>
    dplyr::arrange(.data[[bird_id_col]], .data[[trip_id_col]], .data[[datetime_col]]) |>
    dplyr::group_by(.data[[bird_id_col]], .data[[trip_id_col]]) |>
    dplyr::summarise(
      trip_start = min(.data[[datetime_col]], na.rm = TRUE),
      trip_end = max(.data[[datetime_col]], na.rm = TRUE),
      duration_h = as.numeric(
        difftime(.data$trip_end, .data$trip_start, units = "hours")
      ),
      n_fixes = dplyr::n(),
      max_dist_to_colony_m = if (has_distance) {
        max(.data[[distance_col]], na.rm = TRUE)
      } else {
        NA_real_
      },
      prop_day = if (has_diel) {
        mean(.data$diel_period == "day", na.rm = TRUE)
      } else {
        NA_real_
      },
      prop_night = if (has_diel) {
        mean(.data$diel_period == "night", na.rm = TRUE)
      } else {
        NA_real_
      },
      prop_commuting = if (has_phase) {
        mean(.data[[phase_col]] == "commuting", na.rm = TRUE)
      } else {
        NA_real_
      },
      prop_foraging = if (has_phase) {
        mean(.data[[phase_col]] == "foraging", na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}
