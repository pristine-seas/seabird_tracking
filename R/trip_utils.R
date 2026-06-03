library(dplyr)
library(lubridate)
library(geosphere)

#' Combine Date and Time Columns into a Unified Datetime Object
#'
#' @param track A data frame or tibble containing spatial bird telemetry records.
#' @param date_col Character string. Name of the column containing dates. Default: `"date_gmt"`.
#' @param time_col Character string. Name of the column containing times. Default: `"time_gmt"`.
#' @param tz Character string. Timezone specification. Default: `"UTC"`.
#' @param output_col Character string. Name of the new generated datetime column. Default: `"datetime_gmt"`.
#'
#' @return A mutated data frame containing the converted POSIXct datetime tracking column.
#' @export
make_datetime <- function(track,
                          date_col = "date_gmt",
                          time_col = "time_gmt",
                          tz = "UTC",
                          output_col = "datetime_gmt") {
  
  if (!date_col %in% names(track)) stop(paste("Missing", date_col))
  if (!time_col %in% names(track)) stop(paste("Missing", time_col))
  
  track %>%
    mutate(
      !!output_col := ymd_hms(paste(.data[[date_col]], .data[[time_col]]), tz = tz)
    )
}

#' Identify and Flag Colony Attendance Periods
#'
#' Computes the great-circle distance from telemetry positions to designated colony breeding sites,
#' flagging records falling within a defined spatial buffer radius.
#'
#' @param track A data frame or tibble containing standardized track coordinates.
#' @param lon_col Character string. Longitudinal track column name. Default: `"longitude"`.
#' @param lat_col Character string. Latitudinal track column name. Default: `"latitude"`.
#' @param colony_lon_col Character string. Column name containing colony longitude coordinates. Default: `"lon_colony"`.
#' @param colony_lat_col Character string. Column name containing colony latitude coordinates. Default: `"lat_colony"`.
#' @param radius_m Numeric. Spatial attendance threshold radius in meters. Default: `200`.
#' @param distance_col Character string. Name of the calculated distance output column. Default: `"dist_to_colony_m"`.
#' @param colony_flag_col Character string. Name of the logical output colony status flag column. Default: `"at_colony"`.
#'
#' @return A data frame containing calculated distance metrics and a logical attendance indicator column.
#' @export
identify_colony_visits <- function(track,
                                   lon_col = "longitude",
                                   lat_col = "latitude",
                                   colony_lon_col = "lon_colony",
                                   colony_lat_col = "lat_colony",
                                   radius_m = 200,
                                   distance_col = "dist_to_colony_m",
                                   colony_flag_col = "at_colony") {
  
  needed <- c(lon_col, lat_col, colony_lon_col, colony_lat_col)
  missing_cols <- setdiff(needed, names(track))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  
  point_coords <- cbind(track[[lon_col]], track[[lat_col]])
  colony_coords <- cbind(track[[colony_lon_col]], track[[colony_lat_col]])
  
  dists <- geosphere::distHaversine(point_coords, colony_coords)
  
  track %>%
    mutate(
      !!distance_col := dists,
      !!colony_flag_col := .data[[distance_col]] <= radius_m
    )
}

#' Segment Tracking Series into Foraging Trips Away From Colony
#'
#' Assigns sequential trip indices to chronological strings of points captured outside of 
#' flagged breeding colony boundary zones. Attendance tracks evaluate as NA.
#'
#' @param track A data frame or tibble containing colony flagged telemetry points.
#' @param bird_id_col Character string. Unique track grouping column name. Default: `"track_id"`.
#' @param datetime_col Character string. Chronological POSIXct standard datetime field. Default: `"datetime_gmt"`.
#' @param colony_flag_col Character string. Logical colony attendance status vector name. Default: `"at_colony"`.
#' @param trip_id_col Character string. Unique index output column name assigned to valid trips. Default: `"trip_id"`.
#'
#' @return An ordered data frame appended with calculated discrete sequential trip identifiers.
#' @export
segment_trips <- function(track,
                          bird_id_col = "track_id",
                          datetime_col = "datetime_gmt",
                          colony_flag_col = "at_colony",
                          trip_id_col = "trip_id") {
  
  needed <- c(bird_id_col, datetime_col, colony_flag_col)
  missing_cols <- setdiff(needed, names(track))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  
  track %>%
    arrange(.data[[bird_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]]) %>%
    mutate(
      prev_at_colony = lag(.data[[colony_flag_col]], default = TRUE),
      departed = prev_at_colony & !.data[[colony_flag_col]],
      trip_counter = cumsum(departed),
      !!trip_id_col := if_else(.data[[colony_flag_col]], NA_integer_, as.integer(trip_counter))
    ) %>%
    ungroup() %>%
    select(-prev_at_colony, -departed, -trip_counter)
}

#' Classify Behavioral Flight Phases via Rule-Based Velocity Filtering
#'
#' Characterizes individual behavioral segments based on point-to-point step-speed calculations.
#'
#' @param track A data frame or tibble consisting of segmented foraging tracks.
#' @param bird_id_col Character string. Unique tracker identity column name. Default: `"track_id"`.
#' @param trip_id_col Character string. Target trip identification column designation. Default: `"trip_id"`.
#' @param datetime_col Character string. Datetime column descriptor. Default: `"datetime_gmt"`.
#' @param lon_col Character string. Track longitude column identifier. Default: `"longitude"`.
#' @param lat_col Character string. Track latitude column identifier. Default: `"latitude"`.
#' @param phase_col Character string. Behavioral classification label output column name. Default: `"phase"`.
#' @param commute_speed_threshold Numeric. Lower bounding velocity limit (m/s) defining commuting activities. Default: `5`.
#' @param forage_speed_threshold Numeric. Upper bounding velocity limit (m/s) defining active foraging bounds. Default: `1`.
#'
#' @return A modified tracking table updated with characterized behavioural phase identifiers.
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
  
  needed <- c(bird_id_col, trip_id_col, datetime_col, lon_col, lat_col)
  missing_cols <- setdiff(needed, names(track))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  
  out <- track %>%
    arrange(.data[[bird_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    mutate(
      next_lon = lead(.data[[lon_col]]),
      next_lat = lead(.data[[lat_col]]),
      next_time = lead(.data[[datetime_col]]),
      step_m = geosphere::distHaversine(
        cbind(.data[[lon_col]], .data[[lat_col]]),
        cbind(next_lon, next_lat)
      ),
      dt_s = as.numeric(difftime(next_time, .data[[datetime_col]], units = "secs")),
      speed_m_s = step_m / dt_s
    ) %>%
    ungroup()
  
  out %>%
    mutate(
      !!phase_col := case_when(
        is.na(.data[[trip_id_col]]) ~ "colony",
        is.na(speed_m_s) ~ "unknown",
        speed_m_s >= commute_speed_threshold ~ "commuting",
        speed_m_s <= forage_speed_threshold ~ "foraging",
        TRUE ~ "mixed"
      )
    ) %>%
    select(-next_lon, -next_lat, -next_time)
}

#' Append Diel Environmental Lifecycle Periods to Telemetry Points
#'
#' Appends solar cycle period assignments (day, night, dawn, dusk) based on coordinate lookups.
#'
#' @param track A data frame or tibble comprising structured telemetry data points.
#' @param datetime_col Character string. Target date-time column context parameter. Default: `"datetime_gmt"`.
#' @param lat_col Character string. Latitude tracker evaluation component. Default: `"latitude"`.
#' @param lon_col Character string. Longitude tracker evaluation component. Default: `"longitude"`.
#' @param timezone Character string. Internal operational solar evaluation zone context. Default: `"UTC"`.
#' @param output_col Character string. Diel category column identifier. Default: `"diel_period"`.
#' @param twilight_buffer_mins Numeric. Offsetting window length spanning surrounding target astronomical calculations. Default: `45`.
#'
#' @return A detailed telemetry frame tracking specific chronological diel cycles.
#' @export
label_day_night_period <- function(track,
                                   datetime_col = "datetime_gmt",
                                   lat_col = "latitude",
                                   lon_col = "longitude",
                                   timezone = "UTC",
                                   output_col = "diel_period",
                                   twilight_buffer_mins = 45) {
  
  needed <- c(datetime_col, lat_col, lon_col)
  missing_cols <- setdiff(needed, names(track))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  
  dates_only <- as.Date(with_tz(track[[datetime_col]], tzone = timezone))
  
  sun_tbl <- suncalc::getSunlightTimes(
    date = unique(dates_only),
    lat = mean(track[[lat_col]], na.rm = TRUE),
    lon = mean(track[[lon_col]], na.rm = TRUE),
    keep = c("sunrise", "sunset"),
    tz = timezone
  ) %>%
    mutate(
      dawn_start = sunrise - minutes(twilight_buffer_mins),
      dawn_end   = sunrise + minutes(twilight_buffer_mins),
      dusk_start = sunset  - minutes(twilight_buffer_mins),
      dusk_end   = sunset  + minutes(twilight_buffer_mins)
    ) %>%
    select(date, dawn_start, dawn_end, dusk_start, dusk_end)
  
  track %>%
    mutate(date_only = as.Date(with_tz(.data[[datetime_col]], tzone = timezone))) %>%
    left_join(sun_tbl, by = c("date_only" = "date")) %>%
    mutate(
      !!output_col := case_when(
        .data[[datetime_col]] >= dawn_start & .data[[datetime_col]] <= dawn_end ~ "dawn",
        .data[[datetime_col]] >= dusk_start & .data[[datetime_col]] <= dusk_end ~ "dusk",
        .data[[datetime_col]] > dawn_end & .data[[datetime_col]] < dusk_start ~ "day",
        TRUE ~ "night"
      )
    ) %>%
    select(-date_only, -dawn_start, -dawn_end, -dusk_start, -dusk_end)
}

#' Generate Trip-Level Aggregate Analysis Profiles
#'
#' Collapses fine-scale tracking points into an itemized registry summarizing spatial footprints 
#' and temporal trip characteristics.
#'
#' @param track A structured tracking data frame containing segment and phase information.
#' @param bird_id_col Character string. Unique individual identifier index column name. Default: `"track_id"`.
#' @param trip_id_col Character string. Trip assignment reference variable. Default: `"trip_id"`.
#' @param datetime_col Character string. Coordinate processing chronological reference column name. Default: `"datetime_gmt"`.
#' @param distance_col Character string. Source spatial calculation track index. Default: `"dist_to_colony_m"`.
#' @param phase_col Character string. Extracted behavioral label tracking target. Default: `"phase"`.
#'
#' @return An aggregated summary table capturing individual flight tracking statistics.
#' @export
summarize_trips <- function(track,
                            bird_id_col = "track_id",
                            trip_id_col = "trip_id",
                            datetime_col = "datetime_gmt",
                            distance_col = "dist_to_colony_m",
                            phase_col = "phase") {
  
  needed <- c(bird_id_col, trip_id_col, datetime_col)
  missing_cols <- setdiff(needed, names(track))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  
  has_distance <- distance_col %in% names(track)
  has_diel     <- "diel_period" %in% names(track)
  has_phase    <- phase_col %in% names(track)
  
  track %>%
    filter(!is.na(.data[[trip_id_col]])) %>%
    arrange(.data[[bird_id_col]], .data[[trip_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    summarise(
      trip_start           = min(.data[[datetime_col]], na.rm = TRUE),
      trip_end             = max(.data[[datetime_col]], na.rm = TRUE),
      duration_h           = as.numeric(difftime(trip_end, trip_start, units = "hours")),
      n_fixes              = n(),
      max_dist_to_colony_m = if (has_distance) max(.data[[distance_col]], na.rm = TRUE) else NA_real_,
      prop_day             = if (has_diel) mean(diel_period == "day",    na.rm = TRUE) else NA_real_,
      prop_night           = if (has_diel) mean(diel_period == "night",  na.rm = TRUE) else NA_real_,
      prop_commuting       = if (has_phase) mean(.data[[phase_col]] == "commuting", na.rm = TRUE) else NA_real_,
      prop_foraging        = if (has_phase) mean(.data[[phase_col]] == "foraging",  na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}