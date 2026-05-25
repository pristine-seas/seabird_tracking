#' Summarize Movement Metrics
#'
#' Runs the main movement-analysis workflow on GPS tracking data. This wrapper
#' can optionally segment trips, classify trip phase, summarize trips, calculate
#' trip-level movement metrics, and aggregate those metrics to individual and
#' population-level summaries.
#'
#' @param track A data frame containing GPS tracking data.
#' @param colony_coords Named numeric vector with `lon` and `lat`, for example
#'   `c(lon = -158.276489, lat = 21.573956)`.
#' @param already_segmented Logical. If `TRUE`, assumes `track` already contains
#'   a trip ID column.
#' @param classify_phases Logical. If `TRUE`, runs `classify_trip_phase()`.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Character. Column identifying trips.
#' @param datetime_col Character. Datetime column.
#' @param colony_flag_col Character. Logical column indicating whether each point
#'   is at the colony.
#' @param lon_col Character. Longitude column.
#' @param lat_col Character. Latitude column.
#' @param phase_col Character. Output column for trip phase.
#' @param duration_units Character. Units for trip duration. One of `"secs"`,
#'   `"mins"`, `"hours"`, or `"days"`.
#' @param include_spatial Logical. If `TRUE`, also calculates track centroids and
#'   foraging ranges.
#' @param foraging_value Character. Phase value used to identify foraging points.
#'
#' @return A named list containing cleaned/processed tracks, trip summaries,
#'   trip metrics, individual metrics, population metrics, and optionally spatial
#'   summaries.
#'
#' @export
summarize_movement <- function(track,
                               colony_coords,
                               already_segmented = FALSE,
                               classify_phases = TRUE,
                               bird_id_col = "track_id",
                               trip_id_col = "trip_id",
                               datetime_col = "datetime_gmt",
                               colony_flag_col = "at_colony",
                               lon_col = "longitude",
                               lat_col = "latitude",
                               phase_col = "phase",
                               duration_units = "hours",
                               include_spatial = TRUE,
                               foraging_value = "foraging") {

  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.numeric(colony_coords) ||
      !all(c("lon", "lat") %in% names(colony_coords))) {
    stop(
      "`colony_coords` must be a named numeric vector with names `lon` and `lat`.",
      call. = FALSE
    )
  }

  needed <- c(bird_id_col, datetime_col, lon_col, lat_col)

  if (!already_segmented) {
    needed <- c(needed, colony_flag_col)
  } else {
    needed <- c(needed, trip_id_col)
  }

  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Make sure timestamp column is valid and timezone-aware.
  assert_datetime_tz(track, datetime_col)

  # 1. Segment trips if needed.
  if (!already_segmented) {
    processed_track <- segment_trips(
      track = track,
      bird_id_col = bird_id_col,
      datetime_col = datetime_col,
      colony_flag_col = colony_flag_col,
      trip_id_col = trip_id_col
    )
  } else {
    processed_track <- track
  }

  # 2. Optionally classify behavior/phase.
  if (classify_phases) {
    processed_track <- classify_trip_phase(
      track = processed_track,
      bird_id_col = bird_id_col,
      trip_id_col = trip_id_col,
      datetime_col = datetime_col,
      lon_col = lon_col,
      lat_col = lat_col,
      phase_col = phase_col
    )
  }

  # 3. Create trip summaries from Person 4 function.
  trip_summary <- summarize_trips(
    track = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    distance_col = "dist_to_colony_m",
    phase_col = phase_col
  )

  # 4. Calculate Person 5 movement metrics.
  trip_distance <- calc_trip_distance(
    trip_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    lon_col = lon_col,
    lat_col = lat_col,
    output_col = "trip_distance_m"
  )

  trip_duration <- calc_trip_duration(
    trip_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    units = duration_units,
    output_col = "trip_duration"
  )

  path_length <- calc_path_length(
    track_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    lon_col = lon_col,
    lat_col = lat_col,
    output_col = "path_length_m"
  )

  max_distance <- calc_max_distance_from_colony(
    trip_data = processed_track,
    colony_coords = colony_coords,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    lon_col = lon_col,
    lat_col = lat_col,
    output_col = "max_distance_from_colony_m"
  )

  join_cols <- c(bird_id_col, trip_id_col)

  trip_metrics <- trip_distance %>%
    dplyr::left_join(trip_duration, by = join_cols) %>%
    dplyr::left_join(path_length, by = join_cols) %>%
    dplyr::left_join(max_distance, by = join_cols)

  # 5. Aggregate to individual and population summaries.
  individual_metrics <- summarize_individual_metrics(
    trip_metrics = trip_metrics,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    distance_col = "trip_distance_m",
    duration_col = "trip_duration",
    path_length_col = "path_length_m",
    max_distance_col = "max_distance_from_colony_m"
  )

  population_metrics <- summarize_population_metrics(
    individual_metrics = individual_metrics,
    bird_id_col = bird_id_col
  )

  # 6. Optional spatial summaries.
  centroids <- NULL
  foraging_ranges <- NULL

  if (include_spatial) {
    centroids <- calc_track_centroid(
      track_data = processed_track,
      bird_id_col = bird_id_col,
      trip_id_col = NULL,
      lon_col = lon_col,
      lat_col = lat_col,
      crs = 4326
    )

    if (classify_phases && phase_col %in% names(processed_track)) {
      foraging_ranges <- calc_foraging_range(
        track_data = processed_track,
        bird_id_col = bird_id_col,
        lon_col = lon_col,
        lat_col = lat_col,
        phase_col = phase_col,
        foraging_value = foraging_value,
        method = "convex_hull",
        crs = 4326
      )
    }
  }

  list(
    processed_track = processed_track,
    trip_summary = trip_summary,
    trip_metrics = trip_metrics,
    individual_metrics = individual_metrics,
    population_metrics = population_metrics,
    centroids = centroids,
    foraging_ranges = foraging_ranges
  )
}
