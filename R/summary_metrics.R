############################################################
# calc_track_centroid()
# Compute spatial centroid of a track or set of points.
############################################################

#' Calculate Track Centroid
#'
#' Calculates the geographic centroid of each bird's track, or each bird-trip
#' if a trip ID column is provided.
#'
#' @param track_data A data frame or sf object containing track points.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Optional character. Column identifying trips. If `NULL`,
#'   centroids are calculated by bird only.
#' @param lon_col Character. Longitude column. Used only if `track_data` is not sf.
#' @param lat_col Character. Latitude column. Used only if `track_data` is not sf.
#' @param crs Coordinate reference system for non-sf data. Default is 4326.
#'
#' @return An sf object with centroid geometry for each group.
#' @export
calc_track_centroid <- function(track_data,
                                bird_id_col = "track_id",
                                trip_id_col = NULL,
                                lon_col = "longitude",
                                lat_col = "latitude",
                                crs = 4326) {

  needed <- bird_id_col

  if (!is.null(trip_id_col)) {
    needed <- c(needed, trip_id_col)
  }

  if (!inherits(track_data, "sf")) {
    needed <- c(needed, lon_col, lat_col)
  }

  missing_cols <- setdiff(needed, names(track_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for calc_track_centroid().")
  }

  if (!inherits(track_data, "sf")) {
    track_sf <- sf::st_as_sf(
      track_data,
      coords = c(lon_col, lat_col),
      crs = crs,
      remove = FALSE
    )
  } else {
    track_sf <- track_data
  }

  assert_crs(track_sf)

  group_cols <- bird_id_col

  if (!is.null(trip_id_col)) {
    group_cols <- c(group_cols, trip_id_col)
  }

  track_sf %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    {
      if (!is.null(trip_id_col)) {
        dplyr::filter(., !is.na(.data[[trip_id_col]]))
      } else {
        .
      }
    } %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      geometry = sf::st_centroid(sf::st_union(geometry)),
      .groups = "drop"
    )
}

############################################################
# 6. calc_foraging_range()
# Estimate spatial extent of individual or group foraging area.
############################################################

#' Calculate Foraging Range
#'
#' Estimates the spatial extent of a bird's foraging range using either
#' a convex hull or bounding box around track points.
#'
#' @param track_data A data frame or sf object containing track points.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param lon_col Character. Longitude column. Used only if `track_data` is not sf.
#' @param lat_col Character. Latitude column. Used only if `track_data` is not sf.
#' @param phase_col Optional character. If supplied, data can be filtered to foraging points.
#' @param foraging_value Character. Value in `phase_col` that identifies foraging points.
#' @param method Character. Either `"convex_hull"` or `"bbox"`.
#' @param crs Coordinate reference system for non-sf data. Default is 4326.
#' @param area_crs Projected CRS used for area calculation. Default is 3857.
#'
#' @return An sf object with foraging range geometry and area in square kilometers.
#' @export
calc_foraging_range <- function(track_data,
                                bird_id_col = "track_id",
                                lon_col = "longitude",
                                lat_col = "latitude",
                                phase_col = NULL,
                                foraging_value = "foraging",
                                method = c("convex_hull", "bbox"),
                                crs = 4326,
                                area_crs = 3857) {

  method <- match.arg(method)

  needed <- bird_id_col

  if (!inherits(track_data, "sf")) {
    needed <- c(needed, lon_col, lat_col)
  }

  if (!is.null(phase_col)) {
    needed <- c(needed, phase_col)
  }

  missing_cols <- setdiff(needed, names(track_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for calc_foraging_range().")
  }

  if (!inherits(track_data, "sf")) {
    track_sf <- sf::st_as_sf(
      track_data,
      coords = c(lon_col, lat_col),
      crs = crs,
      remove = FALSE
    )
  } else {
    track_sf <- track_data
  }

  assert_crs(track_sf)

  if (!is.null(phase_col)) {
    track_sf <- track_sf %>%
      dplyr::filter(.data[[phase_col]] == foraging_value)
  }

  range_sf <- track_sf %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    dplyr::group_by(.data[[bird_id_col]]) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      geometry = {
        geom_union <- sf::st_union(geometry)

        if (method == "convex_hull") {
          sf::st_convex_hull(geom_union)
        } else {
          sf::st_as_sfc(sf::st_bbox(geom_union))
        }
      },
      .groups = "drop"
    )

  range_sf %>%
    sf::st_transform(area_crs) %>%
    dplyr::mutate(
      foraging_range_km2 = as.numeric(sf::st_area(geometry)) / 1e6
    ) %>%
    sf::st_transform(sf::st_crs(track_sf))
}

############################################################
# 7. summarize_individual_metrics()
# Aggregate trip and movement metrics to one row per bird.
############################################################

#' Summarize Individual Movement Metrics
#'
#' Aggregates trip-level and movement-level metrics to one row per individual bird.
#'
#' @param trip_metrics A data frame containing trip-level movement metrics.
#' @param bird_id_col Character. Column identifying each bird or track.
#' @param trip_id_col Character. Column identifying each trip.
#' @param distance_col Optional character. Trip distance column.
#' @param duration_col Optional character. Trip duration column.
#' @param path_length_col Optional character. Path length column.
#' @param max_distance_col Optional character. Maximum distance from colony column.
#'
#' @return A data frame with one row per bird and summary metrics.
#' @export
summarize_individual_metrics <- function(trip_metrics,
                                         bird_id_col = "track_id",
                                         trip_id_col = "trip_id",
                                         distance_col = "trip_distance_m",
                                         duration_col = "trip_duration",
                                         path_length_col = "path_length_m",
                                         max_distance_col = "max_distance_from_colony_m") {

  needed <- c(bird_id_col, trip_id_col)
  optional_cols <- c(distance_col, duration_col, path_length_col, max_distance_col)
  existing_optional <- optional_cols[optional_cols %in% names(trip_metrics)]

  missing_cols <- setdiff(needed, names(trip_metrics))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  if (length(existing_optional) == 0) {
    stop("No metric columns found to summarize.")
  }

  out <- trip_metrics %>%
    dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
    dplyr::group_by(.data[[bird_id_col]]) %>%
    dplyr::summarise(
      n_trips = dplyr::n_distinct(.data[[trip_id_col]]),
      .groups = "drop"
    )

  if (distance_col %in% names(trip_metrics)) {
    distance_summary <- trip_metrics %>%
      dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
      dplyr::group_by(.data[[bird_id_col]]) %>%
      dplyr::summarise(
        mean_trip_distance_m = mean(.data[[distance_col]], na.rm = TRUE),
        max_trip_distance_m  = max(.data[[distance_col]], na.rm = TRUE),
        total_trip_distance_m = sum(.data[[distance_col]], na.rm = TRUE),
        .groups = "drop"
      )

    out <- dplyr::left_join(out, distance_summary, by = bird_id_col)
  }

  if (duration_col %in% names(trip_metrics)) {
    duration_summary <- trip_metrics %>%
      dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
      dplyr::group_by(.data[[bird_id_col]]) %>%
      dplyr::summarise(
        mean_trip_duration = mean(.data[[duration_col]], na.rm = TRUE),
        max_trip_duration  = max(.data[[duration_col]], na.rm = TRUE),
        total_trip_duration = sum(.data[[duration_col]], na.rm = TRUE),
        .groups = "drop"
      )

    out <- dplyr::left_join(out, duration_summary, by = bird_id_col)
  }

  if (path_length_col %in% names(trip_metrics)) {
    path_summary <- trip_metrics %>%
      dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
      dplyr::group_by(.data[[bird_id_col]]) %>%
      dplyr::summarise(
        mean_path_length_m = mean(.data[[path_length_col]], na.rm = TRUE),
        max_path_length_m  = max(.data[[path_length_col]], na.rm = TRUE),
        total_path_length_m = sum(.data[[path_length_col]], na.rm = TRUE),
        .groups = "drop"
      )

    out <- dplyr::left_join(out, path_summary, by = bird_id_col)
  }

  if (max_distance_col %in% names(trip_metrics)) {
    max_dist_summary <- trip_metrics %>%
      dplyr::filter(!is.na(.data[[trip_id_col]])) %>%
      dplyr::group_by(.data[[bird_id_col]]) %>%
      dplyr::summarise(
        mean_max_distance_from_colony_m = mean(.data[[max_distance_col]], na.rm = TRUE),
        max_distance_from_colony_m = max(.data[[max_distance_col]], na.rm = TRUE),
        .groups = "drop"
      )

    out <- dplyr::left_join(out, max_dist_summary, by = bird_id_col)
  }

  out
}

############################################################
# 8. summarize_population_metrics()
# Aggregate individual summaries into population-level summaries.
############################################################

#' Summarize Population Movement Metrics
#'
#' Aggregates individual movement summaries into population-level summaries.
#'
#' @param individual_metrics A data frame containing one row per individual bird.
#' @param bird_id_col Character. Column identifying each bird or track.
#'
#' @return A one-row data frame containing population-level movement summaries.
#' @export
summarize_population_metrics <- function(individual_metrics,
                                         bird_id_col = "track_id") {

  if (!bird_id_col %in% names(individual_metrics)) {
    stop("Missing:", bird_id_col)
  }

  numeric_cols <- names(individual_metrics)[sapply(individual_metrics, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, bird_id_col)

  if (length(numeric_cols) == 0) {
    stop("No numeric metric columns found to summarize.")
  }

  base_summary <- individual_metrics %>%
    dplyr::summarise(
      n_individuals = dplyr::n_distinct(.data[[bird_id_col]])
    )

  metric_summary <- individual_metrics %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ stats::sd(.x, na.rm = TRUE),
          min  = ~ min(.x, na.rm = TRUE),
          max  = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  dplyr::bind_cols(base_summary, metric_summary)
}
