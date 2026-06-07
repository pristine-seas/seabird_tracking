# =============================================================================
# top_level_wrappers.R
# Non-duplicated top-level workflow wrappers for the Shearwater package
# =============================================================================
#
# This file intentionally does NOT define functions that already live in their
# own dedicated files, including:
#   - clean_tracks()                -> clean_tracks.R
#   - summarize_movement()          -> summarize_movement.R
#   - analyze_fisheries_overlap()   -> analyze_fisheries_overlap.R
#   - export_spatial_outputs()      -> export_spatial_outputs.R
#
# Keeping only one definition of each exported function prevents source-order
# overwrites during development and inconsistent behavior during tests.


#' Estimate Space Use
#'
#' Runs the space-use workflow from a raw GPS file through cleaning, kernel
#' utilization distribution estimation, isopleth extraction, area calculation,
#' and trip summaries.
#'
#' @param file_path Character. Path to the raw GPS telemetry dataset.
#' @param col_map Optional named character vector used for column standardization.
#' @param max_speed Numeric. Maximum allowed speed for outlier filtering.
#' @param speed_col Character. Speed column.
#' @param interval_minutes Numeric. Track regularization interval.
#' @param colony_coords Named numeric vector with `lon` and `lat`.
#' @param kud_ref Character or numeric. Kernel bandwidth argument.
#' @param density_levels Numeric vector of isopleth levels.
#'
#' @return A named list of space-use outputs.
#' @export
estimate_space_use <- function(file_path,
                               col_map = NULL,
                               max_speed = 100,
                               speed_col = "Speed",
                               interval_minutes = 30,
                               colony_coords = c(lon = 0, lat = 0),
                               kud_ref = "href",
                               density_levels = c(50, 95)) {
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("`file_path` must be a single non-missing character string.", call. = FALSE)
  }

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path, call. = FALSE)
  }

  if (!is.numeric(max_speed) || length(max_speed) != 1 || is.na(max_speed) || max_speed <= 0) {
    stop("`max_speed` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(interval_minutes) ||
      length(interval_minutes) != 1 ||
      is.na(interval_minutes) ||
      interval_minutes <= 0) {
    stop("`interval_minutes` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(colony_coords) || !all(c("lon", "lat") %in% names(colony_coords))) {
    stop("`colony_coords` must be a named numeric vector with names `lon` and `lat`.", call. = FALSE)
  }

  if (!is.numeric(density_levels) || length(density_levels) < 1) {
    stop("`density_levels` must be a numeric vector.", call. = FALSE)
  }

  if (any(is.na(density_levels)) || any(density_levels <= 0 | density_levels >= 100)) {
    stop("`density_levels` must contain values greater than 0 and less than 100.", call. = FALSE)
  }

  raw_data <- read_gps_data(
    file_path = file_path,
    format = "csv"
  )

  clean_data <- clean_tracks(
    data = raw_data,
    col_map = col_map,
    max_speed = max_speed,
    speed_col = speed_col,
    regularize = TRUE,
    interval_minutes = interval_minutes
  )

  needed_regular_cols <- c("datetime_regular", "lat", "lon")
  missing_regular_cols <- setdiff(needed_regular_cols, names(clean_data))

  if (length(missing_regular_cols) > 0) {
    stop(
      "After cleaning, data is missing required columns: ",
      paste(missing_regular_cols, collapse = ", "),
      call. = FALSE
    )
  }

  id_col <- if ("ID" %in% names(clean_data)) {
    "ID"
  } else if ("bird_id" %in% names(clean_data)) {
    "bird_id"
  } else if ("id" %in% names(clean_data)) {
    "id"
  } else if ("track_id" %in% names(clean_data)) {
    "track_id"
  } else {
    stop("After cleaning, data is missing a bird ID column.", call. = FALSE)
  }

  regular_data <- clean_data |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lon)) |>
    dplyr::rename(
      track_id = dplyr::all_of(id_col),
      datetime_gmt = .data$datetime_regular,
      latitude = .data$lat,
      longitude = .data$lon
    )

  if (nrow(regular_data) == 0) {
    stop("After cleaning, no rows with non-missing coordinates remain.", call. = FALSE)
  }

  tracks_sf <- sf::st_as_sf(
    regular_data,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  validate_gps_data(tracks_sf, strict = FALSE)

  if (!"trip_id" %in% names(tracks_sf)) {
    tracks_sf$trip_id <- paste0(tracks_sf$track_id, "_trip1")
  }

  trip_dist <- calc_trip_distance(
    trip_data = sf::st_drop_geometry(tracks_sf),
    bird_id_col = "track_id",
    trip_id_col = "trip_id",
    datetime_col = "datetime_gmt",
    lon_col = "longitude",
    lat_col = "latitude"
  )

  tracks_sf <- tracks_sf |>
    dplyr::mutate(time = .data$datetime_gmt)

  kud_output <- calculate_kud(
    tracks = tracks_sf,
    ref = kud_ref
  )

  isopleth_polygons <- get_isopleths(
    kud = kud_output,
    levels = density_levels
  )

  isopleth_polygons <- calculate_area_metrics(
    sf_polys = isopleth_polygons
  )

  tracks_with_stats <- calculate_trip_stats(
    tracks = tracks_sf,
    colony_coords = colony_coords
  )

  final_trip_summaries <- tracks_with_stats |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$track_id, .data$trip_id) |>
    dplyr::summarise(
      max_distance_colony_km = max(.data$max_dist_km, na.rm = TRUE),
      duration_hours = max(.data$duration_hrs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(trip_dist, by = c("track_id", "trip_id"))

  list(
    cleaned_tracks = tracks_with_stats,
    kud_estimates = kud_output,
    isopleth_polygons = isopleth_polygons,
    trip_summaries = final_trip_summaries
  )
}


#' Analyze Jurisdiction and Conservation Overlap
#'
#' Runs jurisdiction, MPA, priority-area, transboundary, and policy exposure
#' analyses for seabird tracks.
#'
#' @param track_data An `sf` track object.
#' @param eez_layer An `sf` EEZ layer.
#' @param mpa_layer Optional `sf` MPA layer.
#' @param priority_layer Optional `sf` conservation priority layer.
#'
#' @return A named list of jurisdiction and conservation outputs.
#' @export
analyze_jurisdiction_overlap <- function(track_data,
                                         eez_layer,
                                         mpa_layer = NULL,
                                         priority_layer = NULL) {
  if (!inherits(track_data, "sf")) {
    stop("`track_data` must be an sf object.", call. = FALSE)
  }

  if (!inherits(eez_layer, "sf")) {
    stop("`eez_layer` must be an sf object.", call. = FALSE)
  }

  if (!is.null(mpa_layer) && !inherits(mpa_layer, "sf")) {
    stop("`mpa_layer` must be an sf object or NULL.", call. = FALSE)
  }

  if (!is.null(priority_layer) && !inherits(priority_layer, "sf")) {
    stop("`priority_layer` must be an sf object or NULL.", call. = FALSE)
  }

  eez_tracks <- overlay_eez_abnj(
    track_data = track_data,
    eez_layer = eez_layer
  )

  jurisdiction_summary <- calc_time_in_jurisdictions(
    track_data = eez_tracks
  )

  transboundary_summary <- calc_transboundary_movements(
    track_data = eez_tracks
  )

  mpa_tracks <- NULL
  priority_tracks <- NULL
  policy_summary <- NULL

  if (!is.null(mpa_layer)) {
    mpa_tracks <- overlay_mpas(
      track_data = eez_tracks,
      mpa_layer = mpa_layer
    )
  }

  if (!is.null(priority_layer)) {
    priority_tracks <- overlay_priority_areas(
      track_data = eez_tracks,
      priority_layer = priority_layer
    )
  }

  if (!is.null(mpa_tracks) && !is.null(priority_tracks)) {
    policy_summary <- summarize_policy_exposure(
      jurisdiction_summary = jurisdiction_summary,
      mpa_track = mpa_tracks,
      priority_track = priority_tracks,
      transboundary_summary = transboundary_summary
    )
  }

  list(
    labeled_tracks = eez_tracks,
    jurisdiction_summary = jurisdiction_summary,
    mpa_tracks = mpa_tracks,
    priority_tracks = priority_tracks,
    transboundary_summary = transboundary_summary,
    policy_summary = policy_summary
  )
}


#' Plot Tracking Results
#'
#' Creates a set of common plots from track, trip, fisheries, and space-use
#' outputs.
#'
#' @param track_data Optional track data frame.
#' @param trip_data Optional trip-segmented data frame.
#' @param fisheries_grid Optional `sf` grid or polygon layer with overlap/risk values.
#' @param isopleth_polygons Optional `sf` home-range or core-use polygons.
#' @param colony_coords Optional named numeric vector with `lon` and `lat`.
#'
#' @return A named list of `ggplot2` objects.
#' @export
plot_tracking_results <- function(track_data = NULL,
                                  trip_data = NULL,
                                  fisheries_grid = NULL,
                                  isopleth_polygons = NULL,
                                  colony_coords = NULL) {
  if (!is.null(colony_coords) &&
      (!is.numeric(colony_coords) || !all(c("lon", "lat") %in% names(colony_coords)))) {
    stop("`colony_coords` must be a named numeric vector with names `lon` and `lat`.", call. = FALSE)
  }

  plots <- list()

  if (!is.null(track_data)) {
    plots$tracks <- plot_tracks(
      track_data = track_data,
      colony_coords = colony_coords
    )

    plots$density <- plot_density_map(
      track_data = track_data
    )
  }

  if (!is.null(trip_data)) {
    plots$trips <- plot_trip_map(
      trip_data = trip_data,
      colony_coords = colony_coords
    )
  }

  if (!is.null(fisheries_grid)) {
    plots$fisheries_heatmap <- plot_fisheries_heatmap(
      grid_data = fisheries_grid
    )
  }

  if (!is.null(isopleth_polygons)) {
    plots$hotspots <- plot_hotspot_map(
      sf_polys = isopleth_polygons
    )
  }

  plots
}


#' Export GIS Layers
#'
#' Writes a spatial object to disk.
#'
#' @param layer A spatial object, usually an `sf` object.
#' @param file_path Character output path.
#' @param ... Additional arguments passed to spatial writers.
#'
#' @return Invisibly returns `file_path`.
#' @export
export_gis_layers <- function(layer, file_path, ...) {
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("`file_path` must be a single non-missing character string.", call. = FALSE)
  }

  if (inherits(layer, "sf")) {
    sf::st_write(
      layer,
      dsn = file_path,
      quiet = TRUE,
      delete_dsn = TRUE,
      ...
    )

    return(invisible(file_path))
  }

  stop("`layer` must be a recognised spatial object.", call. = FALSE)
}


#' Export Utilization Distribution Polygons
#'
#' Writes utilization distribution polygons to disk.
#'
#' @param ud_polys An `sf` object, or a list containing UD polygon objects.
#' @param file_path Character output path.
#' @param ... Additional arguments passed to spatial writers.
#'
#' @return Invisibly returns `file_path`.
#' @export
export_ud_polygons <- function(ud_polys, file_path, ...) {
  if (inherits(ud_polys, "sf")) {
    export_gis_layers(
      layer = ud_polys,
      file_path = file_path,
      ...
    )

    return(invisible(file_path))
  }

  if (is.list(ud_polys)) {
    sf_items <- ud_polys[vapply(ud_polys, inherits, logical(1), what = "sf")]

    if (length(sf_items) > 0) {
      combined <- do.call(rbind, sf_items)

      export_gis_layers(
        layer = combined,
        file_path = file_path,
        ...
      )

      return(invisible(file_path))
    }
  }

  stop("`ud_polys` must be an sf object or a list containing sf objects.", call. = FALSE)
}
