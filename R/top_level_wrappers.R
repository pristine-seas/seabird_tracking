# =============================================================================
# top_level_wrappers.R
# Top-level workflow wrappers for the Shearwater package
# =============================================================================


#' Clean GPS Tracking Data
#'
#' Runs the GPS import, standardization, duplicate filtering, speed filtering,
#' and optional regularization workflow.
#'
#' @param file_path Character. Path to the raw GPS data file.
#' @param col_map Optional named character vector for column standardization.
#' @param format Character. Input file format. Default is `"csv"`.
#' @param max_speed Numeric. Maximum allowed speed for filtering outliers.
#' @param speed_col Character. Name of the speed column.
#' @param regularize Logical. If `TRUE`, regularize tracks to a fixed interval.
#' @param interval_minutes Numeric. Regularization interval in minutes.
#'
#' @return A cleaned GPS data frame.
#' @export
clean_tracks <- function(file_path,
                         col_map = NULL,
                         format = "csv",
                         max_speed = 100,
                         speed_col = "Speed",
                         regularize = TRUE,
                         interval_minutes = 30) {
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("`file_path` must be a single non-missing character string.", call. = FALSE)
  }

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path, call. = FALSE)
  }

  raw_data <- read_gps_data(
    file_path = file_path,
    format = format
  )

  std_data <- standardize_gps_columns(
    raw_data = raw_data,
    col_map = col_map
  )

  if (all(c("timestamp", "bird_id", "lat", "lon") %in% names(std_data))) {
    std_data <- std_data |>
      dplyr::mutate(
        Date = format(.data$timestamp, "%m/%d/%Y"),
        Time = format(.data$timestamp, "%H:%M:%S")
      ) |>
      dplyr::rename(
        ID = .data$bird_id,
        Latitude = .data$lat,
        Longitude = .data$lon
      )
  }

  clean_data <- remove_duplicate_fixes(
    df = std_data,
    id_col = "ID",
    datetime_col = NULL,
    date_col = "Date",
    time_col = "Time"
  )

  if (speed_col %in% names(clean_data)) {
    clean_data <- filter_speed_outliers(
      df = clean_data,
      max_speed = max_speed,
      speed_col = speed_col,
      method = "remove"
    )
  }

  if (regularize) {
    clean_data <- regularize_tracks(
      df = clean_data,
      id_col = "ID",
      date_col = "Date",
      time_col = "Time",
      lat_col = "Latitude",
      lon_col = "Longitude",
      interval_minutes = interval_minutes
    )
  }

  clean_data
}


#' Summarize Movement Metrics
#'
#' Runs trip segmentation, optional phase classification, trip-level movement
#' metrics, and individual/population movement summaries.
#'
#' @param track A data frame containing GPS tracking data.
#' @param colony_coords Named numeric vector with `lon` and `lat`.
#' @param already_segmented Logical. If `TRUE`, assumes `track` already contains
#'   a trip ID column.
#' @param classify_phases Logical. If `TRUE`, classifies trip phases.
#' @param bird_id_col Character. Bird or track ID column.
#' @param trip_id_col Character. Trip ID column.
#' @param datetime_col Character. Datetime column.
#' @param colony_flag_col Character. Colony flag column.
#' @param lon_col Character. Longitude column.
#' @param lat_col Character. Latitude column.
#' @param phase_col Character. Phase output column.
#' @param duration_units Character. Duration units.
#' @param include_spatial Logical. If `TRUE`, calculate centroids and foraging ranges.
#'
#' @return A named list of movement outputs.
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
                               include_spatial = TRUE) {
  if (!is.data.frame(track)) {
    stop("`track` must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.numeric(colony_coords) ||
      !all(c("lon", "lat") %in% names(colony_coords))) {
    stop("`colony_coords` must be a named numeric vector with names `lon` and `lat`.",
         call. = FALSE)
  }

  needed <- c(bird_id_col, datetime_col, lon_col, lat_col)

  if (already_segmented) {
    needed <- c(needed, trip_id_col)
  } else {
    needed <- c(needed, colony_flag_col)
  }

  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop("Missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

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

  trip_summary <- summarize_trips(
    track = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    distance_col = "dist_to_colony_m",
    phase_col = phase_col
  )

  trip_distance <- calc_trip_distance(
    trip_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    lon_col = lon_col,
    lat_col = lat_col
  )

  trip_duration <- calc_trip_duration(
    trip_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    units = duration_units
  )

  path_length <- calc_path_length(
    track_data = processed_track,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    datetime_col = datetime_col,
    lon_col = lon_col,
    lat_col = lat_col
  )

  max_distance <- calc_max_distance_from_colony(
    trip_data = processed_track,
    colony_coords = colony_coords,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col,
    lon_col = lon_col,
    lat_col = lat_col
  )

  join_cols <- c(bird_id_col, trip_id_col)

  trip_metrics <- trip_distance |>
    dplyr::left_join(trip_duration, by = join_cols) |>
    dplyr::left_join(path_length, by = join_cols) |>
    dplyr::left_join(max_distance, by = join_cols)

  individual_metrics <- summarize_individual_metrics(
    trip_metrics = trip_metrics,
    bird_id_col = bird_id_col,
    trip_id_col = trip_id_col
  )

  population_metrics <- summarize_population_metrics(
    individual_metrics = individual_metrics,
    bird_id_col = bird_id_col
  )

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
        foraging_value = "foraging",
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


#' Estimate Space Use
#'
#' Runs the space-use workflow from cleaned GPS data through kernel utilization
#' distribution estimation, isopleth extraction, area calculation, and trip summaries.
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
  clean_data <- clean_tracks(
    file_path = file_path,
    col_map = col_map,
    max_speed = max_speed,
    speed_col = speed_col,
    regularize = TRUE,
    interval_minutes = interval_minutes
  )

  needed_regular_cols <- c("id", "datetime_regular", "Latitude", "Longitude")
  missing_regular_cols <- setdiff(needed_regular_cols, names(clean_data))

  if (length(missing_regular_cols) > 0) {
    stop(
      "After cleaning, data is missing required columns: ",
      paste(missing_regular_cols, collapse = ", "),
      call. = FALSE
    )
  }

  regular_data <- clean_data |>
    dplyr::filter(!is.na(.data$Latitude), !is.na(.data$Longitude)) |>
    dplyr::rename(
      track_id = .data$id,
      datetime_gmt = .data$datetime_regular,
      latitude = .data$Latitude,
      longitude = .data$Longitude
    )

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


#' Analyze Fisheries Overlap
#'
#' Runs fisheries effort standardization, spatial joining, overlap summaries,
#' optional gear-risk scoring, and optional diel overlap.
#'
#' @param track_data A data frame or `sf` object of seabird track points.
#' @param fisheries_data A data frame or `sf` object of fisheries effort data.
#' @param gear_weights Optional named numeric vector or data frame of gear weights.
#' @param track_id_col Character. Track ID column.
#' @param track_lon_col Character. Track longitude column if not `sf`.
#' @param track_lat_col Character. Track latitude column if not `sf`.
#' @param fisheries_lon_col Character. Fisheries longitude column if not `sf`.
#' @param fisheries_lat_col Character. Fisheries latitude column if not `sf`.
#' @param effort_col Character. Fisheries effort column.
#' @param gear_col Character. Gear column.
#' @param cell_id_col Optional character. Fisheries grid/cell ID column.
#' @param join_type Character. Spatial join type.
#' @param crs CRS for non-sf inputs.
#' @param calculate_risk Logical. If `TRUE`, calculate gear-weighted risk.
#' @param calculate_diel Logical. If `TRUE`, calculate diel overlap.
#'
#' @return A named list of fisheries overlap outputs.
#' @export
analyze_fisheries_overlap <- function(track_data,
                                      fisheries_data,
                                      gear_weights = NULL,
                                      track_id_col = "track_id",
                                      track_lon_col = "longitude",
                                      track_lat_col = "latitude",
                                      fisheries_lon_col = "longitude",
                                      fisheries_lat_col = "latitude",
                                      effort_col = "effort",
                                      gear_col = "gear",
                                      cell_id_col = NULL,
                                      join_type = c("intersects", "within", "nearest"),
                                      crs = 4326,
                                      calculate_risk = TRUE,
                                      calculate_diel = FALSE) {
  join_type <- match.arg(join_type)

  if (calculate_risk && is.null(gear_weights)) {
    stop("`gear_weights` must be provided when `calculate_risk = TRUE`.",
         call. = FALSE)
  }

  fisheries_clean <- standardize_fishing_effort(
    fisheries_data = fisheries_data,
    effort_col = effort_col,
    gear_col = gear_col,
    standardize_effort = TRUE,
    log_transform = FALSE
  )

  track_sf <- if (inherits(track_data, "sf")) {
    track_data
  } else {
    sf::st_as_sf(
      track_data,
      coords = c(track_lon_col, track_lat_col),
      crs = crs,
      remove = FALSE
    )
  }

  fisheries_sf <- if (inherits(fisheries_clean, "sf")) {
    fisheries_clean
  } else {
    as_fisheries_sf(
      data = fisheries_clean,
      lon_col = fisheries_lon_col,
      lat_col = fisheries_lat_col,
      crs = crs
    )
  }

  if (sf::st_crs(track_sf) != sf::st_crs(fisheries_sf)) {
    fisheries_sf <- sf::st_transform(fisheries_sf, sf::st_crs(track_sf))
  }

  joined_overlap <- join_tracks_to_fishing_grid(
    track_data = track_sf,
    fisheries_data = fisheries_sf,
    join_type = join_type
  )

  overlap_metrics <- calc_fisheries_overlap(
    joined_data = joined_overlap,
    track_id_col = track_id_col,
    effort_col = "effort_std",
    gear_col = gear_col,
    cell_id_col = cell_id_col
  )

  gear_summary <- summarize_overlap_by_gear(
    overlap_data = overlap_metrics,
    gear_col = gear_col,
    overlap_col = "total_overlap"
  )

  risk_results <- NULL

  if (calculate_risk) {
    risk_results <- calc_risk_index(
      overlap_data = overlap_metrics,
      overlap_col = "total_overlap",
      gear_col = gear_col,
      gear_weights = gear_weights,
      scale_01 = TRUE
    )
  }

  diel_overlap <- NULL

  if (calculate_diel) {
    diel_overlap <- calc_diel_overlap(
      joined_data = joined_overlap,
      track_id_col = track_id_col
    )
  }

  list(
    fisheries_clean = fisheries_clean,
    track_sf = track_sf,
    fisheries_sf = fisheries_sf,
    joined_overlap = joined_overlap,
    overlap_metrics = overlap_metrics,
    gear_summary = gear_summary,
    risk_results = risk_results,
    diel_overlap = diel_overlap
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
#' Creates a set of common plots from track, trip, fisheries, and space-use outputs.
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


#' Export Spatial Outputs
#'
#' Exports spatial layers and policy tables from a named results list.
#'
#' @param results Named list containing spatial and tabular outputs.
#' @param out_dir Character. Output directory.
#' @param gis_format Character. GIS output format.
#' @param table_format Character. Table output format.
#' @param overwrite Logical. Overwrite existing files.
#' @param verbose Logical. Print progress messages.
#'
#' @return Invisibly returns written file paths.
#' @export
export_spatial_outputs <- function(results,
                                   out_dir = "outputs",
                                   gis_format = c("gpkg", "shp", "geojson"),
                                   table_format = c("csv", "xlsx"),
                                   overwrite = FALSE,
                                   verbose = TRUE) {
  gis_format <- match.arg(gis_format)
  table_format <- match.arg(table_format)

  .msg <- function(...) {
    if (verbose) {
      message(...)
    }
  }

  if (!is.list(results)) {
    stop("`results` must be a named list.", call. = FALSE)
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  written <- character(0)

  spatial_keys <- c(
    "tracks",
    "cleaned_tracks",
    "core_area",
    "home_range",
    "ud_polygons",
    "isopleth_polygons",
    "overlap_grid",
    "foraging_range"
  )

  for (key in intersect(spatial_keys, names(results))) {
    layer <- results[[key]]

    if (is.null(layer) || !inherits(layer, c("sf", "sfc", "RasterLayer", "SpatRaster"))) {
      next
    }

    out_path <- file.path(out_dir, paste0(key, ".", gis_format))

    if (file.exists(out_path) && !overwrite) {
      warning("File already exists and `overwrite = FALSE`; skipping: ", out_path,
              call. = FALSE)
      next
    }

    .msg("Writing spatial layer: ", out_path)

    export_gis_layers(
      spatial_object = layer,
      file_path = out_path
    )

    written <- c(written, stats::setNames(out_path, key))
  }

  table_keys <- c(
    "trip_summaries",
    "individual_metrics",
    "population_metrics",
    "jurisdiction_summary",
    "policy_summary",
    "gear_summary",
    "overlap_metrics"
  )

  for (key in intersect(table_keys, names(results))) {
    tbl <- results[[key]]

    if (is.null(tbl) || !is.data.frame(tbl)) {
      next
    }

    out_path <- file.path(out_dir, paste0(key, ".", table_format))

    if (file.exists(out_path) && !overwrite) {
      warning("File already exists and `overwrite = FALSE`; skipping: ", out_path,
              call. = FALSE)
      next
    }

    .msg("Writing table: ", out_path)

    export_policy_summary_tables(
      summary_data = tbl,
      file_path = out_path,
      overwrite = overwrite
    )

    written <- c(written, stats::setNames(out_path, key))
  }

  invisible(written)
}
