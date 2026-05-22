#' Top-Level Space-Use Analysis Wrapper
#'
#' Orchestrates the entire tracking pipeline from raw data import through spatial
#' home range estimation using functions provided by Persons 1, 2, 3, 4, and 6.
#'
#' @param file_path Character. Path to the raw telemetry dataset.
#' @param col_map Named character vector. Naming lookups for column standardization.
#' @param max_speed Numeric. Threshold velocity limit for filtering outliers.
#' @param speed_col Character. Column name storing velocity variables.
#' @param interval_minutes Numeric. Resampling minutes for grid synchronization.
#' @param colony_coords Numeric vector. Named elements `lon` and `lat` of breeding grounds.
#' @param kud_ref Character. Bandwidth selection parameter (e.g., "href" or "LSCV").
#' @param density_levels Numeric vector. Percentage thresholds for home range core boundaries.
#'
#' @return A named list containing:
#'   \item{cleaned_tracks}{An sf object of the filtered, regularized spatial points.}
#'   \item{kud_estimates}{The raw KernelUD object from adehabitatHR.}
#'   \item{isopleth_polygons}{An sf data frame with computed area boundary contours.}
#'   \item{trip_summaries}{Summarized trajectory distances, durations, and dimensions.}
#' @export
estimate_space_use <- function(file_path,
                               col_map = NULL,
                               max_speed = 100,
                               speed_col = "Speed",
                               interval_minutes = 30,
                               colony_coords = c(lon = 0, lat = 0),
                               kud_ref = "href",
                               density_levels = c(50, 95)) {


  # =========================================================================
  # STEP 1: Import & Standardization
  # =========================================================================

  raw_data <- io_read::read_gps_data(file_path = file_path, format = "csv")
  std_data <- io_standardize::standardize_gps_columns(raw_data = raw_data, col_map = col_map)

  # Map standard target names down to expected internal structural parameters
  # mapping back to clean-filter variables: ID, Date, Time, Latitude, Longitude
  std_data <- std_data %>%
    dplyr::mutate(
      Date = format(timestamp, "%m/%d/%Y"),
      Time = format(timestamp, "%H:%M:%S")
    ) %>%
    dplyr::rename(
      ID = bird_id,
      Latitude = lat,
      Longitude = lon
    )

  # =========================================================================
  # STEP 2: Quality Control & Filtering
  # =========================================================================

  clean_data <- clean_filters::remove_duplicate_fixes(
    df = std_data,
    id_col = "ID",
    datetime_col = NULL,
    date_col = "Date",
    time_col = "Time"
  )

  if (speed_col %in% names(clean_data)) {
    clean_data <- clean_filters::filter_speed_outliers(
      df = clean_data,
      max_speed = max_speed,
      speed_col = speed_col,
      method = "remove"
    )
  }

  # =========================================================================
  # STEP 3: Spatiotemporal Regularization
  # =========================================================================
  regular_data <- clean_regularize::regularize_tracks(
    df = clean_data,
    id_col = "ID",
    date_col = "Date",
    time_col = "Time",
    lat_col = "Latitude",
    lon_col = "Longitude",
    interval_minutes = interval_minutes
  )

  # Clean up structural missing records introduced during timeline regularization
  regular_data <- regular_data %>%
    dplyr::filter(!is.na(Latitude) & !is.na(Longitude))

  # Re-expose package-wide schema variables required for Person 4 and 6 downstream
  regular_data <- regular_data %>%
    dplyr::rename(
      track_id = id,
      datetime_gmt = datetime_regular,
      latitude = Latitude,
      longitude = Longitude
    )

  # Convert regularized tracking table to sf object (WGS84 projection code 4326)
  tracks_sf <- sf::st_as_sf(
    regular_data,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  # Validate schema properties before spatial compilation
  utils_schema::validate_gps_data(tracks_sf, strict = FALSE)

  # =========================================================================
  # STEP 4: Segmentations & Trajectory Metrics (Person 4)
  # =========================================================================


  # Check if a trip identifier column exists; mock one if trip segmentation pipeline isn't automated
  if (!"trip_id" %in% names(tracks_sf)) {
    tracks_sf$trip_id <- paste0(tracks_sf$track_id, "_trip1")
  }

  # Compute step displacement characteristics
  trip_dist <- trip_metrics::calc_trip_distance(
    trip_data = as.data.frame(tracks_sf),
    bird_id_col = "track_id",
    trip_id_col = "trip_id",
    datetime_col = "datetime_gmt",
    lon_col = "longitude",
    lat_col = "latitude"
  )

  # =========================================================================
  # STEP 5: Space-Use & Home Ranges
  # =========================================================================

  # Re-expose traditional time formatting elements required by legacy spatial tools
  tracks_sf <- tracks_sf %>% dplyr::mutate(time = datetime_gmt)

  # Run your package module to extract the mathematical kernel calculations
  kud_output <- spaceuse_kud::calculate_kud(tracks = tracks_sf, ref = kud_ref)

  # Construct boundary polygon contours using your extraction script
  isopleth_polygons <- spaceuse_kud::get_isopleths(kud = kud_output, levels = density_levels)

  # Add area calculations to spatial polygons
  isopleth_polygons <- spaceuse_export::calculate_area_metrics(sf_polys = isopleth_polygons)

  # Calculate geographic distance summaries relative to colony coordinate nodes
  tracks_with_stats <- spaceuse_export::calculate_trip_stats(
    tracks = tracks_sf,
    colony_coords = colony_coords
  )

  # Compile consolidated trip metrics with bounding summaries
  final_trip_summaries <- tracks_with_stats %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(track_id, trip_id) %>%
    dplyr::summarise(
      max_distance_colony_km = max(max_dist_km, na.rm = TRUE),
      duration_hours = max(duration_hrs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(trip_dist, by = c("track_id", "trip_id"))

  # Return structured results
  return(
    list(
      cleaned_tracks     = tracks_with_stats,
      kud_estimates      = kud_output,
      isopleth_polygons  = isopleth_polygons,
      trip_summaries     = final_trip_summaries
    )
  )
}
