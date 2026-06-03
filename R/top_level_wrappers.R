#' Top-Level Space-Use Analysis Wrapper
#'
#' Runs the main space-use workflow from raw GPS data import through data
#' cleaning, regularization, kernel utilization distribution estimation,
#' isopleth extraction, area calculation, and trip-level movement summaries.
#'
#' This function is intended as a high-level convenience wrapper that combines
#' lower-level Shearwater package functions into one reproducible workflow.
#'
#' @param file_path Character. Path to the raw GPS telemetry dataset.
#' @param col_map Optional named character vector used for column
#'   standardization.
#' @param max_speed Numeric. Maximum allowed speed for filtering outlier fixes.
#' @param speed_col Character. Name of the speed column.
#' @param interval_minutes Numeric. Time interval, in minutes, used to
#'   regularize tracks.
#' @param colony_coords Named numeric vector with `lon` and `lat` giving the
#'   breeding colony location.
#' @param kud_ref Character or numeric. Kernel smoothing parameter passed to
#'   `calculate_kud()`. Common options are `"href"` and `"LSCV"`.
#' @param density_levels Numeric vector. Utilization distribution percentage
#'   thresholds used to extract isopleths, such as `c(50, 95)`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{cleaned_tracks}{An `sf` object containing filtered and regularized
#'   tracking points with trip statistics.}
#'   \item{kud_estimates}{A kernel utilization distribution object produced by
#'   `calculate_kud()`.}
#'   \item{isopleth_polygons}{An `sf` object containing utilization distribution
#'   contour polygons with area metrics.}
#'   \item{trip_summaries}{A data frame of trip-level distance, duration, and
#'   colony-distance summaries.}
#' }
#'
#' @export
estimate_space_use <- function(file_path,
                               col_map = NULL,
                               max_speed = 100,
                               speed_col = "Speed",
                               interval_minutes = 30,
                               colony_coords = c(lon = 0, lat = 0),
                               kud_ref = "href",
                               density_levels = c(50, 95)) {
  # -------------------------------------------------------------------------
  # Input validation
  # -------------------------------------------------------------------------

  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("`file_path` must be a single non-missing character string.", call. = FALSE)
  }

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path, call. = FALSE)
  }

  if (!is.numeric(max_speed) || length(max_speed) != 1 || max_speed <= 0) {
    stop("`max_speed` must be a single positive number.", call. = FALSE)
  }

  if (!is.character(speed_col) || length(speed_col) != 1) {
    stop("`speed_col` must be a single character string.", call. = FALSE)
  }

  if (!is.numeric(interval_minutes) ||
      length(interval_minutes) != 1 ||
      interval_minutes <= 0) {
    stop("`interval_minutes` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(colony_coords) ||
      !all(c("lon", "lat") %in% names(colony_coords))) {
    stop(
      "`colony_coords` must be a named numeric vector with names `lon` and `lat`.",
      call. = FALSE
    )
  }

  if (!is.numeric(density_levels) || length(density_levels) < 1) {
    stop("`density_levels` must be a numeric vector.", call. = FALSE)
  }

  if (any(density_levels <= 0 | density_levels >= 100)) {
    stop("`density_levels` must contain values greater than 0 and less than 100.",
         call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # Step 1: Import and standardize raw GPS data
  # -------------------------------------------------------------------------

  raw_data <- read_gps_data(
    file_path = file_path,
    format = "csv"
  )

  std_data <- standardize_gps_columns(
    raw_data = raw_data,
    col_map = col_map
  )

  needed_standard_cols <- c("timestamp", "bird_id", "lat", "lon")
  missing_standard_cols <- setdiff(needed_standard_cols, names(std_data))

  if (length(missing_standard_cols) > 0) {
    stop(
      "After standardization, data is missing required columns: ",
      paste(missing_standard_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Convert package-wide standard names to names expected by cleaning functions.
  std_data <- std_data %>%
    dplyr::mutate(
      Date = format(.data$timestamp, "%m/%d/%Y"),
      Time = format(.data$timestamp, "%H:%M:%S")
    ) %>%
    dplyr::rename(
      ID = .data$bird_id,
      Latitude = .data$lat,
      Longitude = .data$lon
    )

  # -------------------------------------------------------------------------
  # Step 2: Quality control and filtering
  # -------------------------------------------------------------------------

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

  # -------------------------------------------------------------------------
  # Step 3: Spatiotemporal regularization
  # -------------------------------------------------------------------------

  regular_data <- regularize_tracks(
    df = clean_data,
    id_col = "ID",
    date_col = "Date",
    time_col = "Time",
    lat_col = "Latitude",
    lon_col = "Longitude",
    interval_minutes = interval_minutes
  )

  regular_data <- regular_data %>%
    dplyr::filter(
      !is.na(.data$Latitude),
      !is.na(.data$Longitude)
    )

  needed_regular_cols <- c("id", "datetime_regular", "Latitude", "Longitude")
  missing_regular_cols <- setdiff(needed_regular_cols, names(regular_data))

  if (length(missing_regular_cols) > 0) {
    stop(
      "After regularization, data is missing required columns: ",
      paste(missing_regular_cols, collapse = ", "),
      call. = FALSE
    )
  }

  regular_data <- regular_data %>%
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

  # -------------------------------------------------------------------------
  # Step 4: Trip identifiers and movement metrics
  # -------------------------------------------------------------------------

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

  # -------------------------------------------------------------------------
  # Step 5: Space-use and home-range estimation
  # -------------------------------------------------------------------------

  tracks_sf <- tracks_sf %>%
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

  # -------------------------------------------------------------------------
  # Step 6: Final trip summaries
  # -------------------------------------------------------------------------

  final_trip_summaries <- tracks_with_stats %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$track_id, .data$trip_id) %>%
    dplyr::summarise(
      max_distance_colony_km = max(.data$max_dist_km, na.rm = TRUE),
      duration_hours = max(.data$duration_hrs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      trip_dist,
      by = c("track_id", "trip_id")
    )

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  list(
    cleaned_tracks = tracks_with_stats,
    kud_estimates = kud_output,
    isopleth_polygons = isopleth_polygons,
    trip_summaries = final_trip_summaries
  )
}
