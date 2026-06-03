#' Analyze Fisheries Overlap
#'
#' Runs the fisheries overlap workflow from prepared seabird tracking data and
#' fisheries effort data. This wrapper standardizes fisheries effort, converts
#' inputs to sf objects when needed, spatially joins tracks to fisheries data,
#' calculates overlap metrics, optionally calculates risk scores, and optionally
#' calculates diel overlap.
#'
#' @param track_data A data frame or sf object containing seabird track points.
#' @param fisheries_data A data frame or sf object containing fisheries effort
#'   points, grid cells, or polygons.
#' @param gear_weights Optional named numeric vector or data frame of gear
#'   weights. Required only if `calculate_risk = TRUE`.
#' @param track_id_col Character. Column identifying each bird or track.
#' @param track_lon_col Character. Longitude column in `track_data` if not sf.
#' @param track_lat_col Character. Latitude column in `track_data` if not sf.
#' @param fisheries_lon_col Character. Longitude column in `fisheries_data` if
#'   not sf.
#' @param fisheries_lat_col Character. Latitude column in `fisheries_data` if
#'   not sf.
#' @param effort_col Character. Raw fisheries effort column.
#' @param gear_col Character. Fishing gear column.
#' @param cell_id_col Optional character. Fisheries grid/cell ID column.
#' @param join_type Character. One of `"intersects"`, `"within"`, or `"nearest"`.
#' @param crs Coordinate reference system for non-sf inputs. Default is 4326.
#' @param standardize_effort Logical. If `TRUE`, creates `effort_std`.
#' @param log_transform Logical. If `TRUE`, creates `effort_log`.
#' @param overlap_effort_col Character. Effort column used for overlap metrics.
#'   Default is `"effort_std"`.
#' @param calculate_risk Logical. If `TRUE`, runs `calc_risk_index()`.
#' @param scale_risk_01 Logical. If `TRUE`, rescales risk index to [0, 1].
#' @param calculate_diel Logical. If `TRUE`, runs `calc_diel_overlap()`.
#' @param track_diel_col Character. Diel-period column from track data.
#' @param fisheries_diel_col Character. Diel-period column from fisheries data.
#'
#' @return A named list containing standardized fisheries data, sf inputs,
#'   joined overlap data, overlap metrics, gear summaries, risk results, and
#'   diel overlap results where requested.
#'
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
                                      standardize_effort = TRUE,
                                      log_transform = FALSE,
                                      overlap_effort_col = "effort_std",
                                      calculate_risk = TRUE,
                                      scale_risk_01 = TRUE,
                                      calculate_diel = FALSE,
                                      track_diel_col = "diel_period",
                                      fisheries_diel_col = "diel_period") {

  join_type <- match.arg(join_type)

  if (!is.data.frame(track_data) && !inherits(track_data, "sf")) {
    stop("`track_data` must be a data frame, tibble, or sf object.", call. = FALSE)
  }

  if (!is.data.frame(fisheries_data) && !inherits(fisheries_data, "sf")) {
    stop("`fisheries_data` must be a data frame, tibble, or sf object.", call. = FALSE)
  }

  if (calculate_risk && is.null(gear_weights)) {
    stop(
      "`gear_weights` must be provided when `calculate_risk = TRUE`.",
      call. = FALSE
    )
  }

  # 1. Standardize fisheries effort and gear labels.
  fisheries_clean <- standardize_fishing_effort(
    fisheries_data = fisheries_data,
    effort_col = effort_col,
    gear_col = gear_col,
    gear_map = NULL,
    standardize_effort = standardize_effort,
    log_transform = log_transform
  )

  # If user asks for effort_std but standardization is off, fall back to raw effort.
  if (!overlap_effort_col %in% names(fisheries_clean)) {
    if (effort_col %in% names(fisheries_clean)) {
      warning(
        "`overlap_effort_col` was not found. Using raw `effort_col` instead.",
        call. = FALSE
      )
      overlap_effort_col <- effort_col
    } else {
      stop("No usable effort column found for overlap analysis.", call. = FALSE)
    }
  }

  # 2. Convert track data to sf if needed.
  if (inherits(track_data, "sf")) {
    track_sf <- track_data
  } else {
    needed_track <- c(track_id_col, track_lon_col, track_lat_col)
    missing_track <- setdiff(needed_track, names(track_data))

    if (length(missing_track) > 0) {
      stop("Missing from track_data: ", paste(missing_track, collapse = ", "), call. = FALSE)
    }

    track_sf <- sf::st_as_sf(
      track_data,
      coords = c(track_lon_col, track_lat_col),
      crs = crs,
      remove = FALSE
    )
  }

  # 3. Convert fisheries data to sf if needed.
  if (inherits(fisheries_clean, "sf")) {
    fisheries_sf <- fisheries_clean
  } else {
    needed_fish <- c(fisheries_lon_col, fisheries_lat_col, effort_col, gear_col)
    missing_fish <- setdiff(needed_fish, names(fisheries_clean))

    if (length(missing_fish) > 0) {
      stop("Missing from fisheries_data: ", paste(missing_fish, collapse = ", "), call. = FALSE)
    }

    fisheries_sf <- as_fisheries_sf(
      data = fisheries_clean,
      lon_col = fisheries_lon_col,
      lat_col = fisheries_lat_col,
      crs = crs
    )
  }

  # 4. Check CRS and transform fisheries data if needed.
  assert_crs(track_sf)
  assert_crs(fisheries_sf)

  if (sf::st_crs(track_sf) != sf::st_crs(fisheries_sf)) {
    fisheries_sf <- sf::st_transform(fisheries_sf, sf::st_crs(track_sf))
  }

  # 5. Spatially join tracks to fisheries grid/points/polygons.
  joined_overlap <- join_tracks_to_fishing_grid(
    track_data = track_sf,
    fisheries_data = fisheries_sf,
    join_type = join_type
  )

  # 6. Calculate overlap metrics.
  overlap_metrics <- calc_fisheries_overlap(
    joined_data = joined_overlap,
    track_id_col = track_id_col,
    effort_col = overlap_effort_col,
    gear_col = gear_col,
    cell_id_col = cell_id_col
  )

  # 7. Summarize by gear.
  gear_summary <- summarize_overlap_by_gear(
    overlap_data = overlap_metrics,
    gear_col = gear_col,
    overlap_col = "total_overlap"
  )

  # 8. Optionally calculate risk.
  risk_results <- NULL

  if (calculate_risk) {
    risk_results <- calc_risk_index(
      overlap_data = overlap_metrics,
      overlap_col = "total_overlap",
      gear_col = gear_col,
      gear_weights = gear_weights,
      scale_01 = scale_risk_01
    )
  }

  # 9. Optionally calculate diel overlap.
  diel_overlap <- NULL

  if (calculate_diel) {
    joined_names <- names(joined_overlap)

    # sf::st_join() may create .x/.y suffixes if both datasets have the same
    # diel-period column name.
    possible_track_diel_cols <- c(
      paste0(track_diel_col, ".x"),
      track_diel_col
    )

    possible_fisheries_diel_cols <- c(
      paste0(fisheries_diel_col, ".y"),
      fisheries_diel_col
    )

    actual_track_diel_col <- possible_track_diel_cols[
      possible_track_diel_cols %in% joined_names
    ][1]

    actual_fisheries_diel_col <- possible_fisheries_diel_cols[
      possible_fisheries_diel_cols %in% joined_names
    ][1]

    if (is.na(actual_track_diel_col) || is.na(actual_fisheries_diel_col)) {
      warning(
        "Diel overlap requested, but diel-period columns were not found after joining. ",
        "Returning `diel_overlap = NULL`.",
        call. = FALSE
      )
    } else {
      diel_overlap <- calc_diel_overlap(
        joined_data = joined_overlap,
        track_id_col = track_id_col,
        track_diel_col = actual_track_diel_col,
        fisheries_diel_col = actual_fisheries_diel_col,
        effort_col = overlap_effort_col
      )
    }
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
