#' Clean GPS tracking data in one step
#'
#' Top-level wrapper that runs the full cleaning pipeline: import
#' standardization, duplicate removal, speed filtering, optional land/invalid
#' point removal, track regularization, and interpolation.  Each stage can be
#' individually controlled through its arguments.
#'
#' @param data A data frame or tibble.  May be raw (pre-standardization) or
#'   already column-standardized.  Must contain at minimum \code{bird_id},
#'   \code{timestamp}, \code{lon}, and \code{lat} columns (or their raw
#'   equivalents when \code{col_map} is supplied).
#' @param col_map Named character vector mapping raw column names to package
#'   standard names, e.g. \code{c(bird_id = "tag_id", lon = "longitude")}.
#'   Pass \code{NULL} (default) when \code{data} is already standardized.
#' @param max_speed Numeric. Maximum plausible speed in m/s used by
#'   \code{filter_speed_outliers()}.  Default \code{30} m/s (~108 km/h),
#'   appropriate for most procellariids.
#' @param interval_minutes Numeric. Target resampling interval in minutes
#'   passed to \code{regularize_tracks()}.  Default \code{60}.
#' @param max_gap_minutes Numeric. Maximum gap in minutes across which
#'   \code{interpolate_tracks()} will fill points.  Gaps wider than this are
#'   left as NA.  Default \code{180} (3 hours).
#' @param land_polygon An \code{sf} polygon object used to remove on-land
#'   points via \code{filter_on_land_or_invalid_points()}.  Pass \code{NULL}
#'   (default) to skip this step.
#' @param flag_quality Logical.  When \code{TRUE} (default) low-quality fixes
#'   are flagged with \code{flag_low_quality_fixes()} before hard filters are
#'   applied.
#' @param drop_flagged Logical.  When \code{TRUE} rows whose
#'   \code{quality_flag} is not \code{"ok"} are dropped from the final output.
#'   Default \code{FALSE} (flags retained for downstream inspection).
#' @param verbose Logical.  Print progress messages.  Default \code{TRUE}.
#'
#' @return A tibble in the package standard track-table format with columns
#'   \code{bird_id}, \code{timestamp}, \code{lon}, \code{lat},
#'   \code{quality_flag}, and any additional columns carried through from
#'   \code{data}.  An attribute \code{"cleaning_log"} records row counts at
#'   each stage.
#'
#' @seealso \code{\link{standardize_gps_columns}},
#'   \code{\link{remove_duplicate_fixes}},
#'   \code{\link{filter_speed_outliers}},
#'   \code{\link{filter_on_land_or_invalid_points}},
#'   \code{\link{flag_low_quality_fixes}},
#'   \code{\link{regularize_tracks}},
#'   \code{\link{interpolate_tracks}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- read_gps_data("wtsh_2023.csv")
#' cleaned <- clean_tracks(raw, col_map = c(bird_id = "ring_number",
#'                                           timestamp = "datetime_utc",
#'                                           lon = "longitude",
#'                                           lat = "latitude"))
#' attr(cleaned, "cleaning_log")
#' }
clean_tracks <- function(
    data,
    col_map          = NULL,
    max_speed        = 30,
    interval_minutes = 60,
    max_gap_minutes  = 180,
    land_polygon     = NULL,
    flag_quality     = TRUE,
    drop_flagged     = FALSE,
    verbose          = TRUE
) {
  .msg <- function(...) if (verbose) message(...)

  # ── Input validation ────────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }
  if (!is.numeric(max_speed) || length(max_speed) != 1L || max_speed <= 0) {
    stop("`max_speed` must be a single positive number.")
  }
  if (!is.numeric(interval_minutes) || length(interval_minutes) != 1L ||
      interval_minutes <= 0) {
    stop("`interval_minutes` must be a single positive number.")
  }
  if (!is.numeric(max_gap_minutes) || length(max_gap_minutes) != 1L ||
      max_gap_minutes <= 0) {
    stop("`max_gap_minutes` must be a single positive number.")
  }
  if (!is.null(land_polygon) &&
      !inherits(land_polygon, c("sf", "sfc", "SpatialPolygonsDataFrame"))) {
    stop("`land_polygon` must be an sf/sfc/Spatial object or NULL.")
  }

  log <- list(input = nrow(data))

  # ── Stage 1: column standardization (optional) ──────────────────────────────
  if (!is.null(col_map)) {
    .msg("[clean_tracks] Stage 1/6: standardizing column names")
    data <- standardize_gps_columns(data, col_map = col_map)
  } else {
    .msg("[clean_tracks] Stage 1/6: column names assumed pre-standardized")
    data <- coerce_track_tbl(data)
  }
  log$after_standardize <- nrow(data)

  # ── Stage 2: quality flagging (optional) ────────────────────────────────────
  if (flag_quality) {
    .msg("[clean_tracks] Stage 2/6: flagging low-quality fixes")
    data <- flag_low_quality_fixes(data)
  }
  log$after_flagging <- nrow(data)

  # ── Stage 3: duplicate removal ───────────────────────────────────────────────
  .msg("[clean_tracks] Stage 3/6: removing duplicate fixes")
  data <- remove_duplicate_fixes(data)
  log$after_dedup <- nrow(data)

  # ── Stage 4: speed outlier filter ───────────────────────────────────────────
  .msg("[clean_tracks] Stage 4/6: filtering speed outliers (max_speed = ",
       max_speed, " m/s)")
  data <- filter_speed_outliers(data, max_speed = max_speed)
  log$after_speed_filter <- nrow(data)

  # ── Stage 4b: land / invalid point filter (optional) ────────────────────────
  if (!is.null(land_polygon)) {
    .msg("[clean_tracks] Stage 4b/6: removing on-land or invalid points")
    data <- filter_on_land_or_invalid_points(data,
                                              land_polygon = land_polygon)
    log$after_land_filter <- nrow(data)
  }

  # ── Stage 5: regularization ──────────────────────────────────────────────────
  .msg("[clean_tracks] Stage 5/6: regularizing tracks to ",
       interval_minutes, "-min intervals")
  data <- regularize_tracks(data, interval_minutes = interval_minutes)
  log$after_regularize <- nrow(data)

  # ── Stage 6: interpolation ───────────────────────────────────────────────────
  .msg("[clean_tracks] Stage 6/6: interpolating gaps up to ",
       max_gap_minutes, " min")
  data <- interpolate_tracks(data, max_gap_minutes = max_gap_minutes)
  log$after_interpolate <- nrow(data)

  # ── Optional: drop flagged rows ──────────────────────────────────────────────
  if (drop_flagged && "quality_flag" %in% names(data)) {
    data <- data[data$quality_flag == "ok", ]
    log$after_drop_flagged <- nrow(data)
  }

  .msg("[clean_tracks] Done. ",
       log$input, " rows in -> ", nrow(data), " rows out.")

  attr(data, "cleaning_log") <- log
  data
}
