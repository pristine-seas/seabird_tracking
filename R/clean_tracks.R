#' Clean GPS tracking data in one step
#'
#' Top-level wrapper that runs the full cleaning pipeline: column
#' standardization/coercion, quality flagging, duplicate removal, speed filtering,
#' optional spatial filtering, optional regularization, and interpolation.
#'
#' @param data A data frame or tibble containing GPS tracking data.
#' @param col_map Optional named character vector/list used for column
#'   standardization.
#' @param max_speed Numeric. Maximum plausible speed.
#' @param speed_col Character. Name of speed column.
#' @param regularize Logical. If TRUE, run `regularize_tracks()` and
#'   `interpolate_tracks()`.
#' @param interval_minutes Numeric. Regularization interval in minutes.
#' @param max_gap_minutes Numeric. Maximum gap to interpolate.
#' @param land_polygon Optional sf/sfc/Spatial object used to filter excluded
#'   spatial points.
#' @param flag_quality Logical. If TRUE, run `flag_low_quality_fixes()`.
#' @param drop_flagged Logical. If TRUE, remove rows flagged as low quality.
#' @param verbose Logical. If TRUE, print progress messages.
#'
#' @return A cleaned data frame with a `cleaning_log` attribute.
#' @export
clean_tracks <- function(data,
                         col_map = NULL,
                         max_speed = 30,
                         speed_col = "Speed",
                         regularize = TRUE,
                         interval_minutes = 60,
                         max_gap_minutes = 180,
                         land_polygon = NULL,
                         flag_quality = TRUE,
                         drop_flagged = FALSE,
                         verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.numeric(max_speed) ||
      length(max_speed) != 1 ||
      is.na(max_speed) ||
      max_speed <= 0) {
    stop("`max_speed` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(interval_minutes) ||
      length(interval_minutes) != 1 ||
      is.na(interval_minutes) ||
      interval_minutes <= 0) {
    stop("`interval_minutes` must be a single positive number.", call. = FALSE)
  }

  if (!is.numeric(max_gap_minutes) ||
      length(max_gap_minutes) != 1 ||
      is.na(max_gap_minutes) ||
      max_gap_minutes <= 0) {
    stop("`max_gap_minutes` must be a single positive number.", call. = FALSE)
  }

  if (!is.logical(regularize) ||
      length(regularize) != 1 ||
      is.na(regularize)) {
    stop("`regularize` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(flag_quality) ||
      length(flag_quality) != 1 ||
      is.na(flag_quality)) {
    stop("`flag_quality` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(drop_flagged) ||
      length(drop_flagged) != 1 ||
      is.na(drop_flagged)) {
    stop("`drop_flagged` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) ||
      length(verbose) != 1 ||
      is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(speed_col) ||
      length(speed_col) != 1 ||
      is.na(speed_col) ||
      speed_col == "") {
    stop("`speed_col` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.null(land_polygon) &&
      !inherits(land_polygon, c("sf", "sfc", "Spatial"))) {
    stop("`land_polygon` must be an sf/sfc/Spatial object or NULL.", call. = FALSE)
  }

  msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  input_n <- nrow(data)
  out <- data

  msg("[clean_tracks] Stage 1/6: standardizing/coercing columns")

  if (!is.null(col_map)) {
    out <- standardize_gps_columns(
      raw_data = out,
      col_map = col_map,
      add_missing_cols = TRUE
    )
  } else {
    out <- coerce_track_tbl(
      data = out,
      validate = FALSE,
      as_tibble = FALSE
    )
  }

  msg("[clean_tracks] Stage 2/6: flagging low-quality fixes")

  if (isTRUE(flag_quality)) {
    out <- flag_low_quality_fixes(out)
  }

  msg("[clean_tracks] Stage 3/6: removing duplicate fixes")

  out <- remove_duplicate_fixes(out)

  msg("[clean_tracks] Stage 4/6: filtering speed outliers")

  if (speed_col %in% names(out)) {
    out <- filter_speed_outliers(
      df = out,
      max_speed = max_speed,
      speed_col = speed_col,
      method = "remove"
    )
  }

  msg("[clean_tracks] Stage 5/6: optional spatial filtering")

  if (!is.null(land_polygon)) {
    out <- filter_on_land_or_invalid_points(
      df = out,
      polygon = land_polygon,
      method = "remove"
    )
  }

  if (isTRUE(drop_flagged)) {
    if ("quality_flag" %in% names(out)) {
      out <- out[out$quality_flag == "ok" | is.na(out$quality_flag), , drop = FALSE]
    }

    if ("..qa_any" %in% names(out)) {
      out <- out[is.na(out$..qa_any) | !out$..qa_any, , drop = FALSE]
    }

    if ("..spatial_outlier" %in% names(out)) {
      out <- out[is.na(out$..spatial_outlier) | !out$..spatial_outlier, , drop = FALSE]
    }

    if ("..speed_outlier" %in% names(out)) {
      out <- out[is.na(out$..speed_outlier) | !out$..speed_outlier, , drop = FALSE]
    }
  }

  msg("[clean_tracks] Stage 6/6: optional regularization/interpolation")

  if (isTRUE(regularize)) {
    out <- regularize_tracks(
      out,
      interval_minutes = interval_minutes
    )

    out <- interpolate_tracks(
      out,
      max_gap_minutes = max_gap_minutes
    )
  }

  attr(out, "cleaning_log") <- list(
    input = input_n,
    output = nrow(out),
    max_speed = max_speed,
    speed_col = speed_col,
    regularize = regularize,
    interval_minutes = interval_minutes,
    max_gap_minutes = max_gap_minutes,
    flag_quality = flag_quality,
    drop_flagged = drop_flagged
  )

  msg("[clean_tracks] Complete")

  out
}
