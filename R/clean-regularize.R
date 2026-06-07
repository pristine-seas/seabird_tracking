#' Regularize GPS tracks to a common time interval
#'
#' Resamples each individual's track to a uniform time step for comparability
#' across birds. The output is a regular time grid per bird with NA coordinates
#' wherever no fix fell within the tolerance window around each grid point.
#'
#' @param df A data.frame with tracking data
#' @param id_col Column name for bird ID
#' @param date_col Column name for date
#' @param time_col Column name for time
#' @param lat_col Column name for latitude
#' @param lon_col Column name for longitude
#' @param interval_minutes Target time interval in minutes for the regularized grid
#' @param tolerance_minutes Half-window in minutes around each grid point within
#'   which an observed fix is considered a match. Defaults to half the interval.
#'
#' @return A data.frame with one row per bird per regular time step.
#' @export
regularize_tracks <- function(
    df,
    id_col            = "ID",
    date_col          = "Date",
    time_col          = "Time",
    lat_col           = "Latitude",
    lon_col           = "Longitude",
    interval_minutes  = 30,
    tolerance_minutes = NULL
) {
  required <- c(id_col, date_col, time_col, lat_col, lon_col)
  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")), call. = FALSE)
  }

  if (!is.numeric(interval_minutes) ||
      length(interval_minutes) != 1 ||
      is.na(interval_minutes) ||
      interval_minutes <= 0) {
    stop("interval_minutes must be a single positive number", call. = FALSE)
  }

  if (is.null(tolerance_minutes)) {
    tolerance_minutes <- interval_minutes / 2
  }

  if (!is.numeric(tolerance_minutes) ||
      length(tolerance_minutes) != 1 ||
      is.na(tolerance_minutes) ||
      tolerance_minutes < 0) {
    stop("tolerance_minutes must be a single non-negative number", call. = FALSE)
  }

  datetime <- as.POSIXct(
    paste(df[[date_col]], df[[time_col]]),
    format = "%m/%d/%Y %H:%M:%S",
    tz = "UTC"
  )

  if (all(is.na(datetime))) {
    stop("datetime parsing failed - check date_col and time_col formats", call. = FALSE)
  }

  df$..datetime <- datetime

  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))

  tolerance_secs <- tolerance_minutes * 60
  interval_secs <- interval_minutes * 60

  floor_to_interval <- function(x, interval_secs) {
    as.POSIXct(
      floor(as.numeric(x) / interval_secs) * interval_secs,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }

  ceiling_to_interval <- function(x, interval_secs) {
    x_num <- as.numeric(x)
    out_num <- ceiling(x_num / interval_secs) * interval_secs

    as.POSIXct(
      out_num,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }

  ids <- unique(df[[id_col]])
  results <- vector("list", length(ids))

  for (i in seq_along(ids)) {
    bird <- ids[i]
    idx <- df[[id_col]] == bird

    bird_dt <- df$..datetime[idx]
    bird_lat <- lat[idx]
    bird_lon <- lon[idx]

    valid <- !is.na(bird_dt)

    bird_dt <- bird_dt[valid]
    bird_lat <- bird_lat[valid]
    bird_lon <- bird_lon[valid]

    if (length(bird_dt) == 0) {
      next
    }

    ord <- order(bird_dt)

    bird_dt <- bird_dt[ord]
    bird_lat <- bird_lat[ord]
    bird_lon <- bird_lon[ord]

    round_to_interval <- function(x, interval_secs) {
      as.POSIXct(
        round(as.numeric(x) / interval_secs) * interval_secs,
        origin = "1970-01-01",
        tz = "UTC"
      )
    }

    t_start <- round_to_interval(min(bird_dt), interval_secs)
    t_end <- round_to_interval(max(bird_dt), interval_secs)

    grid <- seq(
      from = t_start,
      to = t_end,
      by = interval_secs
    )

    grid_lat <- rep(NA_real_, length(grid))
    grid_lon <- rep(NA_real_, length(grid))
    observed <- rep(FALSE, length(grid))

    used_fix <- rep(FALSE, length(bird_dt))

    for (j in seq_along(grid)) {
      diffs <- abs(as.numeric(difftime(bird_dt, grid[j], units = "secs")))

      if (all(is.na(diffs))) {
        next
      }

      nearest <- which.min(diffs)

      if (length(nearest) == 1 &&
          !is.na(diffs[nearest]) &&
          diffs[nearest] <= tolerance_secs &&
          !used_fix[nearest]) {
        grid_lat[j] <- bird_lat[nearest]
        grid_lon[j] <- bird_lon[nearest]
        observed[j] <- TRUE
        used_fix[nearest] <- TRUE
      }
    }

    results[[i]] <- data.frame(
      id = bird,
      datetime_regular = as.POSIXct(grid, origin = "1970-01-01", tz = "UTC"),
      lat = grid_lat,
      lon = grid_lon,
      is_observed = as.logical(observed),
      stringsAsFactors = FALSE
    )

    names(results[[i]])[1] <- id_col
  }

  results <- results[!vapply(results, is.null, logical(1))]

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  out$is_observed[is.na(out$is_observed)] <- FALSE
  out$is_observed <- as.logical(out$is_observed)

  out
}


#' Interpolate small gaps in regularized GPS tracks
#'
#' Fills NA positions in a regularized track using linear interpolation,
#' but only across gaps smaller than or equal to max_gap_minutes. Larger gaps
#' are left as NA.
#'
#' @param df A regularized track data.frame
#' @param id_col Column name for bird ID
#' @param datetime_col Column name for regular grid timestamps
#' @param lat_col Column name for latitude
#' @param lon_col Column name for longitude
#' @param max_gap_minutes Maximum gap length in minutes to interpolate across.
#'
#' @return The input data.frame with interpolated coordinates and
#'   `is_interpolated`.
#' @export
interpolate_tracks <- function(
    df,
    id_col          = "ID",
    datetime_col    = "datetime_regular",
    lat_col         = "lat",
    lon_col         = "lon",
    max_gap_minutes = 60
) {
  required <- c(id_col, datetime_col, lat_col, lon_col)
  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")), call. = FALSE)
  }

  if (!inherits(df[[datetime_col]], "POSIXct")) {
    stop(paste(datetime_col, "must be POSIXct - pass output from regularize_tracks() directly"), call. = FALSE)
  }

  if (!is.numeric(max_gap_minutes) ||
      length(max_gap_minutes) != 1 ||
      is.na(max_gap_minutes) ||
      max_gap_minutes < 0) {
    stop("max_gap_minutes must be a single non-negative number", call. = FALSE)
  }

  df$is_interpolated <- FALSE

  ids <- unique(df[[id_col]])

  for (bird in ids) {
    idx <- which(df[[id_col]] == bird)

    dts <- as.numeric(df[[datetime_col]][idx])
    lats <- df[[lat_col]][idx]
    lons <- df[[lon_col]][idx]

    ord <- order(dts)

    idx <- idx[ord]
    dts <- dts[ord]
    lats <- lats[ord]
    lons <- lons[ord]

    missing_pos <- is.na(lats) | is.na(lons)

    if (!any(missing_pos)) {
      next
    }

    if (length(dts) > 1) {
      interval_mins <- median(diff(dts), na.rm = TRUE) / 60
    } else {
      interval_mins <- NA_real_
    }

    runs <- rle(missing_pos)
    run_end <- cumsum(runs$lengths)
    run_start <- c(1, run_end[-length(run_end)] + 1)

    for (k in seq_along(runs$lengths)) {
      if (!runs$values[k]) {
        next
      }

      gap_idx <- run_start[k]:run_end[k]
      before <- run_start[k] - 1
      after <- run_end[k] + 1

      if (before < 1 || after > length(lats)) {
        next
      }

      if (is.na(lats[before]) || is.na(lons[before]) ||
          is.na(lats[after]) || is.na(lons[after])) {
        next
      }

      anchor_gap_minutes <- (dts[after] - dts[before]) / 60

      gap_minutes <- if (length(gap_idx) == 1) {
        anchor_gap_minutes
      } else {
        length(gap_idx) * interval_mins
      }

      if (is.na(gap_minutes) || gap_minutes > max_gap_minutes) {
        next
      }

      t_before <- dts[before]
      t_after <- dts[after]
      t_gap <- dts[gap_idx]

      if (t_after == t_before) {
        next
      }

      weight <- (t_gap - t_before) / (t_after - t_before)

      df[[lat_col]][idx[gap_idx]] <- lats[before] +
        weight * (lats[after] - lats[before])

      df[[lon_col]][idx[gap_idx]] <- lons[before] +
        weight * (lons[after] - lons[before])

      df$is_interpolated[idx[gap_idx]] <- TRUE
    }
  }

  df$is_interpolated[is.na(df$is_interpolated)] <- FALSE
  df$is_interpolated <- as.logical(df$is_interpolated)

  df
}


#' Summarize data gaps and interpolation burden per individual
#'
#' Produces a per-bird QA summary table describing missingness, gap structure,
#' and how much of each track was gap-filled by interpolation.
#'
#' @param df A track data.frame after interpolation
#' @param id_col Column name for bird ID
#' @param datetime_col Column name for regular grid timestamps
#' @param lat_col Column name for latitude
#' @param is_observed_col Column name for observed fix flag
#' @param is_interpolated_col Column name for interpolation flag
#'
#' @return A data.frame with one row per bird.
#' @export
summarize_data_gaps <- function(
    df,
    id_col              = "ID",
    datetime_col        = "datetime_regular",
    lat_col             = "lat",
    is_observed_col     = "is_observed",
    is_interpolated_col = "is_interpolated"
) {
  required <- c(
    id_col,
    datetime_col,
    lat_col,
    is_observed_col,
    is_interpolated_col
  )

  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")), call. = FALSE)
  }

  ids <- unique(df[[id_col]])
  results <- vector("list", length(ids))

  for (i in seq_along(ids)) {
    bird <- ids[i]
    idx <- df[[id_col]] == bird

    dts <- df[[datetime_col]][idx]
    lats <- df[[lat_col]][idx]
    is_obs <- as.logical(df[[is_observed_col]][idx])
    is_interp <- as.logical(df[[is_interpolated_col]][idx])

    is_obs[is.na(is_obs)] <- FALSE
    is_interp[is.na(is_interp)] <- FALSE

    n_total <- sum(idx)
    n_observed <- sum(is_obs, na.rm = TRUE)
    n_interp <- sum(is_interp, na.rm = TRUE)
    n_missing <- sum(is.na(lats))

    raw_missing <- !is_obs

    runs <- rle(raw_missing)
    gap_lengths <- runs$lengths[runs$values]
    n_gaps <- length(gap_lengths)

    if (n_total > 1) {
      median_step_mins <- median(
        as.numeric(diff(dts), units = "mins"),
        na.rm = TRUE
      )
    } else {
      median_step_mins <- NA_real_
    }

    gap_lengths_mins <- gap_lengths * median_step_mins

    results[[i]] <- data.frame(
      id = bird,
      n_fixes_total = n_total,
      n_observed = n_observed,
      n_interpolated = n_interp,
      n_missing = n_missing,
      pct_observed = round(100 * n_observed / n_total, 1),
      pct_interpolated = round(100 * n_interp / n_total, 1),
      pct_missing = round(100 * n_missing / n_total, 1),
      n_gaps = n_gaps,
      max_gap_minutes = if (n_gaps > 0) {
        round(max(gap_lengths_mins), 1)
      } else {
        0
      },
      mean_gap_minutes = if (n_gaps > 0) {
        round(mean(gap_lengths_mins), 1)
      } else {
        0
      },
      track_start = min(dts, na.rm = TRUE),
      track_end = max(dts, na.rm = TRUE),
      track_duration_hours = round(
        as.numeric(
          difftime(
            max(dts, na.rm = TRUE),
            min(dts, na.rm = TRUE),
            units = "hours"
          )
        ),
        2
      ),
      stringsAsFactors = FALSE
    )

    names(results[[i]])[1] <- id_col
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  out
}
