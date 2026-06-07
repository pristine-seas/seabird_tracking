#' Remove duplicate tracking fixes
#'
#' Removes duplicate rows where the same bird has multiple records
#' at the same timestamp. Keeps the first occurrence.
#'
#' @param df A data.frame containing tracking data
#' @param id_col Name of the bird ID column
#' @param datetime_col Name of the datetime column (optional if Date + Time provided)
#' @param date_col Name of the date column (used if datetime_col is NULL)
#' @param time_col Name of the time column (used if datetime_col is NULL)
#'
#' @return A data.frame with duplicate fixes removed
#' @export
remove_duplicate_fixes <- function(
    df,
    id_col = "ID",
    datetime_col = NULL,
    date_col = "Date",
    time_col = "Time"
) {

  # ---- 1. Validate input ----
  if (!id_col %in% names(df)) {
    stop(paste("Missing ID column:", id_col))
  }

  # ---- 2. Create datetime if needed ----
  if (!is.null(datetime_col)) {
    if (!datetime_col %in% names(df)) {
      stop(paste("Missing datetime column:", datetime_col))
    }
    df$..datetime <- df[[datetime_col]]
  } else {
    if (!all(c(date_col, time_col) %in% names(df))) {
      stop("Must provide either datetime_col OR both date_col and time_col")
    }

    df$..datetime <- as.POSIXct(
      paste(df[[date_col]], df[[time_col]]),
      format = "%m/%d/%Y %H:%M:%S",
      tz = "UTC"
    )
  }

  # ---- 3. Remove duplicates ----
  deduped <- df[!duplicated(df[, c(id_col, "..datetime")]), ]

  # ---- 4. Clean up helper column ----
  deduped$..datetime <- NULL

  return(deduped)
}

#' Filter speed outliers using recorded speed
#'
#' Removes or flags biologically unrealistic movement points based on
#' the provided Speed column.
#'
#' @param df A data.frame with tracking data
#' @param max_speed Maximum allowed speed (same units as Speed column)
#' @param speed_col Column name for speed
#' @param method "remove" to drop outliers, "flag" to mark them
#'
#' @return A data.frame with outliers removed or flagged
#' @export
filter_speed_outliers <- function(
    df,
    max_speed,
    speed_col = "Speed",
    method = c("remove", "flag")
) {

  method <- match.arg(method)

  # ---- 1. Validate ----
  if (!speed_col %in% names(df)) {
    stop(paste("Missing speed column:", speed_col))
  }

  # ---- 2. Coerce to numeric (defensive) ----
  speed <- as.numeric(df[[speed_col]])

  # ---- 3. Identify outliers ----
  outlier <- speed > max_speed

  # ---- 4. Apply method ----
  if (method == "flag") {
    df$..speed_outlier <- outlier
    return(df)
  }

  if (method == "remove") {
    df <- df[!outlier | is.na(outlier), ]
    return(df)
  }
}

#' Filter invalid or spatially excluded tracking points
#'
#' Removes or flags points with invalid coordinates or those falling within
#' a provided exclusion polygon (e.g., land).
#'
#' @param df A data.frame with tracking data
#' @param lat_col Column name for latitude
#' @param lon_col Column name for longitude
#' @param polygon Optional sf polygon defining excluded area (e.g., land)
#' @param method "remove" to drop rows, "flag" to mark them
#'
#' @return A data.frame with invalid/excluded points handled
#' @export
filter_on_land_or_invalid_points <- function(
    df,
    lat_col = "Latitude",
    lon_col = "Longitude",
    polygon = NULL,
    method = c("remove", "flag")
) {

  method <- match.arg(method)

  # ---- 1. Validate columns ----
  required <- c(lat_col, lon_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  lat <- as.numeric(df[[lat_col]])
  lon <- as.numeric(df[[lon_col]])

  # ---- 2. Invalid coordinate check ----
  invalid_coord <- is.na(lat) | is.na(lon) |
    lat < -90 | lat > 90 |
    lon < -180 | lon > 180

  # ---- 3. Spatial exclusion (optional) ----
  on_excluded_area <- rep(FALSE, nrow(df))

  if (!is.null(polygon)) {
    if (!inherits(polygon, "sf")) {
      stop("polygon must be an sf object")
    }

    # convert points to sf
    pts <- sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)

    # check if points fall inside polygon
    on_excluded_area <- as.logical(sf::st_within(pts, polygon, sparse = FALSE))
  }

  # ---- 4. Combine flags ----
  flagged <- invalid_coord | on_excluded_area

  # ---- 5. Apply method ----
  if (method == "flag") {
    df$..invalid_coord <- invalid_coord
    df$..on_excluded_area <- on_excluded_area
    df$..spatial_outlier <- flagged
    return(df)
  }

  if (method == "remove") {
    df <- df[!flagged, ]
    return(df)
  }
}
#' Flag low-quality GPS fixes
#'
#' Adds logical QA columns for high speed, high distance, invalid fix type, and
#' low essential-quality values when those checks are available.
#'
#' @param df A data frame.
#' @param speed_col Speed column.
#' @param max_speed Optional speed threshold.
#' @param distance_col Distance column.
#' @param max_distance Optional distance threshold.
#' @param fix_type_col Fix type column.
#' @param valid_fix_types Valid fix-type values.
#' @param essential_col Essential-quality column.
#' @param valid_essential_values Valid essential values.
#' @param prefix Prefix for generated QA columns.
#'
#' @return The input data frame with QA flag columns added.
#' @export
flag_low_quality_fixes <- function(df,
                                   speed_col = "Speed",
                                   max_speed = NULL,
                                   distance_col = "Distance",
                                   max_distance = NULL,
                                   fix_type_col = "Type",
                                   valid_fix_types = 0,
                                   essential_col = "Essential",
                                   valid_essential_values = 1,
                                   prefix = "..qa_") {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (!is.character(prefix) ||
      length(prefix) != 1 ||
      is.na(prefix)) {
    stop("`prefix` must be a single character string.", call. = FALSE)
  }

  flags <- list()
  skipped <- character(0)

  if (!is.null(speed_col) && !is.null(max_speed)) {
    if (!speed_col %in% names(df)) {
      skipped <- c(skipped, paste0("Speed column not found, skipping: ", speed_col))
    } else {
      speed <- suppressWarnings(as.numeric(df[[speed_col]]))
      flags[["high_speed"]] <- is.na(speed) | speed > max_speed
    }
  }

  if (!is.null(distance_col) && !is.null(max_distance)) {
    if (!distance_col %in% names(df)) {
      skipped <- c(skipped, paste0("Distance column not found, skipping: ", distance_col))
    } else {
      dist <- suppressWarnings(as.numeric(df[[distance_col]]))
      flags[["high_distance"]] <- is.na(dist) | dist > max_distance
    }
  }

  if (!is.null(fix_type_col) && !is.null(valid_fix_types)) {
    if (!fix_type_col %in% names(df)) {
      skipped <- c(skipped, paste0("Fix type column not found, skipping: ", fix_type_col))
    } else {
      type_vals <- as.character(df[[fix_type_col]])
      valid_vals <- as.character(valid_fix_types)
      flags[["invalid_type"]] <- !type_vals %in% valid_vals
    }
  }

  if (!is.null(essential_col) && !is.null(valid_essential_values)) {
    if (!essential_col %in% names(df)) {
      skipped <- c(skipped, paste0("Essential column not found, skipping: ", essential_col))
    } else {
      ess_vals <- as.character(df[[essential_col]])
      valid_ess <- as.character(valid_essential_values)
      flags[["low_essential"]] <- is.na(df[[essential_col]]) | !ess_vals %in% valid_ess
    }
  }

  if (length(skipped) > 0) {
    warning(paste(skipped, collapse = "; "), call. = FALSE)
  }

  if (length(flags) == 0) {
    warning(
      "No QA checks were applied - verify column names and that threshold arguments are non-NULL.",
      call. = FALSE
    )
    return(df)
  }

  for (flag_name in names(flags)) {
    df[[paste0(prefix, flag_name)]] <- flags[[flag_name]]
  }

  flag_matrix <- do.call(cbind, flags)
  df[[paste0(prefix, "any")]] <- rowSums(flag_matrix, na.rm = TRUE) > 0

  df
}
