

#' Process Bird Tracking Data
#'
#' Filters bird tracking data by fledge date and movement velocity.
#'
#' This function maps each bird ID to a start date, removes records before that
#' start date, parses datetime values, calculates velocities from step distance,
#' and removes records with unrealistic movement distances or speeds.
#'
#' @param data A data frame containing bird tracking data. Must include columns
#'   for bird ID, `datetime`, `type`, and `correct_step_distance`.
#' @param start_dates_vec A character vector of fledge or start dates. These
#'   should correspond to the unique bird IDs in `data`.
#' @param id_col The bird ID column, supplied unquoted.
#'
#' @return A filtered data frame with parsed date columns and calculated
#'   velocities.
#'
#' @export
process_bird_data <- function(data, start_dates_vec, id_col) {
  id_quo <- rlang::enquo(id_col)
  id_name <- rlang::as_name(id_quo)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(id_name, "datetime", "type", "correct_step_distance")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  tag_ids <- data |>
    dplyr::select(dplyr::all_of(id_name)) |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()

  if (length(tag_ids) != length(start_dates_vec)) {
    stop(
      "`start_dates_vec` must have the same length as the number of unique IDs.",
      call. = FALSE
    )
  }

  date_mapping <- data.frame(
    temp_ids = tag_ids,
    Start_Date = lubridate::mdy(start_dates_vec),
    stringsAsFactors = FALSE
  )

  out <- data |>
    dplyr::mutate(
      date_parsed = lubridate::parse_date_time(
        .data$datetime,
        orders = c("ymd HMS", "ymd", "mdy HMS", "mdy")
      ),
      Month = lubridate::month(.data$date_parsed),
      Day = lubridate::day(.data$date_parsed),
      Year = lubridate::year(.data$date_parsed)
    ) |>
    dplyr::left_join(
      date_mapping,
      by = stats::setNames("temp_ids", id_name)
    ) |>
    dplyr::filter(.data$date_parsed >= .data$Start_Date) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_name))) |>
    dplyr::arrange(.data$date_parsed, .by_group = TRUE) |>
    dplyr::mutate(
      timestamp = .data$date_parsed + ifelse(
        .data$type == "noon",
        lubridate::hours(12),
        lubridate::hours(0)
      ),
      time_diff = as.numeric(
        difftime(
          .data$timestamp,
          dplyr::lag(.data$timestamp),
          units = "hours"
        )
      ),
      divisor = ifelse(
        !is.na(.data$time_diff) & abs(.data$time_diff - 12) < 0.1,
        12,
        24
      ),
      velocities = .data$correct_step_distance / .data$divisor
    ) |>
    dplyr::filter(
      .data$correct_step_distance <= 960,
      .data$velocities <= 80
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -dplyr::all_of(c("timestamp", "time_diff", "divisor"))
    )

  out
}


#' Apply Geospatial Filters to Bird Tracking Data
#'
#' Removes records during equinox periods, records north of 65 degrees latitude,
#' and points within 50 kilometers of land.
#'
#' @param data A data frame containing `longitude`, `latitude`, `Month`, and
#'   `Day` columns.
#'
#' @return A filtered data frame containing only records that pass the
#'   geospatial filters.
#'
#' @export
apply_geospatial_filters <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_cols <- c("longitude", "latitude", "Month", "Day")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data |>
    dplyr::filter(
      !(
        (.data$Month == 3 & .data$Day >= 10 & .data$Day <= 30) |
          (.data$Month == 9 & .data$Day >= 12) |
          (.data$Month == 10 & .data$Day <= 1)
      )
    ) |>
    dplyr::filter(.data$latitude <= 65)

  gdf <- sf::st_as_sf(
    out,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  world <- rnaturalearth::ne_countries(
    scale = 110,
    returnclass = "sf"
  ) |>
    dplyr::filter(.data$continent != "Antarctica")

  robinson_crs <- paste(
    "+proj=robin",
    "+lon_0=0",
    "+x_0=0",
    "+y_0=0",
    "+datum=WGS84",
    "+units=m",
    "+no_defs"
  )

  gdf_proj <- sf::st_transform(gdf, robinson_crs)
  land_proj <- sf::st_transform(world, robinson_crs)

  land_combined <- land_proj |>
    sf::st_make_valid() |>
    sf::st_union()

  land_buffer <- sf::st_buffer(land_combined, 50000)

  is_over_land <- sf::st_within(
    gdf_proj,
    land_buffer,
    sparse = FALSE
  )

  out[!as.vector(is_over_land), , drop = FALSE]
}
