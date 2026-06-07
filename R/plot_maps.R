# =============================================================================
# plot_maps.R
# Visualization helpers for tracks, trips, space use, fisheries, and policy
# =============================================================================

#' Plot a Fisheries Overlap or Risk Heatmap
#'
#' Maps the spatial pattern of fisheries overlap or risk intensity using an
#' `sf` grid or polygon layer.
#'
#' @param grid_data An `sf` object containing grid or polygon geometry and a
#'   value column to plot.
#' @param fill_col Character string. Name of the overlap or risk column to use
#'   for fill color. Default is `"total_overlap"`.
#' @param border_color Character string. Polygon border color. Default is
#'   `"grey40"`.
#' @param na_color Character string. Fill color for missing values. Default is
#'   `"white"`.
#' @param title Optional character string for the plot title.
#' @param legend_title Optional character string for the legend title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_fisheries_heatmap <- function(grid_data,
                                   fill_col = "total_overlap",
                                   border_color = "grey40",
                                   na_color = "white",
                                   title = NULL,
                                   legend_title = NULL) {
  if (!inherits(grid_data, "sf")) {
    stop("grid_data must be an sf object.", call. = FALSE)
  }

  if (!is.character(fill_col) || length(fill_col) != 1 || is.na(fill_col)) {
    stop("fill_col must be a single character string.", call. = FALSE)
  }

  if (!fill_col %in% names(grid_data)) {
    stop("fill_col not found in grid_data.", call. = FALSE)
  }

  fill_vals <- suppressWarnings(as.numeric(as.character(grid_data[[fill_col]])))

  if (length(fill_vals) > 0 &&
      all(is.na(fill_vals)) &&
      any(!is.na(grid_data[[fill_col]]))) {
    stop("fill_col must contain numeric or numeric-like values.", call. = FALSE)
  }

  grid_data[[fill_col]] <- fill_vals

  if (is.null(legend_title)) {
    legend_title <- fill_col
  }

  ggplot2::ggplot(grid_data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_col]]),
      color = border_color
    ) +
    ggplot2::scale_fill_gradient(
      name = legend_title,
      low = "lightyellow",
      high = "red",
      na.value = na_color
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title)
}


#' Plot Bird Tracks on a Map
#'
#' Creates a map of GPS tracks for one or more individuals.
#'
#' @param track_data A data frame or tibble containing longitude and latitude
#'   columns.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param color_by Character. Column used to color tracks. Default is
#'   `"track_id"`.
#' @param colony_coords Optional named numeric vector with `lon` and `lat`.
#' @param title Character. Plot title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_tracks <- function(track_data,
                        lon_col = "longitude",
                        lat_col = "latitude",
                        color_by = "track_id",
                        colony_coords = NULL,
                        title = "Bird Tracks") {
  if (!is.data.frame(track_data)) {
    stop("track_data must be a data frame or tibble.", call. = FALSE)
  }

  .check_track_cols(track_data, c(lon_col, lat_col, color_by))

  if (!is.null(colony_coords)) {
    .check_colony_coords(colony_coords)
  }

  p <- ggplot2::ggplot(
    track_data,
    ggplot2::aes(
      x = .data[[lon_col]],
      y = .data[[lat_col]],
      color = .data[[color_by]],
      group = .data[[color_by]]
    )
  ) +
    ggplot2::geom_path(alpha = 0.6, linewidth = 0.4) +
    ggplot2::geom_point(size = 0.8, alpha = 0.4) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = title,
      x = "Longitude",
      y = "Latitude",
      color = color_by
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (!is.null(colony_coords)) {
    colony_df <- data.frame(
      lon = colony_coords[["lon"]],
      lat = colony_coords[["lat"]]
    )

    p <- p +
      ggplot2::geom_point(
        data = colony_df,
        ggplot2::aes(x = .data$lon, y = .data$lat),
        inherit.aes = FALSE,
        shape = 23,
        size = 4,
        fill = "red",
        color = "black"
      ) +
      ggplot2::annotate(
        "text",
        x = colony_coords[["lon"]],
        y = colony_coords[["lat"]] + 0.3,
        label = "Colony",
        size = 3
      )
  }

  p
}


#' Plot One or More Trips with Colony Context
#'
#' @param trip_data A track data frame containing a trip ID column.
#' @param trip_ids Character, integer, or numeric vector of trip IDs to plot.
#'   If `NULL`, all trips are shown.
#' @param colony_coords Optional named numeric vector with `lon` and `lat`.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param trip_id_col Character. Trip ID column. Default is `"trip_id"`.
#' @param color_by Character. Column to color-code trips by. Default is
#'   `"trip_id"`.
#' @param title Character. Plot title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_trip_map <- function(trip_data,
                          trip_ids = NULL,
                          colony_coords = NULL,
                          lon_col = "longitude",
                          lat_col = "latitude",
                          trip_id_col = "trip_id",
                          color_by = "trip_id",
                          title = "Trip Map") {
  if (!is.data.frame(trip_data)) {
    stop("trip_data must be a data frame or tibble.", call. = FALSE)
  }

  .check_track_cols(trip_data, c(lon_col, lat_col, trip_id_col, color_by))

  if (!is.null(colony_coords)) {
    .check_colony_coords(colony_coords)
  }

  if (!is.null(trip_ids)) {
    trip_data <- trip_data[trip_data[[trip_id_col]] %in% trip_ids, , drop = FALSE]

    if (nrow(trip_data) == 0) {
      stop("No rows found for the supplied trip_ids.", call. = FALSE)
    }
  }

  p <- ggplot2::ggplot(
    trip_data,
    ggplot2::aes(
      x = .data[[lon_col]],
      y = .data[[lat_col]],
      color = as.factor(.data[[color_by]]),
      group = as.factor(.data[[color_by]])
    )
  ) +
    ggplot2::geom_path(linewidth = 0.5, alpha = 0.8) +
    ggplot2::geom_point(size = 1, alpha = 0.5) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = title,
      x = "Longitude",
      y = "Latitude",
      color = color_by
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (!is.null(colony_coords)) {
    colony_df <- data.frame(
      lon = colony_coords[["lon"]],
      lat = colony_coords[["lat"]]
    )

    p <- p +
      ggplot2::geom_point(
        data = colony_df,
        ggplot2::aes(x = .data$lon, y = .data$lat),
        inherit.aes = FALSE,
        shape = 23,
        size = 5,
        fill = "red",
        color = "black"
      ) +
      ggplot2::annotate(
        "text",
        x = colony_coords[["lon"]],
        y = colony_coords[["lat"]] + 0.3,
        label = "Colony",
        size = 3
      )
  }

  p
}


#' Plot Spatial Density of Seabird Use
#'
#' @param track_data A standardized or cleaned track data frame.
#' @param lon_col Character. Longitude column. Default is `"longitude"`.
#' @param lat_col Character. Latitude column. Default is `"latitude"`.
#' @param bins Integer. Number of 2D histogram bins per axis. Default is `100`.
#' @param title Character. Plot title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_density_map <- function(track_data,
                             lon_col = "longitude",
                             lat_col = "latitude",
                             bins = 100,
                             title = "Seabird Use Density") {
  if (!is.data.frame(track_data)) {
    stop("track_data must be a data frame or tibble.", call. = FALSE)
  }

  .check_track_cols(track_data, c(lon_col, lat_col))

  if (!is.numeric(bins) || length(bins) != 1 || is.na(bins) || bins <= 0) {
    stop("bins must be a single positive number.", call. = FALSE)
  }

  ggplot2::ggplot(
    track_data,
    ggplot2::aes(x = .data[[lon_col]], y = .data[[lat_col]])
  ) +
    ggplot2::stat_bin_2d(
      bins = bins,
      ggplot2::aes(fill = ggplot2::after_stat(count))
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      name = "Fix count"
    ) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = title,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


#' Plot Home Range and Core Space-Use Hotspots
#'
#' @param sf_polys An `sf` polygon dataset generated by space-use functions.
#' @param fill_col Character. Column used to fill polygons.
#' @param title Character. Plot title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_core_use_hotspots <- function(sf_polys,
                                   fill_col = NULL,
                                   title = "Seabird Space-Use Hotspots") {
  if (!inherits(sf_polys, "sf")) {
    stop("sf_polys must be an sf object.", call. = FALSE)
  }

  if (is.null(fill_col)) {
    fill_col <- if ("area_type" %in% names(sf_polys)) {
      "area_type"
    } else if ("level_pct" %in% names(sf_polys)) {
      "level_pct"
    } else if ("id" %in% names(sf_polys)) {
      "id"
    } else {
      names(sf_polys)[1]
    }
  }

  if (!fill_col %in% names(sf_polys)) {
    stop("fill_col not found in sf_polys.", call. = FALSE)
  }

  ggplot2::ggplot(sf_polys) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = as.factor(.data[[fill_col]])),
      alpha = 0.4,
      color = "black"
    ) +
    ggplot2::scale_fill_viridis_d(
      option = "viridis",
      name = fill_col
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Computed Core Areas and Range Extents",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right"
    )
}


#' Plot Home Range and Core Space-Use Hotspots
#'
#' Backward-compatible wrapper for `plot_core_use_hotspots()`.
#'
#' @param sf_polys An `sf` polygon dataset.
#' @param title Character. Plot title.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_hotspot_map <- function(sf_polys,
                             title = "Seabird Space-Use Hotspots") {
  plot_core_use_hotspots(sf_polys = sf_polys, title = title)
}


#' Plot Jurisdiction Summary
#'
#' Creates a bar plot from jurisdiction or policy exposure summaries.
#'
#' @param summary_data A data frame.
#' @param x_col Character. X-axis column. Default is `"jurisdiction"`.
#' @param y_col Character. Numeric y-axis column. Default is `"total_hours"`.
#' @param fill_col Optional fill column.
#' @param facet_col Optional facet column.
#' @param title Character. Plot title.
#' @param x_label Character. X-axis label.
#' @param y_label Character. Y-axis label.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_jurisdiction_summary <- function(summary_data,
                                      x_col = "jurisdiction",
                                      y_col = "total_hours",
                                      fill_col = NULL,
                                      facet_col = NULL,
                                      title = "Jurisdiction Exposure Summary",
                                      x_label = NULL,
                                      y_label = NULL) {
  if (!is.data.frame(summary_data)) {
    stop("summary_data must be a data frame or tibble.", call. = FALSE)
  }

  required_cols <- c(x_col, y_col)
  missing_cols <- setdiff(required_cols, names(summary_data))

  if (length(missing_cols) > 0) {
    stop(
      "missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(fill_col) && !fill_col %in% names(summary_data)) {
    stop("fill_col not found in summary_data.", call. = FALSE)
  }

  if (!is.null(facet_col) && !facet_col %in% names(summary_data)) {
    stop("facet_col not found in summary_data.", call. = FALSE)
  }

  y_vals <- suppressWarnings(as.numeric(as.character(summary_data[[y_col]])))

  if (length(y_vals) > 0 &&
      all(is.na(y_vals)) &&
      any(!is.na(summary_data[[y_col]]))) {
    stop("y_col must contain numeric or numeric-like values.", call. = FALSE)
  }

  summary_data[[y_col]] <- y_vals

  if (is.null(x_label)) {
    x_label <- x_col
  }

  if (is.null(y_label)) {
    y_label <- y_col
  }

  p <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      fill = if (is.null(fill_col)) .data[[x_col]] else .data[[fill_col]]
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = if (is.null(fill_col)) x_col else fill_col
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)))
  }

  p
}


# Internal helper: check required columns
.check_track_cols <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# Internal helper: validate colony coordinates
.check_colony_coords <- function(colony_coords) {
  if (!is.numeric(colony_coords) ||
      !all(c("lon", "lat") %in% names(colony_coords))) {
    stop(
      "colony_coords must be a named numeric vector with names lon and lat.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
