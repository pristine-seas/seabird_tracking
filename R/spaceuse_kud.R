# =============================================================================
# spaceuse_kud.R
# Kernel utilization distribution and space-use polygon utilities
# =============================================================================

#' Estimate Kernel Utilization Distributions
#'
#' Converts `sf` tracking points to a legacy `SpatialPointsDataFrame` and
#' estimates fixed kernel utilization distributions grouped by a trip or bird ID.
#'
#' @param tracks An `sf` point object containing GPS locations.
#' @param id_col Character. Column used to group utilization distributions.
#'   Default is `"trip_id"`.
#' @param ref Character or numeric. Bandwidth selection method or value passed
#'   to `adehabitatHR::kernelUD()`. Common options are `"href"` and `"LSCV"`.
#' @param ... Additional arguments passed to `adehabitatHR::kernelUD()`.
#'
#' @return An `EstUDm` object containing utilization distributions.
#' @export
estimate_kernel_ud <- function(tracks,
                               id_col = "trip_id",
                               ref = "href",
                               ...) {
  if (!inherits(tracks, "sf")) {
    stop("`tracks` must be an sf object.", call. = FALSE)
  }

  if (!id_col %in% names(tracks)) {
    stop("`tracks` must contain the ID column `", id_col, "`.", call. = FALSE)
  }

  if (any(sf::st_is_empty(tracks))) {
    stop("`tracks` contains empty geometries.", call. = FALSE)
  }

  geom_type <- unique(as.character(sf::st_geometry_type(tracks)))
  if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) {
    stop("`tracks` must contain point geometries.", call. = FALSE)
  }

  if (is.na(sf::st_crs(tracks))) {
    stop("`tracks` must have a coordinate reference system.", call. = FALSE)
  }

  coords <- sf::st_coordinates(tracks)

  if (ncol(coords) > 2) {
    coords <- coords[, c("X", "Y"), drop = FALSE]
  }

  track_data <- sf::st_drop_geometry(tracks)
  ids <- track_data[[id_col]]

  if (any(is.na(ids))) {
    warning("Rows with missing `", id_col, "` were removed before KUD estimation.", call. = FALSE)
    keep <- !is.na(ids)
    coords <- coords[keep, , drop = FALSE]
    track_data <- track_data[keep, , drop = FALSE]
  }

  if (nrow(track_data) == 0) {
    stop("No non-missing rows remain for KUD estimation.", call. = FALSE)
  }

  sp_df <- sp::SpatialPointsDataFrame(
    coords = coords,
    data = as.data.frame(track_data),
    proj4string = sp::CRS(sf::st_crs(tracks)$wkt)
  )

  adehabitatHR::kernelUD(sp_df[, id_col], h = ref, ...)
}

#' Calculate Kernel Utilization Distributions
#'
#' Backward-compatible wrapper for `estimate_kernel_ud()`.
#'
#' @param tracks An `sf` point object containing GPS locations.
#' @param ref Bandwidth selection method or value.
#'
#' @return An `EstUDm` object.
#' @export
calculate_kud <- function(tracks, ref = "href") {
  estimate_kernel_ud(tracks = tracks, id_col = "trip_id", ref = ref)
}

#' Extract Home Range Isopleth Polygons
#'
#' Extracts utilization distribution polygons at one or more percentage levels.
#'
#' @param kud An `EstUDm` object produced by `estimate_kernel_ud()`.
#' @param levels Numeric vector of utilization distribution percentages, such as
#'   `c(50, 95)`.
#'
#' @return An `sf` polygon object with a `level_pct` column.
#' @export
get_isopleths <- function(kud, levels = c(50, 95)) {
  if (!inherits(kud, "EstUDm") && !inherits(kud, "estUDm")) {
    stop("`kud` must be an EstUDm object produced by `estimate_kernel_ud()`.", call. = FALSE)
  }

  if (!is.numeric(levels) || length(levels) < 1) {
    stop("`levels` must be a numeric vector.", call. = FALSE)
  }

  if (any(is.na(levels)) || any(levels <= 0 | levels >= 100)) {
    stop("`levels` must contain values greater than 0 and less than 100.", call. = FALSE)
  }

  poly_list <- lapply(levels, function(lvl) {
    polys_at_lvl <- adehabitatHR::getverticeshr(kud, percent = lvl)
    sf_at_lvl <- sf::st_as_sf(polys_at_lvl)
    sf_at_lvl$level_pct <- as.numeric(lvl)
    sf_at_lvl
  })

  dplyr::bind_rows(poly_list)
}

#' Extract Core-Use Area Polygons
#'
#' Extracts the core-use utilization distribution contour, usually the 50%
#' isopleth.
#'
#' @param kud An `EstUDm` object produced by `estimate_kernel_ud()`.
#' @param level Numeric. Core-use isopleth percentage. Default is `50`.
#'
#' @return An `sf` polygon object with `level_pct` and `area_type` columns.
#' @export
get_core_area <- function(kud, level = 50) {
  out <- get_isopleths(kud, levels = level)
  out$area_type <- "core_area"
  out
}

#' Extract Home-Range Polygons
#'
#' Extracts the home-range utilization distribution contour, usually the 95%
#' isopleth.
#'
#' @param kud An `EstUDm` object produced by `estimate_kernel_ud()`.
#' @param level Numeric. Home-range isopleth percentage. Default is `95`.
#'
#' @return An `sf` polygon object with `level_pct` and `area_type` columns.
#' @export
get_home_range <- function(kud, level = 95) {
  out <- get_isopleths(kud, levels = level)
  out$area_type <- "home_range"
  out
}

#' Calculate Utilization Distribution Overlap
#'
#' Calculates spatial overlap between two utilization-distribution polygon
#' layers, such as core areas or home ranges.
#'
#' @param x An `sf` polygon layer.
#' @param y An `sf` polygon layer.
#' @param by Optional character vector of shared grouping columns. If supplied,
#'   overlap is calculated within matching groups.
#' @param area_crs Projected CRS used for area calculations. Default is `3857`.
#'
#' @return A data frame with overlap area and percentage metrics.
#' @export
calc_ud_overlap <- function(x,
                            y,
                            by = NULL,
                            area_crs = 3857) {
  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("`x` and `y` must both be sf objects.", call. = FALSE)
  }

  if (is.na(sf::st_crs(x)) || is.na(sf::st_crs(y))) {
    stop("Both `x` and `y` must have a CRS.", call. = FALSE)
  }

  if (sf::st_crs(x) != sf::st_crs(y)) {
    y <- sf::st_transform(y, sf::st_crs(x))
  }

  if (!is.null(by)) {
    if (!is.character(by) || length(by) < 1) {
      stop("`by` must be NULL or a character vector of shared column names.", call. = FALSE)
    }

    missing_x <- setdiff(by, names(x))
    missing_y <- setdiff(by, names(y))

    if (length(missing_x) > 0 || length(missing_y) > 0) {
      stop("All `by` columns must exist in both `x` and `y`.", call. = FALSE)
    }
  }

  x_proj <- sf::st_transform(x, area_crs)
  y_proj <- sf::st_transform(y, area_crs)

  if (is.null(by)) {
    inter <- suppressWarnings(sf::st_intersection(x_proj, y_proj))

    overlap_area_km2 <- if (nrow(inter) == 0) {
      0
    } else {
      sum(as.numeric(sf::st_area(inter)), na.rm = TRUE) / 1e6
    }

    x_area_km2 <- sum(as.numeric(sf::st_area(x_proj)), na.rm = TRUE) / 1e6
    y_area_km2 <- sum(as.numeric(sf::st_area(y_proj)), na.rm = TRUE) / 1e6

    return(data.frame(
      overlap_area_km2 = overlap_area_km2,
      x_area_km2 = x_area_km2,
      y_area_km2 = y_area_km2,
      pct_x_overlap = if (x_area_km2 > 0) 100 * overlap_area_km2 / x_area_km2 else NA_real_,
      pct_y_overlap = if (y_area_km2 > 0) 100 * overlap_area_km2 / y_area_km2 else NA_real_
    ))
  }

  keys <- unique(rbind(
    sf::st_drop_geometry(x_proj)[by],
    sf::st_drop_geometry(y_proj)[by]
  ))

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]

    keep_x <- Reduce(`&`, Map(function(col) x_proj[[col]] == key[[col]], by))
    keep_y <- Reduce(`&`, Map(function(col) y_proj[[col]] == key[[col]], by))

    x_i <- x_proj[keep_x, , drop = FALSE]
    y_i <- y_proj[keep_y, , drop = FALSE]

    if (nrow(x_i) == 0 || nrow(y_i) == 0) {
      overlap_area_km2 <- 0
    } else {
      inter <- suppressWarnings(sf::st_intersection(x_i, y_i))
      overlap_area_km2 <- if (nrow(inter) == 0) 0 else sum(as.numeric(sf::st_area(inter)), na.rm = TRUE) / 1e6
    }

    x_area_km2 <- if (nrow(x_i) == 0) 0 else sum(as.numeric(sf::st_area(x_i)), na.rm = TRUE) / 1e6
    y_area_km2 <- if (nrow(y_i) == 0) 0 else sum(as.numeric(sf::st_area(y_i)), na.rm = TRUE) / 1e6

    cbind(
      key,
      data.frame(
        overlap_area_km2 = overlap_area_km2,
        x_area_km2 = x_area_km2,
        y_area_km2 = y_area_km2,
        pct_x_overlap = if (x_area_km2 > 0) 100 * overlap_area_km2 / x_area_km2 else NA_real_,
        pct_y_overlap = if (y_area_km2 > 0) 100 * overlap_area_km2 / y_area_km2 else NA_real_
      )
    )
  })

  dplyr::bind_rows(rows)
}

#' Calculate Spatial Polygon Surface Coverage Areas
#'
#' Evaluates polygon areas and appends area values in square kilometers.
#'
#' @param sf_polys An `sf` polygon layer.
#'
#' @return The original `sf` object with an `area_km2` column.
#' @export
calculate_area_metrics <- function(sf_polys) {
  if (!inherits(sf_polys, "sf")) {
    stop("`sf_polys` must be an sf object.", call. = FALSE)
  }

  sf_polys$area_km2 <- as.numeric(sf::st_area(sf_polys)) / 1e6
  sf_polys
}

#' Extract Spatial Centroids from Area Features
#'
#' Computes geographic centroids for polygon or contour features.
#'
#' @param sf_polys An `sf` spatial feature collection.
#'
#' @return An `sf` point object representing feature centroids.
#' @export
get_spatial_centroids <- function(sf_polys) {
  if (!inherits(sf_polys, "sf")) {
    stop("`sf_polys` must be an sf object.", call. = FALSE)
  }

  sf::st_centroid(sf_polys)
}

#' Export Space-Use Layers
#'
#' Exports an `sf` spatial object as a GeoPackage file.
#'
#' @param sf_obj An `sf` spatial object to export.
#' @param filename Character. Output filename without the `.gpkg` extension.
#'
#' @return Invisibly returns the written path.
#' @export
export_spaceuse_layers <- function(sf_obj, filename) {
  if (!inherits(sf_obj, "sf")) {
    stop("`sf_obj` must be an sf object.", call. = FALSE)
  }

  if (!is.character(filename) || length(filename) != 1 || is.na(filename) || filename == "") {
    stop("`filename` must be a single non-empty character string.", call. = FALSE)
  }

  out_path <- paste0(filename, ".gpkg")

  sf::st_write(sf_obj, dsn = out_path, append = FALSE, quiet = TRUE)
  invisible(out_path)
}
