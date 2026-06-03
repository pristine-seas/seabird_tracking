#' Calculate Kernel Utilization Distributions
#'
#' Converts spatial tracking features into a legacy `SpatialPointsDataFrame`
#' object and estimates fixed kernel utilization distributions grouped by
#' `trip_id` using `adehabitatHR`.
#'
#' @param tracks An `sf` spatial object containing GPS point locations. Must
#'   include a `trip_id` column and valid point geometry.
#' @param ref Character or numeric. The smoothing bandwidth parameter selection
#'   method. Common options are `"href"` for ad-hoc bandwidth selection or
#'   `"LSCV"` for least-squares cross-validation. Default is `"href"`.
#'
#' @return An `EstUDm` object containing calculated utilization distributions
#'   for each unique `trip_id`.
#'
#' @export
calculate_kud <- function(tracks, ref = "href") {
  if (!inherits(tracks, "sf")) {
    stop("`tracks` must be an sf object.")
  }

  if (!"trip_id" %in% names(tracks)) {
    stop("`tracks` must contain a `trip_id` column.")
  }

  if (any(sf::st_is_empty(tracks))) {
    stop("`tracks` contains empty geometries.")
  }

  coords <- sf::st_coordinates(tracks)

  track_data <- sf::st_drop_geometry(tracks)

  sp_df <- sp::SpatialPointsDataFrame(
    coords = coords,
    data = as.data.frame(track_data),
    proj4string = sp::CRS(sf::st_crs(tracks)$wkt)
  )

  kud <- adehabitatHR::kernelUD(
    sp_df[, "trip_id"],
    h = ref
  )

  kud
}


#' Extract Home Range Isopleth Polygons
#'
#' Extracts home range or core-use polygons from calculated kernel utilization
#' distributions at user-defined utilization percentage thresholds.
#'
#' @param kud An `EstUDm` object produced by `calculate_kud()`.
#' @param levels Numeric vector. Utilization distribution thresholds to extract,
#'   such as `c(50, 95)`.
#'
#' @return An `sf` data frame containing isopleth polygon geometries and a
#'   `level_pct` column identifying the utilization threshold.
#'
#' @export
get_isopleths <- function(kud, levels = c(50, 95)) {
  if (!inherits(kud, "EstUDm")) {
    stop("`kud` must be an EstUDm object produced by `calculate_kud()`.")
  }

  if (!is.numeric(levels) || length(levels) < 1) {
    stop("`levels` must be a numeric vector.")
  }

  if (any(levels <= 0 | levels >= 100)) {
    stop("`levels` must contain values greater than 0 and less than 100.")
  }

  poly_list <- lapply(levels, function(lvl) {
    polys_at_lvl <- adehabitatHR::getverticeshr(
      kud,
      percent = lvl
    )

    sf_at_lvl <- sf::st_as_sf(polys_at_lvl)

    sf_at_lvl$level_pct <- as.character(lvl)

    sf_at_lvl
  })

  sf_polys <- dplyr::bind_rows(poly_list)

  sf_polys
}

