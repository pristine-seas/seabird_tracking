#' Calculate Kernel Utilization Distributions (KUD)
#'
#' Converts spatial tracking features into a legacy SpatialPoints object to estimate
#' fixed kernel utilization distributions grouped by unique trip sequences using adehabitatHR.
#'
#' @param tracks An \code{sf} spatial object containing coordinate locations. Must include
#'   a \code{trip_id} column group string and a valid spatial geometry attribute.
#' @param ref Character or Numeric. The smoothing bandwidth parameter selection method.
#'   Options include \code{"href"} (ad-hoc bandwidth selection) or \code{"LSCV"}
#'   (least-squares cross-validation). Default is \code{"href"}.
#' @return A \code{EstUDm} object containing calculated utilization distributions
#'   for each unique \code{trip_id}.
#' @export
#'
#'
#' #' Extract Home Range Isopleth Polygons
#'
#' Extracts boundary contour vertices from calculated kernel distributions at
#' user-defined cumulative utilization percentage thresholds and returns them as spatial polygons.
#'
#' @param kud An \code{EstUDm} object resulting from \code{calculate_kud()}.
#' @param levels Numeric vector. Cumulative percentage utilization distribution thresholds
#'   representing core vs. total range boundaries (e.g., \code{50} for core foraging areas,
#'   \code{95} for total seasonal grounds). Default is \code{c(50, 95)}.
#'
#' @return An \code{sf} data frame containing polygon shapes matching the designated percentage fields.
#' @export

library(adehabitatHR)
library(sf)
library(sp)


calculate_kud <- function(tracks, ref = "href") {
  # Convert sf to SpatialPointsDataFrame for adehabitatHR
  coords <- sf::st_coordinates(tracks)
  sp_df <- sp::SpatialPointsDataFrame(coords, data = as.data.frame(tracks))
  # Calculate KUD
  kud <- adehabitatHR::kernelUD(sp_df[, "trip_id"], h = ref)
  return(kud)
}


get_isopleths <- function(kud, levels = c(50, 95)) {
  # Extract polygons at specified levels
  polys <- adehabitatHR::getverticeshr(kud, percent = levels)

  sf_polys <- sf::st_as_sf(polys)
  return(sf_polys)
}


