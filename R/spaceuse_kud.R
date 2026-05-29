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
#' Extract Home Range Isopleth Polygons
#'
#' Extracts boundary contour vertices from calculated kernel distributions at
#' user-defined cumulative utilization percentage thresholds, looping safely
#' over multiple levels.
#'
#' @param kud An \code{EstUDm} object resulting from \code{calculate_kud()}.
#' @param levels Numeric vector. Cumulative percentage utilization distribution thresholds
#'   representing core vs. total range boundaries (e.g., \code{50} and \code{95}).
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
  # Loop over each requested percentage level individually since adehabitatHR
  # cannot handle vectors of length > 1 for multi-track (EstUDm) objects.
  poly_list <- lapply(levels, function(lvl) {
    # Extract the polygon shapes for all trips at this single level
    polys_at_lvl <- adehabitatHR::getverticeshr(kud, percent = lvl)

    # Coerce to sf format
    sf_at_lvl <- sf::st_as_sf(polys_at_lvl)

    # Add a column indicating which threshold level this row represents
    # This is critical for downstream plotting (Person 6) and policy reporting (Person 9)
    sf_at_lvl$level_pct <- as.character(lvl)
    return(sf_at_lvl)
  })

  # Combine all individual level layers into a single clean sf data frame
  sf_polys <- dplyr::bind_rows(poly_list)
  return(sf_polys)
}


