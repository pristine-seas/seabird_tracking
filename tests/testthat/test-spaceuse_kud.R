#Testing spaceuse functions
library(testthat)
library(sf)
library(ggplot2)

source("~/Desktop/Shearwater/R/spaceuse_kud.R")
# ------------------------------------------------------------------------------
# MOCK DATA FIXTURE FOR KUD TESTING
# ------------------------------------------------------------------------------
setup_mock_tracks <- function() {
  # Create a small grid of coordinates spanning two fake trips
  # Coordinates must have some dispersion so kernelUD doesn't crash on singular points
  df <- data.frame(
    trip_id = c(rep("trip_A", 5), rep("trip_B", 5)),
    lon     = c(0, 1, 2, 1, 0,  10, 11, 12, 11, 10),
    lat     = c(0, 0, 1, 2, 1,  10, 10, 11, 12, 11),
    time    = seq(from = Sys.time(), by = "30 min", length.out = 10)
  )
  # Coerce into an sf object (WGS84 projection)
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# ------------------------------------------------------------------------------
# TESTS FOR calculate_kud()
# ------------------------------------------------------------------------------
test_that("calculate_kud extracts valid EstUDm structures matching trip groups", {
  tracks <- setup_mock_tracks()

  # Run function
  kud_res <- calculate_kud(tracks, ref = "href")

  # Assert type matches adehabitatHR legacy infrastructure outputs
  expect_s3_class(kud_res, "estUDm")

  # Assert that it split data into exactly 2 trip profiles
  expect_equal(length(kud_res), 2)
  expect_setequal(names(kud_res), c("trip_A", "trip_B"))
})

test_that("calculate_kud errors gracefully when prerequisite trip_id is missing", {
  tracks <- setup_mock_tracks()
  tracks$trip_id <- NULL # Remove required column

  expect_error(calculate_kud(tracks))
})

# ------------------------------------------------------------------------------
# TESTS FOR get_isopleths()
# ------------------------------------------------------------------------------
test_that("get_isopleths translates KUD contours into spatial sf polygons", {
  tracks  <- setup_mock_tracks()
  kud_res <- calculate_kud(tracks, ref = "href")

  # Request 50% core and 95% home range boundaries
  levels_vec <- c(50, 95)
  polys <- get_isopleths(kud_res, levels = levels_vec)

  # Assert output format is an sf object
  expect_s3_class(polys, "sf")

  # Assert geometry types are polygon arrays
  geom_types <- unique(as.character(sf::st_geometry_type(polys)))
  expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))

  # Check rows: 2 trips x 2 levels = 4 resulting feature shapes
  expect_equal(nrow(polys), 4)
  expect_true("id" %in% names(polys)) # adehabitatHR stores trip identity in 'id'
})
