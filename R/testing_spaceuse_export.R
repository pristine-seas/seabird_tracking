#Testing_spaceuse_export
install.packages(c("class", "sf", "e1071"), lib = .libPaths()[1], dependencies = TRUE)

library(class)
library(sf)
source("~/Desktop/Shearwater/R/spaceuse_export.R")

# ------------------------------------------------------------------------------
# MOCK DATA FIXTURE FOR EXPORT TESTING
# ------------------------------------------------------------------------------
setup_mock_export_data <- function() {
  # Two coordinates precisely 1 degree of latitude apart on the Equator
  # 1 degree of latitude on the equator = ~111,320 meters
  df <- data.frame(
    trip_id = "trip_A",
    time    = as.POSIXct(c("2026-05-21 12:00:00", "2026-05-21 15:00:00"), tz = "UTC"),
    lon     = c(0, 0),
    lat     = c(0, 1)
  )
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# ------------------------------------------------------------------------------
# TESTS FOR calculate_trip_stats()
# ------------------------------------------------------------------------------
test_that("calculate_trip_stats computes exact time gaps and colony distances", {
  tracks <- setup_mock_export_data()
  colony <- c(lon = 0, lat = 0) # Place colony directly at point 1

  res <- calculate_trip_stats(tracks, colony_coords = colony)

  # 1. Assert new descriptive features are generated
  expect_true(all(c("duration_hrs", "dist_to_colony_m", "max_dist_km") %in% names(res)))

  # 2. Assert Time Math: 12:00 to 15:00 is exactly 3 hours
  expect_equal(unique(res$duration_hrs), 3)

  # 3. Assert Distance Math: Point 1 should be 0 meters away from colony
  expect_equal(res$dist_to_colony_m[1], 0, tolerance = 1e-3)

  # 4. Assert Max distance matches expected equatorial distance bounds (~111.3 km)
  expect_equal(unique(res$max_dist_km), 111.32, tolerance = 0.5)
})

# ------------------------------------------------------------------------------
# TESTS FOR calculate_area_metrics()
# ------------------------------------------------------------------------------
test_that("calculate_area_metrics appends precise square kilometer boundaries", {
  # Create a planar mock box (e.g., using meters via UTM CRS 32630)
  # A square box 2000m x 2000m = 4,000,000 square meters = 4 square kilometers
  wkt_str <- "POLYGON((0 0, 2000 0, 2000 2000, 0 2000, 0 0))"

  mock_df <- data.frame(
    id  = "trip_A",
    wkt = wkt_str
  )

  mock_poly <- sf::st_as_sf(mock_df, wkt = "wkt", crs = 32630)

  res_poly <- calculate_area_metrics(mock_poly)

  expect_true("area_km2" %in% names(res_poly))
  expect_equal(res_poly$area_km2, 4.0, tolerance = 1e-5)
})

# ------------------------------------------------------------------------------
# TESTS FOR SHIPPED HOTSPOT PLOTTING (Visualization Module Goal)
# ------------------------------------------------------------------------------
test_that("plot_hotspot_map compiles functional ggplot visualization objects", {
  # Build a mock sf polygon input mimicking output bounds
  wkt_str <- "POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))"


  mock_df <- data.frame(
    id  = c("50", "95"),
    wkt = c(wkt_str, wkt_str) # Provide a shape string for each row
  )

  mock_polys <- sf::st_as_sf(mock_df, wkt = "wkt", crs = 4326)

  # Call your visualizer code
  plt <- plot_hotspot_map(sf_polys = mock_polys, title = "Test Range Map")

  # Assert it returns a fully qualified ggplot structure
  expect_s3_class(plt, "ggplot")

  # Assert structural elements inside ggplot match configuration targets
  expect_equal(plt$labels$title, "Test Range Map")
})
