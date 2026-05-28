library(testthat)
library(ggplot2)
library(sf)

test_that("plot_fisheries_heatmap creates a ggplot from sf object", {
  # Create a dummy sf polygon grid
  p1 <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  grid_sf <- st_sf(total_overlap = 10, geometry = st_sfc(p1))
  
  p <- plot_fisheries_heatmap(grid_sf)
  expect_s3_class(p, "ggplot")
  
  # Test assertions
  expect_error(plot_fisheries_heatmap(data.frame(x=1)), "must be an sf object")
  expect_error(plot_fisheries_heatmap(grid_sf, fill_col = "missing"), "not found in grid_data")
})

test_that("plot_tracks successfully generates a map", {
  df <- data.frame(lon = c(175, 176), lat = c(-20, -21), bird_id = c("A", "A"))
  
  # Basic plot
  p <- plot_tracks(df)
  expect_s3_class(p, "ggplot")
  
  # Plot with colony coordinates
  p2 <- plot_tracks(df, colony_coords = c(lon = 175.5, lat = -20.5))
  expect_s3_class(p2, "ggplot")
  
  # Test missing column error
  expect_error(plot_tracks(data.frame(x=1, y=2)), "missing required columns")
})

test_that("plot_trip_map filters and plots trips", {
  df <- data.frame(lon = c(175, 176, 177), lat = c(-20, -21, -22), trip_id = c(1, 1, 2))
  
  # Plot specific trip
  p <- plot_trip_map(df, trip_ids = 1)
  expect_s3_class(p, "ggplot")
  
  # Expect error if trip doesn't exist
  expect_error(plot_trip_map(df, trip_ids = 99), "No rows found")
})

test_that("plot_density_map generates a valid plot", {
  df <- data.frame(lon = runif(100, 170, 175), lat = runif(100, -25, -20))
  p <- plot_density_map(df)
  expect_s3_class(p, "ggplot")
})