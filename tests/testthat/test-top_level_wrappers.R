library(testthat)
library(dplyr)
library(sf)
library(ggplot2)



# =============================================================================
# test-top_level_wrappers.R
# Tests only for functions that remain in top_level_wrappers.R
# =============================================================================
#
# Tests for repeated functions were moved to their dedicated test files:
#   - clean_tracks()              -> test-clean_tracks.R
#   - summarize_movement()        -> test-summarize_movement.R
#   - analyze_fisheries_overlap() -> test-analyze_fisheries_overlap.R
#   - export_spatial_outputs()    -> test-export_spatial_outputs.R


# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

make_track_df <- function() {
  data.frame(
    bird_id = c("bird_A", "bird_A", "bird_B"),
    track_id = c("bird_A", "bird_A", "bird_B"),
    Date = c("01/01/2026", "01/01/2026", "01/01/2026"),
    Time = c("10:00:00", "11:00:00", "10:30:00"),
    datetime_gmt = as.POSIXct(
      c("2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-01 10:30:00"),
      tz = "UTC"
    ),
    longitude = c(175.0, 175.2, 176.0),
    latitude = c(-20.0, -20.2, -21.0),
    trip_id = c("trip_1", "trip_1", "trip_2"),
    stringsAsFactors = FALSE
  )
}

make_track_sf <- function() {
  df <- make_track_df()
  sf::st_as_sf(
    df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
}

make_poly_sf <- function(value_col = TRUE) {
  p1 <- sf::st_polygon(list(rbind(
    c(174, -22),
    c(177, -22),
    c(177, -19),
    c(174, -19),
    c(174, -22)
  )))

  out <- sf::st_sf(
    iso3 = "USA",
    sovereign = "United States",
    eez_name = "US_EEZ",
    id = "poly_1",
    geometry = sf::st_sfc(p1, crs = 4326)
  )

  if (isTRUE(value_col)) {
    out$total_overlap <- 10
    out$level_pct <- 50
  }

  out
}


# ------------------------------------------------------------------------------
# estimate_space_use()
# ------------------------------------------------------------------------------

test_that("estimate_space_use validates file_path before running workflow", {
  expect_error(
    estimate_space_use(file_path = NA_character_),
    "single non-missing character string"
  )

  expect_error(
    estimate_space_use(file_path = c("a.csv", "b.csv")),
    "single non-missing character string"
  )

  expect_error(
    estimate_space_use(file_path = tempfile(fileext = ".csv")),
    "File does not exist"
  )
})

test_that("estimate_space_use validates numeric workflow arguments", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(make_track_df(), tmp, row.names = FALSE)

  expect_error(
    estimate_space_use(tmp, max_speed = -1),
    "max_speed"
  )

  expect_error(
    estimate_space_use(tmp, interval_minutes = 0),
    "interval_minutes"
  )

  expect_error(
    estimate_space_use(tmp, density_levels = c(0, 95)),
    "density_levels"
  )

  expect_error(
    estimate_space_use(tmp, density_levels = c(50, 100)),
    "density_levels"
  )
})

test_that("estimate_space_use validates colony coordinate structure", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(make_track_df(), tmp, row.names = FALSE)

  expect_error(
    estimate_space_use(tmp, colony_coords = c(x = 175, y = -20)),
    "colony_coords"
  )

  expect_error(
    estimate_space_use(tmp, colony_coords = c(lon = 175)),
    "colony_coords"
  )
})


# ------------------------------------------------------------------------------
# analyze_jurisdiction_overlap()
# ------------------------------------------------------------------------------

test_that("analyze_jurisdiction_overlap validates sf inputs", {
  track_sf <- make_track_sf()
  eez_sf <- make_poly_sf()

  expect_error(
    analyze_jurisdiction_overlap(data.frame(x = 1), eez_sf),
    "track_data"
  )

  expect_error(
    analyze_jurisdiction_overlap(track_sf, data.frame(x = 1)),
    "eez_layer"
  )

  expect_error(
    analyze_jurisdiction_overlap(track_sf, eez_sf, mpa_layer = data.frame(x = 1)),
    "mpa_layer"
  )

  expect_error(
    analyze_jurisdiction_overlap(track_sf, eez_sf, priority_layer = data.frame(x = 1)),
    "priority_layer"
  )
})

test_that("analyze_jurisdiction_overlap returns named workflow outputs when dependencies are stubbed", {
  track_sf <- make_track_sf()
  eez_sf <- make_poly_sf()

  old_overlay_eez_abnj <- if (exists("overlay_eez_abnj", mode = "function")) overlay_eez_abnj else NULL
  old_calc_time <- if (exists("calc_time_in_jurisdictions", mode = "function")) calc_time_in_jurisdictions else NULL
  old_calc_trans <- if (exists("calc_transboundary_movements", mode = "function")) calc_transboundary_movements else NULL

  overlay_eez_abnj <<- function(track_data, eez_layer) {
    track_data$jurisdiction <- "EEZ"
    track_data
  }

  calc_time_in_jurisdictions <<- function(track_data) {
    data.frame(
      bird_id = unique(track_data$track_id),
      jurisdiction = "EEZ",
      total_hours = 1,
      stringsAsFactors = FALSE
    )
  }

  calc_transboundary_movements <<- function(track_data) {
    data.frame(
      bird_id = unique(track_data$track_id),
      is_transboundary = FALSE,
      crossed_into_abnj = FALSE,
      stringsAsFactors = FALSE
    )
  }

  on.exit({
    if (is.null(old_overlay_eez_abnj)) {
      rm(overlay_eez_abnj, envir = .GlobalEnv)
    } else {
      overlay_eez_abnj <<- old_overlay_eez_abnj
    }

    if (is.null(old_calc_time)) {
      rm(calc_time_in_jurisdictions, envir = .GlobalEnv)
    } else {
      calc_time_in_jurisdictions <<- old_calc_time
    }

    if (is.null(old_calc_trans)) {
      rm(calc_transboundary_movements, envir = .GlobalEnv)
    } else {
      calc_transboundary_movements <<- old_calc_trans
    }
  }, add = TRUE)

  res <- analyze_jurisdiction_overlap(track_sf, eez_sf)

  expect_type(res, "list")
  expect_named(
    res,
    c(
      "labeled_tracks",
      "jurisdiction_summary",
      "mpa_tracks",
      "priority_tracks",
      "transboundary_summary",
      "policy_summary"
    )
  )
  expect_s3_class(res$labeled_tracks, "sf")
  expect_s3_class(res$jurisdiction_summary, "data.frame")
  expect_s3_class(res$transboundary_summary, "data.frame")
  expect_null(res$mpa_tracks)
  expect_null(res$priority_tracks)
  expect_null(res$policy_summary)
})


# ------------------------------------------------------------------------------
# plot_tracking_results()
# ------------------------------------------------------------------------------

test_that("plot_tracking_results returns an empty list when no inputs are supplied", {
  plots <- plot_tracking_results()

  expect_type(plots, "list")
  expect_equal(length(plots), 0)
})

test_that("plot_tracking_results validates colony coordinates", {
  expect_error(
    plot_tracking_results(track_data = make_track_df(), colony_coords = c(x = 1, y = 2)),
    "colony_coords"
  )
})

test_that("plot_tracking_results creates track and density plots from track data", {
  plots <- plot_tracking_results(track_data = make_track_df())

  expect_type(plots, "list")
  expect_named(plots, c("tracks", "density"))
  expect_s3_class(plots$tracks, "ggplot")
  expect_s3_class(plots$density, "ggplot")
})

test_that("plot_tracking_results creates trip, fisheries, and hotspot plots when inputs are supplied", {
  track_df <- make_track_df()
  fisheries_grid <- make_poly_sf()
  hotspot_polys <- make_poly_sf()

  plots <- plot_tracking_results(
    trip_data = track_df,
    fisheries_grid = fisheries_grid,
    isopleth_polygons = hotspot_polys
  )

  expect_type(plots, "list")
  expect_true(all(c("trips", "fisheries_heatmap", "hotspots") %in% names(plots)))
  expect_s3_class(plots$trips, "ggplot")
  expect_s3_class(plots$fisheries_heatmap, "ggplot")
  expect_s3_class(plots$hotspots, "ggplot")
})


# ------------------------------------------------------------------------------
# export_gis_layers()
# ------------------------------------------------------------------------------

test_that("export_gis_layers validates output path and spatial layer type", {
  layer <- make_poly_sf()

  expect_error(
    export_gis_layers(layer, file_path = NA_character_),
    "file_path"
  )

  expect_error(
    export_gis_layers(data.frame(x = 1), file_path = tempfile(fileext = ".gpkg")),
    "recognised spatial object"
  )
})

test_that("export_gis_layers writes sf objects and invisibly returns the file path", {
  layer <- make_poly_sf()
  out_path <- tempfile(fileext = ".gpkg")

  written <- export_gis_layers(layer, out_path)

  expect_equal(written, out_path)
  expect_true(file.exists(out_path))
})


# ------------------------------------------------------------------------------
# export_ud_polygons()
# ------------------------------------------------------------------------------

test_that("export_ud_polygons writes a single sf object", {
  ud <- make_poly_sf()
  out_path <- tempfile(fileext = ".gpkg")

  written <- export_ud_polygons(ud, out_path)

  expect_equal(written, out_path)
  expect_true(file.exists(out_path))
})

test_that("export_ud_polygons combines and writes a list of sf objects", {
  ud1 <- make_poly_sf()
  ud2 <- make_poly_sf()
  ud2$id <- "poly_2"
  out_path <- tempfile(fileext = ".gpkg")

  written <- export_ud_polygons(list(core = ud1, home = ud2), out_path)

  expect_equal(written, out_path)
  expect_true(file.exists(out_path))
})

test_that("export_ud_polygons errors when no sf polygons are supplied", {
  expect_error(
    export_ud_polygons(list(a = data.frame(x = 1)), tempfile(fileext = ".gpkg")),
    "ud_polys"
  )

  expect_error(
    export_ud_polygons(data.frame(x = 1), tempfile(fileext = ".gpkg")),
    "ud_polys"
  )
})
