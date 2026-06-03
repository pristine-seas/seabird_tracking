library(testthat)
library(sf)
library(dplyr)

make_square <- function(xmin, ymin, xmax, ymax, crs = 4326) {
  sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(xmin, ymin),
      c(xmin, ymax),
      c(xmax, ymax),
      c(xmax, ymin),
      c(xmin, ymin)
    )))),
    crs = crs
  )
}

make_point_sf <- function(coords, crs = 4326) {
  sf::st_sf(
    geometry = sf::st_sfc(lapply(seq_len(nrow(coords)), function(i) sf::st_point(coords[i, ]))),
    crs = crs
  )
}

test_that("read_management_layers reads and annotates a layer", {
  skip_if_not_installed("sf")

  tmpdir <- tempfile("layers_")
  dir.create(tmpdir)

  eez <- sf::st_sf(
    iso3 = "USA",
    sovereign = "United States",
    eez_name = "US_EEZ",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  eez_path <- file.path(tmpdir, "eez.gpkg")
  sf::st_write(eez, eez_path, quiet = TRUE)

  layers <- read_management_layers(eez_path = eez_path)

  expect_s3_class(layers, "management_layers")
  expect_true("eez" %in% names(layers))
  expect_s3_class(layers$eez, "sf")
  expect_equal(attr(layers, "n_layers"), 1)
  expect_equal(attr(layers$eez, "source_path"), eez_path)
  expect_equal(attr(layers$eez, "n_features"), 1)
  expect_equal(layers$eez$eez_name[1], "US_EEZ")
})

test_that("read_management_layers errors when file is missing", {
  expect_error(
    read_management_layers(eez_path = "does_not_exist.gpkg"),
    "EEZ file not found"
  )
})

test_that("print.management_layers prints a summary", {
  eez <- sf::st_sf(
    iso3 = "USA",
    sovereign = "United States",
    eez_name = "US_EEZ",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  layers <- list(eez = eez)
  class(layers) <- c("management_layers", "list")
  attr(layers, "target_crs") <- 4326
  attr(layers, "n_layers") <- 1

  expect_output(print(layers), "Management Layers")
  expect_output(print(layers), "EEZ")
})

test_that("overlay_eez_abnj assigns EEZ and ABNJ correctly", {
  eez <- sf::st_sf(
    iso3 = "USA",
    sovereign = "United States",
    eez_name = "US_EEZ",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  pts <- sf::st_sf(
    bird_id = c("A", "A"),
    Date = c("01/01/2020", "01/01/2020"),
    Time = c("00:00:00", "01:00:00"),
    geometry = sf::st_sfc(
      sf::st_point(c(5, 5)),
      sf::st_point(c(20, 20))
    ),
    crs = 4326
  )

  out <- overlay_eez_abnj(pts, eez)

  expect_equal(out$jurisdiction, c("US_EEZ", "ABNJ"))
  expect_equal(out$iso3[1], "USA")
  expect_true(is.na(out$iso3[2]))
})

test_that("overlay_eez_abnj reprojects track data if needed", {
  eez <- sf::st_sf(
    iso3 = "USA",
    sovereign = "United States",
    eez_name = "US_EEZ",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  pts <- sf::st_sf(
    bird_id = "A",
    Date = "01/01/2020",
    Time = "00:00:00",
    geometry = sf::st_sfc(sf::st_point(c(5, 5))),
    crs = 3857
  )

  out <- overlay_eez_abnj(pts, eez)

  expect_equal(out$jurisdiction[1], "US_EEZ")
  expect_equal(sf::st_crs(out)$epsg, 3857)
})

test_that("calc_time_in_jurisdictions computes hours correctly", {
  df <- tibble::tibble(
    bird_id = "A",
    Date = c("01/01/2020", "01/01/2020", "01/01/2020"),
    Time = c("00:00:00", "01:00:00", "02:00:00"),
    jurisdiction = c("EEZ", "EEZ", "ABNJ"),
    iso3 = c("USA", "USA", NA_character_)
  )

  out <- calc_time_in_jurisdictions(df)

  expect_true(all(c("bird_id", "jurisdiction", "iso3", "n_fixes", "total_hours") %in% names(out)))
  expect_equal(out$total_hours[out$jurisdiction == "EEZ"], 2)
})

test_that("calc_time_in_jurisdictions returns raw table when requested", {
  df <- tibble::tibble(
    bird_id = "A",
    Date = c("01/01/2020", "01/01/2020"),
    Time = c("00:00:00", "01:00:00"),
    jurisdiction = c("EEZ", "ABNJ"),
    iso3 = c("USA", NA_character_)
  )

  out <- calc_time_in_jurisdictions(df, return_raw = TRUE)

  expect_true("step_duration_h" %in% names(out))
  expect_equal(out$step_duration_h[1], 1)
})

test_that("calc_time_in_jurisdictions warns on malformed dates or times", {
  df <- tibble::tibble(
    bird_id = "A",
    Date = c("bad_date", "01/01/2020"),
    Time = c("00:00:00", "01:00:00"),
    jurisdiction = c("EEZ", "ABNJ"),
    iso3 = c("USA", NA_character_)
  )

  expect_warning(
    calc_time_in_jurisdictions(df),
    "failed to parse"
  )
})

test_that("overlay_mpas labels point tracks inside and outside MPAs", {
  mpa <- sf::st_sf(
    mpa_id = 1,
    mpa_name = "Test MPA",
    iucn_cat = "II",
    status = "Designated",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  pts <- sf::st_sf(
    bird_id = c("A", "A"),
    Date = c("01/01/2020", "01/01/2020"),
    Time = c("00:00:00", "01:00:00"),
    geometry = sf::st_sfc(
      sf::st_point(c(5, 5)),
      sf::st_point(c(20, 20))
    ),
    crs = 4326
  )

  out <- overlay_mpas(pts, mpa)

  expect_true("in_mpa" %in% names(out))
  expect_equal(out$in_mpa, c(TRUE, FALSE))
  expect_equal(out$mpa_name[1], "Test MPA")
})

test_that("overlay_mpas computes polygon overlap", {
  mpa <- sf::st_sf(
    mpa_id = 1,
    mpa_name = "Test MPA",
    iucn_cat = "II",
    status = "Designated",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  ud <- sf::st_sf(
    ud_id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(5, 0), c(5, 10), c(15, 10), c(15, 0), c(5, 0)
    )))),
    crs = 4326
  )

  out <- overlay_mpas(ud, mpa)

  expect_true(nrow(out) >= 1)
  expect_true(all(c("overlap_km2", "pct_ud_in_mpa") %in% names(out)))
  expect_true(out$overlap_km2[1] > 0)
})

test_that("calc_transboundary_movements detects crossings", {
  df <- tibble::tibble(
    bird_id = "A",
    trip_id = 1,
    Time = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00"), tz = "UTC"),
    jurisdiction = c("EEZ", "ABNJ")
  )

  out <- calc_transboundary_movements(df)

  expect_equal(nrow(out), 1)
  expect_true(out$is_transboundary[1])
  expect_true(out$crossed_into_abnj[1])
  expect_equal(out$n_jurisdictions[1], 2)
})

test_that("calc_transboundary_movements handles single-jurisdiction trips", {
  df <- tibble::tibble(
    bird_id = "A",
    trip_id = 1,
    Time = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00"), tz = "UTC"),
    jurisdiction = c("EEZ", "EEZ")
  )

  out <- calc_transboundary_movements(df)

  expect_false(out$is_transboundary[1])
  expect_false(out$crossed_into_abnj[1])
  expect_equal(out$n_jurisdictions[1], 1)
})

test_that("overlay_priority_areas labels point tracks inside and outside areas", {
  pri <- sf::st_sf(
    area_id = 1,
    area_name = "Priority Area",
    priority_tier = "High",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  pts <- sf::st_sf(
    bird_id = c("A", "A"),
    Date = c("01/01/2020", "01/01/2020"),
    Time = c("00:00:00", "01:00:00"),
    geometry = sf::st_sfc(
      sf::st_point(c(5, 5)),
      sf::st_point(c(20, 20))
    ),
    crs = 4326
  )

  out <- overlay_priority_areas(pts, pri)

  expect_true("in_priority_area" %in% names(out))
  expect_equal(out$in_priority_area, c(TRUE, FALSE))
  expect_equal(out$area_name[1], "Priority Area")
})

test_that("overlay_priority_areas computes polygon overlap", {
  pri <- sf::st_sf(
    area_id = 1,
    area_name = "Priority Area",
    priority_tier = "High",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0)
    )))),
    crs = 4326
  )

  ud <- sf::st_sf(
    ud_id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(5, 0), c(5, 10), c(15, 10), c(15, 0), c(5, 0)
    )))),
    crs = 4326
  )

  out <- overlay_priority_areas(ud, pri)

  expect_true(nrow(out) >= 1)
  expect_true(all(c("overlap_km2", "pct_ud_in_area") %in% names(out)))
  expect_true(out$overlap_km2[1] > 0)
})
