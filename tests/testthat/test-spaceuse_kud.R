library(testthat)
library(sf)
library(ggplot2)



# ------------------------------------------------------------------------------
# MOCK DATA FIXTURES FOR SPACE-USE / KUD TESTING
# ------------------------------------------------------------------------------

setup_mock_tracks <- function() {
  # Use enough spread and enough points per group so kernelUD can estimate
  # utilization distributions without singular-point problems.
  df <- data.frame(
    trip_id = c(rep("trip_A", 6), rep("trip_B", 6)),
    lon = c(
      0, 1, 2, 2, 1, 0,
      10, 11, 12, 12, 11, 10
    ),
    lat = c(
      0, 0, 1, 2, 3, 2,
      10, 10, 11, 12, 13, 12
    ),
    time = seq(
      from = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      by = "30 min",
      length.out = 12
    ),
    stringsAsFactors = FALSE
  )

  sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
}

make_overlap_polys <- function() {
  x_poly <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  y_poly <- sf::st_polygon(list(rbind(
    c(1, 1),
    c(3, 1),
    c(3, 3),
    c(1, 3),
    c(1, 1)
  )))

  x <- sf::st_sf(
    trip_id = "trip_A",
    level_pct = 50,
    geometry = sf::st_sfc(x_poly, crs = 4326)
  )

  y <- sf::st_sf(
    trip_id = "trip_A",
    level_pct = 95,
    geometry = sf::st_sfc(y_poly, crs = 4326)
  )

  list(x = x, y = y)
}


# ------------------------------------------------------------------------------
# TESTS FOR estimate_kernel_ud()
# ------------------------------------------------------------------------------

test_that("estimate_kernel_ud works with a custom grouping column", {
  tracks <- setup_mock_tracks()
  tracks$bird_id <- ifelse(tracks$trip_id == "trip_A", "bird_A", "bird_B")

  kud_res <- estimate_kernel_ud(
    tracks,
    id_col = "bird_id",
    ref = "href"
  )

  expect_true(inherits(kud_res, "estUDm") || inherits(kud_res, "EstUDm"))
  expect_equal(length(kud_res), 2)
  expect_setequal(names(kud_res), c("bird_A", "bird_B"))
})

test_that("estimate_kernel_ud errors on non-sf inputs", {
  df <- data.frame(
    trip_id = c("A", "A"),
    lon = c(0, 1),
    lat = c(0, 1)
  )

  expect_error(
    estimate_kernel_ud(df),
    "sf object"
  )
})

test_that("estimate_kernel_ud errors when custom id_col is missing", {
  tracks <- setup_mock_tracks()

  expect_error(
    estimate_kernel_ud(tracks, id_col = "missing_id"),
    "missing_id|ID column|contain",
    ignore.case = TRUE
  )
})

test_that("estimate_kernel_ud errors when geometries are not points", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  )))

  poly_sf <- sf::st_sf(
    trip_id = "trip_A",
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  expect_error(
    estimate_kernel_ud(poly_sf),
    "point geometries|POINT",
    ignore.case = TRUE
  )
})

test_that("estimate_kernel_ud errors when CRS is missing", {
  tracks <- setup_mock_tracks()
  sf::st_crs(tracks) <- NA

  expect_error(
    estimate_kernel_ud(tracks),
    "coordinate reference system|CRS",
    ignore.case = TRUE
  )
})

test_that("calculate_kud remains backward compatible with estimate_kernel_ud", {
  tracks <- setup_mock_tracks()

  kud_res <- calculate_kud(tracks, ref = "href")

  expect_true(inherits(kud_res, "estUDm") || inherits(kud_res, "EstUDm"))
  expect_equal(length(kud_res), 2)
  expect_setequal(names(kud_res), c("trip_A", "trip_B"))
})


# ------------------------------------------------------------------------------
# TESTS FOR get_isopleths(), get_core_area(), and get_home_range()
# ------------------------------------------------------------------------------

test_that("get_isopleths translates KUD contours into spatial sf polygons", {
  tracks <- setup_mock_tracks()
  kud_res <- estimate_kernel_ud(tracks, id_col = "trip_id", ref = "href")

  levels_vec <- c(50, 95)
  polys <- get_isopleths(kud_res, levels = levels_vec)

  expect_s3_class(polys, "sf")

  geom_types <- unique(as.character(sf::st_geometry_type(polys)))
  expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))

  expect_equal(nrow(polys), 4)
  expect_true("id" %in% names(polys))
  expect_true("level_pct" %in% names(polys))
  expect_setequal(unique(polys$level_pct), c(50, 95))
})

test_that("get_core_area returns 50 percent core-area polygons by default", {
  tracks <- setup_mock_tracks()
  kud_res <- estimate_kernel_ud(tracks, id_col = "trip_id", ref = "href")

  core <- get_core_area(kud_res)

  expect_s3_class(core, "sf")
  expect_true("level_pct" %in% names(core))
  expect_true("area_type" %in% names(core))
  expect_true(all(core$level_pct == 50))
  expect_true(all(core$area_type == "core_area"))
  expect_equal(nrow(core), 2)
})

test_that("get_home_range returns 95 percent home-range polygons by default", {
  tracks <- setup_mock_tracks()
  kud_res <- estimate_kernel_ud(tracks, id_col = "trip_id", ref = "href")

  home <- get_home_range(kud_res)

  expect_s3_class(home, "sf")
  expect_true("level_pct" %in% names(home))
  expect_true("area_type" %in% names(home))
  expect_true(all(home$level_pct == 95))
  expect_true(all(home$area_type == "home_range"))
  expect_equal(nrow(home), 2)
})

test_that("get_core_area and get_home_range reject invalid contour levels through get_isopleths", {
  tracks <- setup_mock_tracks()
  kud_res <- estimate_kernel_ud(tracks, id_col = "trip_id", ref = "href")

  expect_error(
    get_core_area(kud_res, level = 0),
    "greater than 0|less than 100"
  )

  expect_error(
    get_home_range(kud_res, level = 100),
    "greater than 0|less than 100"
  )
})

test_that("get_core_area and get_home_range error on non-KUD objects", {
  expect_error(
    get_core_area(data.frame(x = 1)),
    "EstUDm|estUDm"
  )

  expect_error(
    get_home_range(data.frame(x = 1)),
    "EstUDm|estUDm"
  )
})


# ------------------------------------------------------------------------------
# TESTS FOR calc_ud_overlap()
# ------------------------------------------------------------------------------

test_that("calc_ud_overlap computes positive overlap metrics for intersecting polygons", {
  polys <- make_overlap_polys()

  overlap <- calc_ud_overlap(polys$x, polys$y)

  expect_s3_class(overlap, "data.frame")
  expect_named(
    overlap,
    c("overlap_area_km2", "x_area_km2", "y_area_km2", "pct_x_overlap", "pct_y_overlap")
  )
  expect_equal(nrow(overlap), 1)
  expect_gt(overlap$overlap_area_km2, 0)
  expect_gt(overlap$pct_x_overlap, 0)
  expect_gt(overlap$pct_y_overlap, 0)
  expect_lte(overlap$pct_x_overlap, 100)
  expect_lte(overlap$pct_y_overlap, 100)
})

test_that("calc_ud_overlap returns zero overlap for non-intersecting polygons", {
  x_poly <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  )))

  y_poly <- sf::st_polygon(list(rbind(
    c(10, 10),
    c(11, 10),
    c(11, 11),
    c(10, 11),
    c(10, 10)
  )))

  x <- sf::st_sf(id = "A", geometry = sf::st_sfc(x_poly, crs = 4326))
  y <- sf::st_sf(id = "B", geometry = sf::st_sfc(y_poly, crs = 4326))

  overlap <- calc_ud_overlap(x, y)

  expect_equal(nrow(overlap), 1)
  expect_equal(overlap$overlap_area_km2, 0)
  expect_equal(overlap$pct_x_overlap, 0)
  expect_equal(overlap$pct_y_overlap, 0)
})

test_that("calc_ud_overlap calculates grouped overlaps when by is supplied", {
  polys <- make_overlap_polys()

  x2 <- polys$x
  x2$trip_id <- "trip_B"

  y2 <- polys$y
  y2$trip_id <- "trip_B"

  x_all <- rbind(polys$x, x2)
  y_all <- rbind(polys$y, y2)

  overlap <- calc_ud_overlap(x_all, y_all, by = "trip_id")

  expect_s3_class(overlap, "data.frame")
  expect_true("trip_id" %in% names(overlap))
  expect_setequal(overlap$trip_id, c("trip_A", "trip_B"))
  expect_equal(nrow(overlap), 2)
  expect_true(all(overlap$overlap_area_km2 > 0))
})

test_that("calc_ud_overlap transforms mismatched CRS before overlap", {
  polys <- make_overlap_polys()

  y_projected <- sf::st_transform(polys$y, 3857)

  overlap <- calc_ud_overlap(polys$x, y_projected)

  expect_s3_class(overlap, "data.frame")
  expect_equal(nrow(overlap), 1)
  expect_gt(overlap$overlap_area_km2, 0)
})

test_that("calc_ud_overlap errors on invalid inputs", {
  polys <- make_overlap_polys()

  expect_error(
    calc_ud_overlap(data.frame(x = 1), polys$y),
    "sf objects"
  )

  expect_error(
    calc_ud_overlap(polys$x, polys$y, by = "missing_group"),
    "by columns|exist",
    ignore.case = TRUE
  )

  x_no_crs <- polys$x
  sf::st_crs(x_no_crs) <- NA

  expect_error(
    calc_ud_overlap(x_no_crs, polys$y),
    "CRS"
  )
})
