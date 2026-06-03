# tests/testthat/test-top_level_wrappers.R
#
# Tests for:
#   clean_tracks()            (clean_tracks.R)
#   export_spatial_outputs()  (export_spatial_outputs.R)
#
# Strategy
# --------
# Both wrappers delegate to module functions that are not yet implemented, so
# all collaborator functions are mocked with local() + mockr / testthat::local_mocked_bindings.
# Integration-style tests that need real data are tagged with skip_if() guards.

library(testthat)

# ── Shared minimal fixtures ──────────────────────────────────────────────────

make_track_tbl <- function(n = 20L) {
  tibble::tibble(
    bird_id      = rep(c("A", "B"), each = n / 2L),
    timestamp    = seq(
      as.POSIXct("2023-01-01 00:00:00", tz = "UTC"),
      by           = "1 hour",
      length.out   = n
    ),
    lon          = seq(145.0, 145.5, length.out = n),
    lat          = seq(-35.0, -35.5, length.out = n),
    quality_flag = "ok"
  )
}

# All module functions return their first argument unchanged so pipeline wiring
# can be verified without real implementations.
stub_identity <- function(data, ...) data

# ═══════════════════════════════════════════════════════════════════════════════
# clean_tracks()
# ═══════════════════════════════════════════════════════════════════════════════

test_that("clean_tracks() errors on non-data-frame input", {
  expect_error(clean_tracks("not a data frame"), "`data` must be a data frame")
  expect_error(clean_tracks(1:10),               "`data` must be a data frame")
  expect_error(clean_tracks(NULL),               "`data` must be a data frame")
})

test_that("clean_tracks() errors on invalid max_speed", {
  df <- make_track_tbl()
  expect_error(clean_tracks(df, max_speed = -1),  "single positive number")
  expect_error(clean_tracks(df, max_speed = 0),   "single positive number")
  expect_error(clean_tracks(df, max_speed = "fast"), "single positive number")
  expect_error(clean_tracks(df, max_speed = c(10, 20)), "single positive number")
})

test_that("clean_tracks() errors on invalid interval_minutes", {
  df <- make_track_tbl()
  expect_error(clean_tracks(df, interval_minutes = 0),  "single positive number")
  expect_error(clean_tracks(df, interval_minutes = -60), "single positive number")
})

test_that("clean_tracks() errors on invalid max_gap_minutes", {
  df <- make_track_tbl()
  expect_error(clean_tracks(df, max_gap_minutes = 0),   "single positive number")
  expect_error(clean_tracks(df, max_gap_minutes = "big"), "single positive number")
})

test_that("clean_tracks() errors on invalid land_polygon type", {
  df <- make_track_tbl()
  expect_error(
    clean_tracks(df, land_polygon = data.frame(x = 1)),
    "sf/sfc/Spatial object or NULL"
  )
})

test_that("clean_tracks() runs full pipeline and returns a data frame", {
  df <- make_track_tbl()

  local_mocked_bindings(
    coerce_track_tbl              = stub_identity,
    flag_low_quality_fixes        = stub_identity,
    remove_duplicate_fixes        = stub_identity,
    filter_speed_outliers         = stub_identity,
    regularize_tracks             = stub_identity,
    interpolate_tracks            = stub_identity,
    .env = environment(clean_tracks)
  )

  out <- suppressMessages(clean_tracks(df, verbose = FALSE))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(df))
})

test_that("clean_tracks() attaches a cleaning_log attribute", {
  df <- make_track_tbl()

  local_mocked_bindings(
    coerce_track_tbl              = stub_identity,
    flag_low_quality_fixes        = stub_identity,
    remove_duplicate_fixes        = stub_identity,
    filter_speed_outliers         = stub_identity,
    regularize_tracks             = stub_identity,
    interpolate_tracks            = stub_identity,
    .env = environment(clean_tracks)
  )

  out  <- suppressMessages(clean_tracks(df, verbose = FALSE))
  log  <- attr(out, "cleaning_log")

  expect_type(log, "list")
  expect_true("input" %in% names(log))
  expect_equal(log$input, nrow(df))
})

test_that("clean_tracks() calls standardize_gps_columns() when col_map supplied", {
  df      <- make_track_tbl()
  col_map <- c(bird_id = "ring_id")
  called  <- FALSE

  local_mocked_bindings(
    standardize_gps_columns  = function(data, col_map) { called <<- TRUE; data },
    flag_low_quality_fixes   = stub_identity,
    remove_duplicate_fixes   = stub_identity,
    filter_speed_outliers    = stub_identity,
    regularize_tracks        = stub_identity,
    interpolate_tracks       = stub_identity,
    .env = environment(clean_tracks)
  )

  suppressMessages(clean_tracks(df, col_map = col_map, verbose = FALSE))
  expect_true(called)
})

test_that("clean_tracks() calls coerce_track_tbl() when no col_map supplied", {
  df     <- make_track_tbl()
  called <- FALSE

  local_mocked_bindings(
    coerce_track_tbl         = function(data, ...) { called <<- TRUE; data },
    flag_low_quality_fixes   = stub_identity,
    remove_duplicate_fixes   = stub_identity,
    filter_speed_outliers    = stub_identity,
    regularize_tracks        = stub_identity,
    interpolate_tracks       = stub_identity,
    .env = environment(clean_tracks)
  )

  suppressMessages(clean_tracks(df, verbose = FALSE))
  expect_true(called)
})

test_that("clean_tracks() skips flag_low_quality_fixes() when flag_quality = FALSE", {
  df     <- make_track_tbl()
  called <- FALSE

  local_mocked_bindings(
    coerce_track_tbl         = stub_identity,
    flag_low_quality_fixes   = function(data, ...) { called <<- TRUE; data },
    remove_duplicate_fixes   = stub_identity,
    filter_speed_outliers    = stub_identity,
    regularize_tracks        = stub_identity,
    interpolate_tracks       = stub_identity,
    .env = environment(clean_tracks)
  )

  suppressMessages(clean_tracks(df, flag_quality = FALSE, verbose = FALSE))
  expect_false(called)
})

test_that("clean_tracks() calls filter_on_land_or_invalid_points() when land_polygon provided", {
  df     <- make_track_tbl()
  land   <- structure(list(), class = c("sf", "data.frame"))
  called <- FALSE

  local_mocked_bindings(
    coerce_track_tbl                  = stub_identity,
    flag_low_quality_fixes            = stub_identity,
    remove_duplicate_fixes            = stub_identity,
    filter_speed_outliers             = stub_identity,
    filter_on_land_or_invalid_points  = function(data, ...) { called <<- TRUE; data },
    regularize_tracks                 = stub_identity,
    interpolate_tracks                = stub_identity,
    .env = environment(clean_tracks)
  )

  suppressMessages(
    clean_tracks(df, land_polygon = land, verbose = FALSE)
  )
  expect_true(called)
})

test_that("clean_tracks() drops flagged rows when drop_flagged = TRUE", {
  df <- make_track_tbl()
  df$quality_flag[1:5] <- "low"

  local_mocked_bindings(
    coerce_track_tbl       = stub_identity,
    flag_low_quality_fixes = stub_identity,
    remove_duplicate_fixes = stub_identity,
    filter_speed_outliers  = stub_identity,
    regularize_tracks      = stub_identity,
    interpolate_tracks     = stub_identity,
    .env = environment(clean_tracks)
  )

  out <- suppressMessages(
    clean_tracks(df, drop_flagged = TRUE, verbose = FALSE)
  )
  expect_true(all(out$quality_flag == "ok"))
  expect_equal(nrow(out), nrow(df) - 5L)
})

test_that("clean_tracks() retains flagged rows when drop_flagged = FALSE (default)", {
  df <- make_track_tbl()
  df$quality_flag[1:3] <- "low"

  local_mocked_bindings(
    coerce_track_tbl       = stub_identity,
    flag_low_quality_fixes = stub_identity,
    remove_duplicate_fixes = stub_identity,
    filter_speed_outliers  = stub_identity,
    regularize_tracks      = stub_identity,
    interpolate_tracks     = stub_identity,
    .env = environment(clean_tracks)
  )

  out <- suppressMessages(clean_tracks(df, verbose = FALSE))
  expect_equal(nrow(out), nrow(df))
})

test_that("clean_tracks() passes max_speed to filter_speed_outliers()", {
  df             <- make_track_tbl()
  received_speed <- NA_real_

  local_mocked_bindings(
    coerce_track_tbl       = stub_identity,
    flag_low_quality_fixes = stub_identity,
    remove_duplicate_fixes = stub_identity,
    filter_speed_outliers  = function(data, max_speed, ...) {
      received_speed <<- max_speed
      data
    },
    regularize_tracks      = stub_identity,
    interpolate_tracks     = stub_identity,
    .env = environment(clean_tracks)
  )

  suppressMessages(clean_tracks(df, max_speed = 15, verbose = FALSE))
  expect_equal(received_speed, 15)
})

test_that("clean_tracks() is silent when verbose = FALSE", {
  df <- make_track_tbl()

  local_mocked_bindings(
    coerce_track_tbl       = stub_identity,
    flag_low_quality_fixes = stub_identity,
    remove_duplicate_fixes = stub_identity,
    filter_speed_outliers  = stub_identity,
    regularize_tracks      = stub_identity,
    interpolate_tracks     = stub_identity,
    .env = environment(clean_tracks)
  )

  expect_silent(clean_tracks(df, verbose = FALSE))
})

# ═══════════════════════════════════════════════════════════════════════════════
# export_spatial_outputs()
# ═══════════════════════════════════════════════════════════════════════════════

make_sf_point <- function() {
  sf::st_as_sf(
    data.frame(lon = c(145.1, 145.2), lat = c(-35.1, -35.2)),
    coords = c("lon", "lat"),
    crs    = 4326
  )
}

test_that("export_spatial_outputs() errors on non-list results", {
  expect_error(export_spatial_outputs("not a list"), "`results` must be a named list")
  expect_error(export_spatial_outputs(42),           "`results` must be a named list")
})

test_that("export_spatial_outputs() errors on invalid out_dir", {
  expect_error(export_spatial_outputs(list(), out_dir = ""),    "non-empty character")
  expect_error(export_spatial_outputs(list(), out_dir = 123),   "non-empty character")
  expect_error(export_spatial_outputs(list(), out_dir = c("a", "b")), "non-empty character")
})

test_that("export_spatial_outputs() errors on invalid overwrite", {
  expect_error(
    export_spatial_outputs(list(), overwrite = "yes"),
    "TRUE or FALSE"
  )
})

test_that("export_spatial_outputs() creates out_dir if it does not exist", {
  tmp <- file.path(tempdir(), paste0("test_export_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))

  local_mocked_bindings(
    export_gis_layers              = function(...) invisible(NULL),
    export_ud_polygons             = function(...) invisible(NULL),
    export_policy_summary_tables   = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  suppressMessages(export_spatial_outputs(list(), out_dir = tmp, verbose = FALSE))
  expect_true(dir.exists(tmp))
})

test_that("export_spatial_outputs() returns character(0) for empty results", {
  tmp <- tempdir()

  local_mocked_bindings(
    export_gis_layers              = function(...) invisible(NULL),
    export_ud_polygons             = function(...) invisible(NULL),
    export_policy_summary_tables   = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  out <- suppressMessages(
    export_spatial_outputs(list(), out_dir = tmp, verbose = FALSE)
  )
  expect_equal(out, character(0))
})

test_that("export_spatial_outputs() writes tracks layer and returns its path", {
  tmp      <- file.path(tempdir(), paste0("eso_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  pts      <- make_sf_point()
  results  <- list(tracks = pts)
  written_path <- NULL

  local_mocked_bindings(
    export_gis_layers              = function(layer, file_path, ...) {
      written_path <<- file_path
      invisible(NULL)
    },
    export_ud_polygons             = function(...) invisible(NULL),
    export_policy_summary_tables   = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  out <- suppressMessages(
    export_spatial_outputs(results, out_dir = tmp, verbose = FALSE)
  )

  expect_length(out, 1L)
  expect_equal(names(out), "tracks")
  expect_match(out[["tracks"]], "tracks\\.gpkg$")
})

test_that("export_spatial_outputs() writes combined UD contours when core_area and home_range present", {
  tmp  <- file.path(tempdir(), paste0("eso_ud_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))

  poly <- make_sf_point()  # stand-in for polygon
  results <- list(core_area = poly, home_range = poly)
  ud_path <- NULL

  local_mocked_bindings(
    export_gis_layers    = function(layer, file_path, ...) invisible(NULL),
    export_ud_polygons   = function(ud_polys, file_path, ...) {
      ud_path <<- file_path
      invisible(NULL)
    },
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  out <- suppressMessages(
    export_spatial_outputs(results, out_dir = tmp, verbose = FALSE)
  )

  expect_true("ud_contours" %in% names(out))
  expect_match(ud_path, "ud_contours\\.gpkg$")
})

test_that("export_spatial_outputs() writes jurisdiction and policy tables", {
  tmp  <- file.path(tempdir(), paste0("eso_tbl_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))

  juris  <- data.frame(zone = "EEZ_AU", hours = 42.5)
  policy <- data.frame(area = "MPA_A",  pct = 0.18)
  results <- list(jurisdiction_summary = juris, policy_summary = policy)
  table_paths <- character(0)

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(tbl, file_path, ...) {
      table_paths <<- c(table_paths, file_path)
      invisible(NULL)
    },
    .env = environment(export_spatial_outputs)
  )

  out <- suppressMessages(
    export_spatial_outputs(results, out_dir = tmp, verbose = FALSE)
  )

  expect_setequal(names(out), c("jurisdiction_summary", "policy_summary"))
  expect_length(table_paths, 2L)
})

test_that("export_spatial_outputs() warns and skips non-spatial GIS entries", {
  tmp  <- file.path(tempdir(), paste0("eso_warn_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  results <- list(tracks = data.frame(x = 1))

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  expect_warning(
    suppressMessages(
      export_spatial_outputs(results, out_dir = tmp, verbose = FALSE)
    ),
    "not a recognised spatial object"
  )
})

test_that("export_spatial_outputs() warns and skips when file exists and overwrite = FALSE", {
  tmp <- file.path(tempdir(), paste0("eso_ow_", sample.int(1e6, 1)))
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  existing <- file.path(tmp, "tracks.gpkg")
  file.create(existing)

  pts     <- make_sf_point()
  results <- list(tracks = pts)

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  expect_warning(
    suppressMessages(
      export_spatial_outputs(results, out_dir = tmp, overwrite = FALSE,
                              verbose = FALSE)
    ),
    "overwrite = FALSE"
  )
})

test_that("export_spatial_outputs() respects gis_format = 'geojson'", {
  tmp  <- file.path(tempdir(), paste0("eso_fmt_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  pts     <- make_sf_point()
  results <- list(tracks = pts)
  saved   <- NULL

  local_mocked_bindings(
    export_gis_layers            = function(layer, file_path, ...) {
      saved <<- file_path; invisible(NULL)
    },
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  suppressMessages(
    export_spatial_outputs(results, out_dir = tmp, gis_format = "geojson",
                            verbose = FALSE)
  )
  expect_match(saved, "\\.geojson$")
})

test_that("export_spatial_outputs() respects table_format = 'xlsx'", {
  tmp  <- file.path(tempdir(), paste0("eso_xlsx_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  policy  <- data.frame(area = "MPA_A", pct = 0.1)
  results <- list(policy_summary = policy)
  saved   <- NULL

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(tbl, file_path, ...) {
      saved <<- file_path; invisible(NULL)
    },
    .env = environment(export_spatial_outputs)
  )

  suppressMessages(
    export_spatial_outputs(results, out_dir = tmp, table_format = "xlsx",
                            verbose = FALSE)
  )
  expect_match(saved, "\\.xlsx$")
})

test_that("export_spatial_outputs() is silent when verbose = FALSE", {
  tmp <- tempdir()

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  expect_silent(
    export_spatial_outputs(list(), out_dir = tmp, verbose = FALSE)
  )
})

test_that("export_spatial_outputs() invisibly returns paths", {
  tmp     <- file.path(tempdir(), paste0("eso_inv_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  pts     <- make_sf_point()
  results <- list(tracks = pts)

  local_mocked_bindings(
    export_gis_layers            = function(...) invisible(NULL),
    export_ud_polygons           = function(...) invisible(NULL),
    export_policy_summary_tables = function(...) invisible(NULL),
    .env = environment(export_spatial_outputs)
  )

  ret <- withVisible(
    suppressMessages(
      export_spatial_outputs(results, out_dir = tmp, verbose = FALSE)
    )
  )
  expect_false(ret$visible)
})
