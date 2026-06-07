library(testthat)
library(sf)

describe("export_spatial_outputs()", {
  make_point_sf <- function() {
    sf::st_as_sf(
      data.frame(
        track_id = c("A", "B"),
        longitude = c(175, 176),
        latitude = c(-20, -21)
      ),
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )
  }

  it("exports recognized spatial and table results and returns written paths", {
    results <- list(
      tracks = make_point_sf(),
      jurisdiction_summary = data.frame(
        bird_id = c("A", "B"),
        jurisdiction = c("EEZ", "ABNJ"),
        total_hours = c(10, 5)
      ),
      ignored_object = data.frame(x = 1)
    )

    testthat::local_mocked_bindings(
      export_gis_layers = function(layer, file_path, format = "gpkg", ...) {
        expect_s3_class(layer, "sf")
        expect_true(dir.exists(dirname(file_path)))
        expect_equal(format, "gpkg")
        invisible(file_path)
      },
      export_policy_summary_tables = function(tbl, file_path, format = "csv", ...) {
        expect_s3_class(tbl, "data.frame")
        expect_true(dir.exists(dirname(file_path)))
        expect_equal(format, "csv")
        invisible(file_path)
      }
    )

    out_dir <- tempfile("shearwater_exports_")

    written <- export_spatial_outputs(
      results,
      out_dir = out_dir,
      gis_format = "gpkg",
      table_format = "csv",
      overwrite = TRUE,
      verbose = FALSE
    )

    expect_type(written, "character")
    expect_true(all(c("tracks", "jurisdiction_summary") %in% names(written)))
    expect_true(all(grepl(out_dir, written, fixed = TRUE)))
  })

  it("returns character(0) when no recognized outputs are supplied", {
    results <- list(unused = data.frame(x = 1))

    out_dir <- tempfile("shearwater_exports_empty_")

    written <- export_spatial_outputs(
      results,
      out_dir = out_dir,
      verbose = FALSE
    )

    expect_type(written, "character")
    expect_length(written, 0)
  })

  it("errors on invalid arguments", {
    expect_error(
      export_spatial_outputs(data.frame(x = 1)),
      "results|named list",
      ignore.case = TRUE
    )

    expect_error(
      export_spatial_outputs(list(), out_dir = NA_character_),
      "out_dir|character",
      ignore.case = TRUE
    )

    expect_error(
      export_spatial_outputs(list(), gis_format = "bad_format"),
      "arg|gis_format|bad_format",
      ignore.case = TRUE
    )

    expect_error(
      export_spatial_outputs(list(), table_format = "bad_format"),
      "arg|table_format|bad_format",
      ignore.case = TRUE
    )

    expect_error(
      export_spatial_outputs(list(), verbose = NA),
      "verbose|TRUE or FALSE",
      ignore.case = TRUE
    )

    expect_error(
      export_spatial_outputs(list(), overwrite = NA),
      "overwrite|TRUE or FALSE",
      ignore.case = TRUE
    )
  })
})
