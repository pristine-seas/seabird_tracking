#' Export all major spatial deliverables in one step
#'
#' Writes recognized spatial and tabular outputs to an output directory.
#'
#' @param results A named list of spatial/tabular results.
#' @param out_dir Character. Output directory.
#' @param gis_format Character. One of `"gpkg"`, `"shp"`, or `"geojson"`.
#' @param table_format Character. One of `"csv"` or `"xlsx"`.
#' @param overwrite Logical. Whether to overwrite existing files.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return Invisibly returns a named character vector of written file paths.
#' @export
export_spatial_outputs <- function(results,
                                   out_dir = "outputs",
                                   gis_format = c("gpkg", "shp", "geojson"),
                                   table_format = c("csv", "xlsx"),
                                   overwrite = FALSE,
                                   verbose = TRUE) {
  if (is.data.frame(results) || !is.list(results)) {
    stop("`results` must be a named list.", call. = FALSE)
  }

  if (!is.character(out_dir) ||
      length(out_dir) != 1 ||
      is.na(out_dir) ||
      nchar(out_dir) == 0) {
    stop("`out_dir` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.logical(overwrite) ||
      length(overwrite) != 1 ||
      is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) ||
      length(verbose) != 1 ||
      is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  gis_format <- match.arg(gis_format)
  table_format <- match.arg(table_format)

  msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  resolve_path <- function(name, ext) {
    path <- file.path(out_dir, paste0(name, ".", ext))

    if (file.exists(path) && !isTRUE(overwrite)) {
      warning(
        "File already exists and `overwrite = FALSE`; skipping: ",
        path,
        call. = FALSE
      )
      return(NULL)
    }

    path
  }

  written <- character(0)

  gis_keys <- c(
    "tracks",
    "core_area",
    "home_range",
    "ud_polygons",
    "overlap_grid"
  )

  for (key in intersect(gis_keys, names(results))) {
    layer <- results[[key]]

    if (is.null(layer)) {
      next
    }

    if (!inherits(layer, c("sf", "sfc"))) {
      warning("`results$", key, "` is not an sf/sfc object; skipping.", call. = FALSE)
      next
    }

    path <- resolve_path(key, gis_format)

    if (is.null(path)) {
      next
    }

    msg("[export_spatial_outputs] Writing spatial layer: ", key)

    export_gis_layers(
      layer = layer,
      file_path = path,
      format = gis_format
    )

    written <- c(written, stats::setNames(path, key))
  }

  if (all(c("core_area", "home_range") %in% names(results)) &&
      inherits(results$core_area, "sf") &&
      inherits(results$home_range, "sf")) {
    ud_path <- resolve_path("ud_contours", gis_format)

    if (!is.null(ud_path)) {
      ud_polys <- rbind(results$core_area, results$home_range)

      msg("[export_spatial_outputs] Writing combined UD contours")

      export_ud_polygons(
        ud_polys = ud_polys,
        file_path = ud_path
      )

      written <- c(written, stats::setNames(ud_path, "ud_contours"))
    }
  }

  table_keys <- c("jurisdiction_summary", "policy_summary")

  for (key in intersect(table_keys, names(results))) {
    tbl <- results[[key]]

    if (is.null(tbl)) {
      next
    }

    if (!is.data.frame(tbl)) {
      warning("`results$", key, "` is not a data frame; skipping.", call. = FALSE)
      next
    }

    path <- resolve_path(key, table_format)

    if (is.null(path)) {
      next
    }

    msg("[export_spatial_outputs] Writing table: ", key)

    # Use positional argument so the test mock's `tbl` parameter is matched
    export_policy_summary_tables(
      tbl,
      file_path = path,
      overwrite = overwrite
    )

    written <- c(written, stats::setNames(path, key))
  }
  if (length(written) == 0) {
    msg("[export_spatial_outputs] No recognised results to export.")
  }

  invisible(written)
}
