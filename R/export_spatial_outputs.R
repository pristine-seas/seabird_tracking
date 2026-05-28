#' Export all major spatial deliverables in one step
#'
#' Top-level wrapper that writes track lines, utilization-distribution
#' polygons, and policy/jurisdiction summary tables produced by the package
#' to an output directory.  Internally delegates to \code{export_gis_layers()}
#' and \code{export_policy_summary_tables()}.
#'
#' @param results A named list of spatial / tabular results, typically the
#'   combined output of \code{estimate_space_use()} and / or
#'   \code{analyze_jurisdiction_overlap()}.  Recognized list elements:
#'   \describe{
#'     \item{\code{tracks}}{An \code{sf} POINT or LINESTRING object of cleaned
#'       GPS tracks.}
#'     \item{\code{core_area}}{An \code{sf} POLYGON object (50 % UD contour).}
#'     \item{\code{home_range}}{An \code{sf} POLYGON object (95 % UD contour).}
#'     \item{\code{ud_polygons}}{Alternative to \code{core_area} /
#'       \code{home_range}: a single \code{sf} collection of UD polygons
#'       containing a \code{level} column.}
#'     \item{\code{jurisdiction_summary}}{A data frame / tibble from
#'       \code{calc_time_in_jurisdictions()}.}
#'     \item{\code{policy_summary}}{A data frame / tibble from
#'       \code{summarize_policy_exposure()}.}
#'     \item{\code{overlap_grid}}{An \code{sf} grid or raster-backed object
#'       from fisheries or MPA overlap analysis.}
#'   }
#'   Unrecognized names are silently ignored.
#' @param out_dir Character.  Path to the directory where files will be
#'   written.  Created if it does not exist.
#' @param gis_format Character.  Output format for vector GIS files.
#'   Passed to \code{export_gis_layers()}.  One of \code{"gpkg"} (default),
#'   \code{"shp"}, or \code{"geojson"}.
#' @param table_format Character.  Output format for tabular files.  Passed
#'   to \code{export_policy_summary_tables()}.  One of \code{"csv"} (default)
#'   or \code{"xlsx"}.
#' @param overwrite Logical.  Overwrite existing files.  Default \code{FALSE}.
#' @param verbose Logical.  Print progress messages.  Default \code{TRUE}.
#'
#' @return Invisibly returns a named character vector of the file paths
#'   written, or \code{character(0)} if nothing was exported.  Each element
#'   is named after the \code{results} key that produced it.
#'
#' @seealso \code{\link{export_gis_layers}},
#'   \code{\link{export_ud_polygons}},
#'   \code{\link{export_policy_summary_tables}},
#'   \code{\link{estimate_space_use}},
#'   \code{\link{analyze_jurisdiction_overlap}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' space  <- estimate_space_use(cleaned_tracks)
#' juris  <- analyze_jurisdiction_overlap(cleaned_tracks, mgmt_layers)
#'
#' results <- c(space, juris)   # merge named lists
#' written <- export_spatial_outputs(results, out_dir = "outputs/spatial")
#' written
#' }
export_spatial_outputs <- function(
    results,
    out_dir      = "outputs",
    gis_format   = c("gpkg", "shp", "geojson"),
    table_format = c("csv", "xlsx"),
    overwrite    = FALSE,
    verbose      = TRUE
) {
  gis_format   <- match.arg(gis_format)
  table_format <- match.arg(table_format)

  .msg <- function(...) if (verbose) message(...)

  # ── Input validation ────────────────────────────────────────────────────────
  if (!is.list(results)) {
    stop("`results` must be a named list.")
  }
  if (!is.character(out_dir) || length(out_dir) != 1L || nchar(out_dir) == 0) {
    stop("`out_dir` must be a non-empty character string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("`overwrite` must be TRUE or FALSE.")
  }

  if (!dir.exists(out_dir)) {
    .msg("[export_spatial_outputs] Creating output directory: ", out_dir)
    dir.create(out_dir, recursive = TRUE)
  }

  written <- character(0)

  # ── Helper: build and (optionally) guard file path ──────────────────────────
  .resolve_path <- function(name, ext) {
    p <- file.path(out_dir, paste0(name, ".", ext))
    if (file.exists(p) && !overwrite) {
      warning("File already exists and overwrite = FALSE; skipping: ", p,
              call. = FALSE)
      return(NULL)
    }
    p
  }

  # ── GIS spatial layers ───────────────────────────────────────────────────────
  gis_keys <- c("tracks", "core_area", "home_range", "ud_polygons",
                 "overlap_grid")

  for (key in intersect(gis_keys, names(results))) {
    layer <- results[[key]]
    if (is.null(layer)) next

    if (!inherits(layer, c("sf", "sfc", "RasterLayer", "SpatRaster"))) {
      warning("`results$", key, "` is not a recognised spatial object; ",
              "skipping.", call. = FALSE)
      next
    }

    p <- .resolve_path(key, gis_format)
    if (is.null(p)) next

    .msg("[export_spatial_outputs] Writing ", key, " -> ", basename(p))
    export_gis_layers(layer, file_path = p, format = gis_format)
    written <- c(written, stats::setNames(p, key))
  }

  # ── UD polygons via dedicated exporter (if core/home-range present) ──────────
  if (all(c("core_area", "home_range") %in% names(results))) {
    ud_polys <- rbind(
      results$core_area,
      results$home_range
    )
    p <- .resolve_path("ud_contours", gis_format)
    if (!is.null(p)) {
      .msg("[export_spatial_outputs] Writing combined UD contours -> ",
           basename(p))
      export_ud_polygons(ud_polys, file_path = p)
      written <- c(written, stats::setNames(p, "ud_contours"))
    }
  }

  # ── Tabular summary layers ───────────────────────────────────────────────────
  table_keys <- c("jurisdiction_summary", "policy_summary")

  for (key in intersect(table_keys, names(results))) {
    tbl <- results[[key]]
    if (is.null(tbl)) next

    if (!is.data.frame(tbl)) {
      warning("`results$", key, "` is not a data frame; skipping.",
              call. = FALSE)
      next
    }

    p <- .resolve_path(key, table_format)
    if (is.null(p)) next

    .msg("[export_spatial_outputs] Writing ", key, " -> ", basename(p))
    export_policy_summary_tables(tbl, file_path = p, format = table_format)
    written <- c(written, stats::setNames(p, key))
  }

  if (length(written) == 0L) {
    .msg("[export_spatial_outputs] No recognised results to export.")
  } else {
    .msg("[export_spatial_outputs] Done. ", length(written),
         " file(s) written to ", out_dir, ".")
  }

  invisible(written)
}
