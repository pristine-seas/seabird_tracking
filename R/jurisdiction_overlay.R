# =============================================================================
# jurisdiction_overlay.R
# Read and manage spatial jurisdiction and conservation layers
# =============================================================================

#' Read Management Layers
#'
#' Imports spatial management layers from disk, validates their CRS, and returns
#' them as a named list of `sf` objects. Supported inputs include GeoPackage,
#' GeoJSON, Shapefile, GML, and KML files.
#'
#' @param eez_path Character. Path to EEZ layer.
#' @param abnj_path Character. Optional path to ABNJ layer. If `NULL`, skipped.
#' @param mpa_path Character. Optional path to MPA layer. If `NULL`, skipped.
#' @param conservation_path Character. Optional path to conservation-priority
#'   layer. If `NULL`, skipped.
#' @param target_crs Numeric or character. Target CRS for all layers. Default is
#'   `4326`. Pass `NULL` to skip reprojection.
#' @param layer_name Character. Optional layer name for multi-layer files such as
#'   GeoPackages.
#' @param simplify Logical. If `TRUE`, simplify geometries.
#' @param simplify_tolerance Numeric. Simplification tolerance.
#' @param validate Logical. If `TRUE`, run basic geometry validity checks.
#'
#' @return A named list of `sf` objects with class `management_layers`.
#'
#' @export
read_management_layers <- function(eez_path,
                                   abnj_path = NULL,
                                   mpa_path = NULL,
                                   conservation_path = NULL,
                                   target_crs = 4326,
                                   layer_name = NULL,
                                   simplify = FALSE,
                                   simplify_tolerance = 0.01,
                                   validate = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for read_management_layers().", call. = FALSE)
  }

  read_single_layer <- function(path, layer_nm = NULL, label = "Layer") {
    if (is.null(path)) {
      return(NULL)
    }

    if (!is.character(path) ||
        length(path) != 1 ||
        is.na(path) ||
        path == "") {
      stop(label, " path must be a single non-empty character string.", call. = FALSE)
    }

    if (!file.exists(path)) {
      stop(label, " file not found: ", path, call. = FALSE)
    }

    ext <- tolower(tools::file_ext(path))

    tryCatch(
      {
        if (ext %in% c("gpkg", "geopackage")) {
          if (is.null(layer_nm)) {
            sf::st_read(path, quiet = TRUE)
          } else {
            sf::st_read(path, layer = layer_nm, quiet = TRUE)
          }
        } else if (ext %in% c("geojson", "shp", "gml", "kml")) {
          sf::st_read(path, quiet = TRUE)
        } else {
          stop(
            label,
            ": unsupported file format '.",
            ext,
            "'. Supported: .gpkg, .geojson, .shp, .gml, .kml",
            call. = FALSE
          )
        }
      },
      error = function(e) {
        stop(label, " failed to read: ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  process_layer <- function(data, label = "Layer", path = NULL) {
    if (is.null(data)) {
      return(NULL)
    }

    tryCatch(
      assert_crs(data),
      error = function(e) {
        stop(label, ": ", conditionMessage(e), call. = FALSE)
      }
    )

    if (!is.null(target_crs)) {
      current_crs <- sf::st_crs(data)
      target_crs_obj <- sf::st_crs(target_crs)

      if (current_crs != target_crs_obj) {
        data <- sf::st_transform(data, target_crs_obj)
      }
    }

    if (simplify) {
      data <- sf::st_simplify(data, dTolerance = simplify_tolerance)
    }

    if (validate) {
      invalid_geoms <- !sf::st_is_valid(data)

      if (any(invalid_geoms)) {
        warning(
          label,
          ": ",
          sum(invalid_geoms),
          " invalid geometry/geometries detected. ",
          "Consider running sf::st_make_valid().",
          call. = FALSE
        )
      }
    }

    attr(data, "source_path") <- path
    attr(data, "crs") <- sf::st_crs(data)
    attr(data, "n_features") <- nrow(data)
    attr(data, "read_date") <- Sys.time()

    data
  }

  message("Reading management layers...")

  eez <- read_single_layer(eez_path, layer_name, "EEZ")
  eez <- process_layer(eez, "EEZ", eez_path)

  abnj <- read_single_layer(abnj_path, layer_name, "ABNJ")
  abnj <- process_layer(abnj, "ABNJ", abnj_path)

  mpa <- read_single_layer(mpa_path, layer_name, "MPA")
  mpa <- process_layer(mpa, "MPA", mpa_path)

  conservation <- read_single_layer(
    conservation_path,
    layer_name,
    "Conservation"
  )
  conservation <- process_layer(conservation, "Conservation", conservation_path)

  result <- list(
    eez = eez,
    abnj = abnj,
    mpa = mpa,
    conservation = conservation
  )

  result <- result[!sapply(result, is.null)]

  class(result) <- c("management_layers", "list")
  attr(result, "target_crs") <- target_crs
  attr(result, "read_date") <- Sys.time()
  attr(result, "n_layers") <- length(result)

  message("Successfully read ", length(result), " management layer(s).")

  result
}


#' Print Management Layers Object
#'
#' Prints a compact summary of a `management_layers` object.
#'
#' @param x A `management_layers` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.management_layers <- function(x, ...) {
  cat("=== Management Layers ===\n")
  cat("Target CRS:", attr(x, "target_crs"), "\n")
  cat("Layers loaded:", attr(x, "n_layers"), "\n\n")

  for (layer_name in names(x)) {
    layer <- x[[layer_name]]

    cat(toupper(layer_name), ":\n")
    cat("  Features:", nrow(layer), "\n")
    cat("  Geometry type:", sf::st_geometry_type(layer)[1], "\n")
    cat("  CRS:", sf::st_crs(layer)$epsg, "\n")
    cat("  Columns:", paste(names(layer)[-ncol(layer)], collapse = ", "), "\n\n")
  }

  invisible(x)
}


#' Overlay Tracks with EEZ and ABNJ Jurisdictions
#'
#' Determines whether seabird track points fall within Exclusive Economic Zones
#' or areas beyond national jurisdiction.
#'
#' @param track_data An `sf` point object containing seabird track fixes.
#' @param eez_layer An `sf` polygon layer of EEZ boundaries. Expected columns
#'   include `iso3`, `sovereign`, and `eez_name`.
#'
#' @return An `sf` object with jurisdiction and ISO-3 country columns added.
#'
#' @export
overlay_eez_abnj <- function(track_data, eez_layer) {
  assert_crs(track_data)
  assert_crs(eez_layer)

  required_cols <- c("iso3", "sovereign", "eez_name")
  missing_cols <- setdiff(required_cols, names(eez_layer))

  if (length(missing_cols) > 0) {
    stop(
      "`eez_layer` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  track_proj <- sf::st_transform(track_data, sf::st_crs(eez_layer))

  joined <- sf::st_join(
    track_proj,
    eez_layer[, required_cols],
    join = sf::st_within,
    left = TRUE
  )

  joined <- joined |>
    dplyr::mutate(
      jurisdiction = dplyr::if_else(
        is.na(.data$eez_name),
        "ABNJ",
        .data$eez_name
      ),
      iso3 = dplyr::if_else(
        is.na(.data$iso3),
        NA_character_,
        .data$iso3
      )
    )

  sf::st_transform(joined, sf::st_crs(track_data))
}


#' Calculate Time in Jurisdictions
#'
#' Summarizes how much time birds spend in each jurisdictional area.
#'
#' @param track_data An `sf` object returned by `overlay_eez_abnj()`. Must contain
#'   `bird_id`, `Date`, `Time`, and `jurisdiction` columns.
#' @param return_raw Logical. If `TRUE`, returns the row-level table with step
#'   durations instead of the summary.
#'
#' @return A data frame with one row per bird and jurisdiction, or the raw
#'   row-level table if `return_raw = TRUE`.
#'
#' @export
calc_time_in_jurisdictions <- function(track_data, return_raw = FALSE) {
  assert_required_cols(
    track_data,
    c("bird_id", "Date", "Time", "jurisdiction")
  )

  tbl <- sf::st_drop_geometry(track_data)

  tbl <- tbl |>
    dplyr::mutate(
      time_clean = dplyr::case_when(
        nchar(.data$Time) == 6 ~ paste0(
          substr(.data$Time, 1, 2), ":",
          substr(.data$Time, 3, 4), ":",
          substr(.data$Time, 5, 6)
        ),
        TRUE ~ .data$Time
      ),
      datetime = as.POSIXct(
        paste(.data$Date, .data$time_clean),
        format = "%m/%d/%Y %H:%M:%S",
        tz = "UTC"
      )
    )

  if (any(is.na(tbl$datetime))) {
    warning(
      "Some Date/Time combinations failed to parse. Check for malformed rows.",
      call. = FALSE
    )
  }

  tbl <- tbl |>
    dplyr::arrange(.data$bird_id, .data$datetime) |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(
      step_duration_h = as.numeric(
        difftime(
          dplyr::lead(.data$datetime),
          .data$datetime,
          units = "hours"
        )
      )
    ) |>
    dplyr::ungroup()

  if (return_raw) {
    return(tbl)
  }

  group_cols <- c("bird_id", "jurisdiction")

  if ("iso3" %in% names(tbl)) {
    group_cols <- c(group_cols, "iso3")
  }

  tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_fixes = dplyr::n(),
      total_hours = sum(.data$step_duration_h, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$bird_id, dplyr::desc(.data$total_hours))
}


#' Overlay Tracks or Utilization Distributions with MPAs
#'
#' Measures overlap between seabird tracks or utilization-distribution polygons
#' and marine protected areas.
#'
#' @param track_data An `sf` point or polygon object.
#' @param mpa_layer An `sf` polygon layer of marine protected areas. Expected
#'   columns include `mpa_id`, `mpa_name`, `iucn_cat`, and `status`.
#'
#' @return If `track_data` contains points, returns the original points with MPA
#'   columns added. If `track_data` contains polygons, returns a data frame of
#'   overlap metrics.
#'
#' @export
overlay_mpas <- function(track_data, mpa_layer) {
  assert_crs(track_data)
  assert_crs(mpa_layer)

  required_cols <- c("mpa_id", "mpa_name", "iucn_cat", "status")
  missing_cols <- setdiff(required_cols, names(mpa_layer))

  if (length(missing_cols) > 0) {
    stop(
      "`mpa_layer` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  mpa_proj <- sf::st_transform(mpa_layer, sf::st_crs(track_data))
  geom_type <- unique(sf::st_geometry_type(track_data))

  if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
    joined <- sf::st_join(
      track_data,
      mpa_proj[, required_cols],
      join = sf::st_within,
      left = TRUE
    ) |>
      dplyr::mutate(
        in_mpa = !is.na(.data$mpa_id)
      )

    return(joined)
  }

  if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    ea_crs <- "+proj=moll +datum=WGS84"

    ud_ea <- sf::st_transform(track_data, ea_crs)
    mpa_ea <- sf::st_transform(mpa_proj, ea_crs)

    ud_area_m2 <- as.numeric(sf::st_area(sf::st_union(ud_ea)))

    intersection <- suppressWarnings(sf::st_intersection(ud_ea, mpa_ea)) |>
      dplyr::mutate(
        overlap_km2 = as.numeric(sf::st_area(.data$geometry)) / 1e6,
        pct_ud_in_mpa = (.data$overlap_km2 * 1e6 / ud_area_m2) * 100
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select(dplyr::all_of(required_cols), "overlap_km2", "pct_ud_in_mpa") |>
      dplyr::arrange(dplyr::desc(.data$overlap_km2))

    return(intersection)
  }

  stop("`track_data` must contain POINT or POLYGON geometries.", call. = FALSE)
}


#' Calculate Transboundary Movements
#'
#' Identifies trips or individuals that cross jurisdictional boundaries.
#'
#' @param track_data An `sf` object returned by `overlay_eez_abnj()`. Must contain
#'   `bird_id`, `trip_id`, `Time`, and `jurisdiction` columns.
#'
#' @return A data frame with one row per trip and columns describing the number
#'   of jurisdictions visited and whether the trip crossed into ABNJ.
#'
#' @export
calc_transboundary_movements <- function(track_data) {
  assert_required_cols(
    track_data,
    c("bird_id", "trip_id", "Time", "jurisdiction")
  )

  tbl <- sf::st_drop_geometry(track_data)

  tbl |>
    dplyr::group_by(.data$bird_id, .data$trip_id) |>
    dplyr::summarise(
      jurisdictions_visited = paste(unique(.data$jurisdiction), collapse = " | "),
      n_jurisdictions = dplyr::n_distinct(.data$jurisdiction),
      crossed_into_abnj = any(.data$jurisdiction == "ABNJ"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      is_transboundary = .data$n_jurisdictions > 1
    ) |>
    dplyr::arrange(.data$bird_id, .data$trip_id)
}


#' Overlay Tracks or Utilization Distributions with Priority Areas
#'
#' Measures overlap between seabird tracks or utilization-distribution polygons
#' and candidate conservation-priority areas.
#'
#' @param track_data An `sf` point or polygon object.
#' @param priority_layer An `sf` polygon layer of conservation priority areas.
#'   Expected columns include `area_id`, `area_name`, and `priority_tier`.
#'
#' @return If `track_data` contains points, returns the original points with
#'   priority-area columns added. If `track_data` contains polygons, returns a
#'   data frame of overlap metrics.
#'
#' @export
overlay_priority_areas <- function(track_data, priority_layer) {
  assert_crs(track_data)
  assert_crs(priority_layer)

  required_cols <- c("area_id", "area_name", "priority_tier")
  missing_cols <- setdiff(required_cols, names(priority_layer))

  if (length(missing_cols) > 0) {
    stop(
      "`priority_layer` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  priority_proj <- sf::st_transform(priority_layer, sf::st_crs(track_data))
  geom_type <- unique(sf::st_geometry_type(track_data))

  if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
    joined <- sf::st_join(
      track_data,
      priority_proj[, required_cols],
      join = sf::st_within,
      left = TRUE
    ) |>
      dplyr::mutate(
        in_priority_area = !is.na(.data$area_id)
      )

    return(joined)
  }

  if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    ea_crs <- "+proj=moll +datum=WGS84"

    ud_ea <- sf::st_transform(track_data, ea_crs)
    layer_ea <- sf::st_transform(priority_proj, ea_crs)

    ud_area_m2 <- as.numeric(sf::st_area(sf::st_union(ud_ea)))

    intersection <- suppressWarnings(sf::st_intersection(ud_ea, layer_ea)) |>
      dplyr::mutate(
        overlap_km2 = as.numeric(sf::st_area(.data$geometry)) / 1e6,
        pct_ud_in_area = (.data$overlap_km2 * 1e6 / ud_area_m2) * 100
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select(
        dplyr::all_of(required_cols),
        "overlap_km2",
        "pct_ud_in_area"
      ) |>
      dplyr::arrange(dplyr::desc(.data$overlap_km2))

    return(intersection)
  }

  stop("`track_data` must contain POINT or POLYGON geometries.", call. = FALSE)
}
