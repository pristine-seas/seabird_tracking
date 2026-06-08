#' Summarize Policy Exposure
#'
#' Creates a policy-facing summary of each bird's exposure to jurisdictions,
#' marine protected areas, and candidate conservation priority areas.
#'
#' @param jurisdiction_summary A data frame produced by
#'   `calc_time_in_jurisdictions()`. Must contain `bird_id`, `jurisdiction`,
#'   and `total_hours`.
#' @param mpa_track An `sf` object or data frame of track points labeled by
#'   `overlay_mpas()`. Must contain `bird_id`, `Date`, `Time`, and `in_mpa`.
#' @param priority_track An `sf` object or data frame of track points labeled by
#'   `overlay_priority_areas()`. Must contain `bird_id`, `Date`, `Time`, and
#'   `in_priority_area`.
#' @param transboundary_summary A data frame produced by
#'   `calc_transboundary_movements()`. Must contain `bird_id`,
#'   `is_transboundary`, and `crossed_into_abnj`.
#'
#' @return A data frame with one row per bird summarizing jurisdiction,
#'   MPA, priority-area, and transboundary exposure metrics.
#'
#' @export
summarize_policy_exposure <- function(jurisdiction_summary,
                                      mpa_track,
                                      priority_track,
                                      transboundary_summary) {
  if (!is.data.frame(jurisdiction_summary)) {
    stop("`jurisdiction_summary` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(mpa_track)) {
    stop("`mpa_track` must be a data frame or sf object.", call. = FALSE)
  }

  if (!is.data.frame(priority_track)) {
    stop("`priority_track` must be a data frame or sf object.", call. = FALSE)
  }

  if (!is.data.frame(transboundary_summary)) {
    stop("`transboundary_summary` must be a data frame.", call. = FALSE)
  }

  assert_required_cols(
    jurisdiction_summary,
    c("bird_id", "jurisdiction", "total_hours")
  )

  assert_required_cols(
    transboundary_summary,
    c("bird_id", "is_transboundary", "crossed_into_abnj")
  )

  # Jurisdiction: identify top jurisdiction and percent of total time.
  jur <- jurisdiction_summary |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(
      total_bird_hours = sum(.data$total_hours, na.rm = TRUE)
    ) |>
    dplyr::slice_max(
      order_by = .data$total_hours,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      bird_id = .data$bird_id,
      top_jurisdiction = .data$jurisdiction,
      pct_time_top_jurisdiction = dplyr::if_else(
        .data$total_bird_hours > 0,
        (.data$total_hours / .data$total_bird_hours) * 100,
        NA_real_
      )
    )

  # MPA exposure.
  mpa_tbl <- drop_geometry_if_sf(mpa_track)
  assert_required_cols(mpa_tbl, c("bird_id", "Date", "Time", "in_mpa"))

  mpa_tbl <- .add_step_hours(
    mpa_tbl,
    bird_id_col = "bird_id",
    date_col = "Date",
    time_col = "Time"
  )

  mpa_per_bird <- mpa_tbl |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      total_hours_in_mpa = sum(.data$step_h[.data$in_mpa], na.rm = TRUE),
      pct_fixes_in_mpa = mean(.data$in_mpa, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  # Priority-area exposure.
  pri_tbl <- drop_geometry_if_sf(priority_track)
  assert_required_cols(
    pri_tbl,
    c("bird_id", "Date", "Time", "in_priority_area")
  )

  pri_tbl <- .add_step_hours(
    pri_tbl,
    bird_id_col = "bird_id",
    date_col = "Date",
    time_col = "Time"
  )

  pri_per_bird <- pri_tbl |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      total_hours_in_priority_area = sum(
        .data$step_h[.data$in_priority_area],
        na.rm = TRUE
      ),
      pct_fixes_in_priority_area = mean(
        .data$in_priority_area,
        na.rm = TRUE
      ) * 100,
      .groups = "drop"
    )

  # Transboundary exposure.
  trans_per_bird <- transboundary_summary |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      n_transboundary_trips = sum(.data$is_transboundary, na.rm = TRUE),
      crossed_abnj_any_trip = any(.data$crossed_into_abnj, na.rm = TRUE),
      .groups = "drop"
    )

  jur |>
    dplyr::left_join(mpa_per_bird, by = "bird_id") |>
    dplyr::left_join(pri_per_bird, by = "bird_id") |>
    dplyr::left_join(trans_per_bird, by = "bird_id") |>
    dplyr::arrange(.data$bird_id)
}


#' Export Policy Summary Tables
#'
#' Writes one or more policy summary tables to CSV files for reporting and
#' decision support.
#'
#' @param summary_data A data frame, or a fully named list of data frames. If a
#'   single data frame is supplied, `file_path` must be a `.csv` file path. If a
#'   named list is supplied, `file_path` must be a directory path, and one CSV is
#'   written per list element.
#' @param file_path Character. Output CSV path for a single table, or output
#'   directory for a named list of tables.
#' @param overwrite Logical. If `FALSE`, errors when an output file already
#'   exists. Default is `FALSE`.
#'
#' @return Invisibly returns a character vector of written file paths.
#'
#' @export
export_policy_summary_tables <- function(summary_data,
                                         file_path,
                                         overwrite = FALSE) {
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("file_path must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }

  # Single data frame: write directly to file_path.
  if (is.data.frame(summary_data)) {
    if (!grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      stop(
        "file_path must end in .csv when exporting a single table.",
        call. = FALSE
      )
    }

    if (!overwrite && file.exists(file_path)) {
      stop(
        "File already exists: ",
        file_path,
        ". Set overwrite = TRUE to replace it.",
        call. = FALSE
      )
    }

    dir.create(
      dirname(file_path),
      showWarnings = FALSE,
      recursive = TRUE
    )

    readr::write_csv(summary_data, file_path)

    message("Written: ", file_path)

    return(invisible(file_path))
  }

  # Named list of data frames: write one CSV per element into file_path.
  if (is.list(summary_data)) {
    if (is.null(names(summary_data)) || any(names(summary_data) == "")) {
      stop(
        "summary_data must be a fully named list when exporting multiple tables.",
        call. = FALSE
      )
    }

    dir.create(
      file_path,
      showWarnings = FALSE,
      recursive = TRUE
    )

    written <- character(0)

    for (i in seq_along(summary_data)) {
      tbl <- summary_data[[i]]
      table_name <- names(summary_data)[i]
      out_path <- file.path(file_path, paste0(table_name, ".csv"))

      if (!is.data.frame(tbl)) {
        warning(
          "Skipping '",
          table_name,
          "' because it is not a data frame.",
          call. = FALSE
        )
        next
      }

      if (!overwrite && file.exists(out_path)) {
        stop(
          "File already exists: ",
          out_path,
          ". Set overwrite = TRUE to replace it.",
          call. = FALSE
        )
      }

      readr::write_csv(tbl, out_path)

      message("Written: ", out_path)

      written <- c(written, stats::setNames(out_path, table_name))
    }

    return(invisible(written))
  }

  stop(
    "summary_data must be a data frame or a named list of data frames.",
    call. = FALSE
  )
}


# Internal helper: drop geometry only when input is sf.
drop_geometry_if_sf <- function(x) {
  if (inherits(x, "sf")) {
    return(sf::st_drop_geometry(x))
  }

  x
}


# Internal helper: clean time strings and calculate step durations.
.add_step_hours <- function(data,
                            bird_id_col = "bird_id",
                            date_col = "Date",
                            time_col = "Time") {
  assert_required_cols(data, c(bird_id_col, date_col, time_col))

  out <- data |>
    dplyr::mutate(
      time_clean = dplyr::case_when(
        nchar(.data[[time_col]]) == 6 ~ paste0(
          substr(.data[[time_col]], 1, 2),
          ":",
          substr(.data[[time_col]], 3, 4),
          ":",
          substr(.data[[time_col]], 5, 6)
        ),
        TRUE ~ .data[[time_col]]
      ),
      datetime = as.POSIXct(
        paste(.data[[date_col]], .data$time_clean),
        format = "%m/%d/%Y %H:%M:%S",
        tz = "UTC"
      )
    )

  if (any(is.na(out$datetime))) {
    warning(
      "Some Date/Time combinations failed to parse. Check for malformed rows.",
      call. = FALSE
    )
  }

  out |>
    dplyr::arrange(.data[[bird_id_col]], .data$datetime) |>
    dplyr::group_by(.data[[bird_id_col]]) |>
    dplyr::mutate(
      step_h = as.numeric(
        difftime(
          dplyr::lead(.data$datetime),
          .data$datetime,
          units = "hours"
        )
      )
    ) |>
    dplyr::ungroup()
}
