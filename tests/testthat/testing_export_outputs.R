#testing
library(testthat)
library(dplyr)
library(sf)
library(readr)

describe("summarize_policy_exposure()", {
  # Prepare dummy metrics to feed the summarization pipeline engine
  jur_summary <- tibble(
    bird_id = c("A", "A", "B"),
    jurisdiction = c("EEZ-Australia", "High-Seas", "EEZ-NewZealand"),
    total_hours = c(40, 10, 100)
  )

  # Track telemetry strings must emulate real character dates/times or pass nchar requirements
  mpa_track <- sf::st_sf(
    bird_id = c("A", "A", "B"),
    Date = c("01/01/2026", "01/02/2026", "01/01/2026"),
    Time = c("120000", "120000", "120000"),
    in_mpa = c(TRUE, FALSE, TRUE),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(0,0)), sf::st_point(c(0,0))),
    crs = 4326
  )

  priority_track <- sf::st_sf(
    bird_id = c("A", "A", "B"),
    Date = c("01/01/2026", "01/02/2026", "01/01/2026"),
    Time = c("120000", "120000", "120000"),
    in_priority_area = c(FALSE, FALSE, TRUE),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(0,0)), sf::st_point(c(0,0))),
    crs = 4326
  )

  transboundary_summary <- tibble(
    bird_id = c("A", "B"),
    is_transboundary = c(TRUE, FALSE),
    crossed_into_abnj = c(TRUE, FALSE)
  )

  it("calculates aggregate exposure summaries and cross-boundary metrics correctly", {
    summary_output <- summarize_policy_exposure(
      jurisdiction_summary = jur_summary,
      mpa_track = mpa_track,
      priority_track = priority_track,
      transboundary_summary = transboundary_summary
    )

    expect_s3_class(summary_output, "tbl_df")
    expect_equal(nrow(summary_output), 2) # Unique birds A and B
    expect_named(summary_output, c(
      "bird_id", "top_jurisdiction", "pct_time_top_jurisdiction",
      "total_hours_in_mpa", "pct_fixes_in_mpa",
      "total_hours_in_priority_area", "pct_fixes_in_priority_area",
      "n_transboundary_trips", "crossed_abnj_any_trip"
    ))

    # Validate specific outputs
    expect_equal(summary_output$top_jurisdiction[summary_output$bird_id == "A"], "EEZ-Australia")
    expect_equal(summary_output$pct_time_top_jurisdiction[summary_output$bird_id == "A"], 80) # 40 out of 50 hours
  })
})


describe("export_policy_summary_tables()", {
  # Setup tracking testing environment paths
  temp_dir<- tempfile("export_test_")
  single_csv_path <- file.path(temp_dir, "policy_report.csv")

  # Base table data to output
  mock_data <- tibble(bird_id = c("A", "B"), score = c(10, 20))

  it("writes out a single flat tibble directly to a specific target CSV location", {
    # Ensure cleanup environment is fresh
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

    export_policy_summary_tables(mock_data, file_path = single_csv_path, overwrite = FALSE)

    expect_true(file.exists(single_csv_path))
    readback <- readr::read_csv(single_csv_path, show_col_types = FALSE)
    expect_equal(nrow(readback), 2)
  })

  it("halts execution with an error when attempting to overwrite an existing file if overwrite = FALSE", {
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    file.create(single_csv_path)

    expect_error(
      export_policy_summary_tables(mock_data, file_path = single_csv_path, overwrite = FALSE),
      "File already exists"
    )
  })

  it("writes list contents into structured files inside a directory named after their keys", {
    multi_table_dir <- file.path(temp_dir, "multi_output")
    payload <- list(
      policy = mock_data,
      jurisdictions = tibble(region = "High-Seas")
    )

    export_policy_summary_tables(payload, file_path = multi_table_dir, overwrite = TRUE)

    expect_true(file.exists(file.path(multi_table_dir, "policy.csv")))
    expect_true(file.exists(file.path(multi_table_dir, "jurisdictions.csv")))
  })

  it("raises an error when a list parameter lacks complete naming attributes", {
    unnamed_payload <- list(mock_data, tibble(val = 1))
    expect_error(
      export_policy_summary_tables(unnamed_payload, file_path = temp_dir),
      "must be a fully named list"
    )
  })
})
