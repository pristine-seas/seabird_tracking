library(testthat)
library(sf)
library(dplyr)
library(readr)

test_that("export_policy_summary_tables writes single csv", {
  tmp <- tempfile(fileext = ".csv")
  df <- tibble::tibble(x = 1:3)

  out <- export_policy_summary_tables(df, tmp, overwrite = TRUE)

  expect_true(file.exists(tmp))
  expect_equal(out, tmp)
})

test_that("export_policy_summary_tables writes named list to directory", {
  tmpdir <- tempfile("policy_export_")
  dir.create(tmpdir)

  lst <- list(
    policy = tibble::tibble(x = 1),
    jurisdictions = tibble::tibble(y = 2)
  )

  out <- export_policy_summary_tables(lst, tmpdir, overwrite = TRUE)

  expect_true(file.exists(file.path(tmpdir, "policy.csv")))
  expect_true(file.exists(file.path(tmpdir, "jurisdictions.csv")))
  expect_equal(length(out), 2)
})

test_that("export_policy_summary_tables errors on invalid input", {
  expect_error(
    export_policy_summary_tables(1, tempfile(fileext = ".csv")),
    "summary_data must be a data frame or a named list of data frames"
  )
})

test_that("export_policy_summary_tables errors when single csv path is not csv", {
  tmp <- tempfile(fileext = ".txt")
  df <- tibble::tibble(x = 1)

  expect_error(
    export_policy_summary_tables(df, tmp, overwrite = TRUE),
    "file_path must end in .csv"
  )
})

test_that("export_policy_summary_tables errors on existing file when overwrite is FALSE", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", tmp)

  df <- tibble::tibble(x = 1)

  expect_error(
    export_policy_summary_tables(df, tmp, overwrite = FALSE),
    "File already exists"
  )
})

test_that("export_policy_summary_tables errors on unnamed list", {
  tmpdir <- tempfile("policy_export_")
  dir.create(tmpdir)

  lst <- list(tibble::tibble(x = 1), tibble::tibble(y = 2))

  expect_error(
    export_policy_summary_tables(lst, tmpdir, overwrite = TRUE),
    "fully named list"
  )
})
