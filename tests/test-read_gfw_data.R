library(testthat)
library(Shearwater)

test_that("read_gfw_data reads csv files", {
  tmp <- tempfile(fileext = ".csv")

  df <- data.frame(
    cell_id = c("1", "2"),
    longitude = c("175.1", "175.2"),
    latitude = c("-20.1", "-20.2"),
    effort = c("10", "20"),
    gear = c("longline", "trawl"),
    stringsAsFactors = FALSE
  )

  write.csv(df, tmp, row.names = FALSE)

  out <- read_gfw_data(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true(all(c("cell_id", "longitude", "latitude", "effort", "gear") %in% names(out)))
})

test_that("read_gfw_data reads tsv files", {
  tmp <- tempfile(fileext = ".tsv")

  df <- data.frame(
    cell_id = c("1", "2"),
    longitude = c("175.1", "175.2"),
    latitude = c("-20.1", "-20.2"),
    effort = c("10", "20"),
    gear = c("longline", "trawl"),
    stringsAsFactors = FALSE
  )

  write.table(df, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  out <- read_gfw_data(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("read_gfw_data reads txt delimited files", {
  tmp <- tempfile(fileext = ".txt")

  df <- data.frame(
    cell_id = c("1", "2"),
    longitude = c("175.1", "175.2"),
    latitude = c("-20.1", "-20.2"),
    effort = c("10", "20"),
    gear = c("longline", "trawl"),
    stringsAsFactors = FALSE
  )

  write.table(df, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  out <- read_gfw_data(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("read_gfw_data reads rds files", {
  tmp <- tempfile(fileext = ".rds")

  df <- data.frame(
    cell_id = c("1", "2"),
    effort = c("10", "20"),
    gear = c("longline", "trawl"),
    stringsAsFactors = FALSE
  )

  saveRDS(df, tmp)

  out <- read_gfw_data(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(out, df)
})

test_that("read_gfw_data errors on invalid paths and formats", {
  expect_error(
    read_gfw_data(NA_character_),
    "file_path",
    ignore.case = TRUE
  )

  expect_error(
    read_gfw_data("does_not_exist.csv"),
    "does not exist",
    ignore.case = TRUE
  )

  tmp <- tempfile(fileext = ".xlsx")
  writeLines("not really excel", tmp)

  expect_error(
    read_gfw_data(tmp),
    "Unsupported file type",
    ignore.case = TRUE
  )
})
