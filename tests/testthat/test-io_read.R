library(testthat)

test_that("read_gps_data successfully reads csv and assigns attributes", {
  # Create a temporary CSV file
  tmp_csv <- tempfile(fileext = ".csv")
  dummy_data <- data.frame(id = 1:3, lat = c(-20, -21, -22), lon = c(175, 176, 177))
  readr::write_csv(dummy_data, tmp_csv)
  
  # Run function
  res <- read_gps_data(tmp_csv, format = "csv")
  
  # Assertions
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_equal(attr(res, "source_file"), tmp_csv)
  expect_s3_class(attr(res, "import_time"), "POSIXct")
  
  # Cleanup
  unlink(tmp_csv)
})

test_that("read_gps_data catches invalid files and formats", {
  expect_error(read_gps_data("does_not_exist.csv"), "File not found")
  
  tmp_txt <- tempfile(fileext = ".txt")
  writeLines("dummy", tmp_txt)
  expect_error(read_gps_data(tmp_txt, format = "txt"), "Unsupported format")
  unlink(tmp_txt)
})