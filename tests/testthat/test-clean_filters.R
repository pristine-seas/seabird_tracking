library(testthat)
library(sf)
library(readr)

# Load sample data
wtsh <- readr::read_csv("tests/testthat/wtshdata.csv", show_col_types = FALSE)

test_that("remove_duplicate_fixes removes duplicated timestamps", {

  df <- wtsh

  # add duplicate row
  df2 <- rbind(df, df[1, ])

  cleaned <- remove_duplicate_fixes(
    df2,
    id_col = "Type",   # using Type as stand-in ID column for testing
    date_col = "Date",
    time_col = "Time"
  )

  expect_lt(nrow(cleaned), nrow(df2))
  expect_equal(nrow(cleaned), nrow(df2) - 1)
})

test_that("remove_duplicate_fixes errors if ID column missing", {

  expect_error(
    remove_duplicate_fixes(
      wtsh,
      id_col = "BirdID"
    ),
    "Missing ID column"
  )
})

test_that("remove_duplicate_fixes works with datetime column", {

  df <- wtsh
  df$datetime <- as.POSIXct(
    paste(df$Date, df$Time),
    format = "%m/%d/%Y %H:%M:%S",
    tz = "UTC"
  )

  cleaned <- remove_duplicate_fixes(
    df,
    id_col = "Type",
    datetime_col = "datetime"
  )

  expect_s3_class(cleaned, "data.frame")
})

test_that("filter_speed_outliers removes rows above threshold", {

  filtered <- filter_speed_outliers(
    wtsh,
    max_speed = 5000,
    speed_col = "Speed",
    method = "remove"
  )

  expect_true(all(filtered$Speed <= 5000 | is.na(filtered$Speed)))
  expect_lt(nrow(filtered), nrow(wtsh))
})

test_that("filter_speed_outliers flags rows above threshold", {

  flagged <- filter_speed_outliers(
    wtsh,
    max_speed = 5000,
    speed_col = "Speed",
    method = "flag"
  )

  expect_true("..speed_outlier" %in% names(flagged))
  expect_type(flagged$..speed_outlier, "logical")
  expect_true(any(flagged$..speed_outlier))
})

test_that("filter_speed_outliers errors if speed column missing", {

  expect_error(
    filter_speed_outliers(
      wtsh,
      max_speed = 100,
      speed_col = "BADCOL"
    ),
    "Missing speed column"
  )
})

test_that("filter_on_land_or_invalid_points removes invalid coordinates", {

  df <- wtsh

  # inject invalid coordinate
  df$Latitude[1] <- 999

  cleaned <- filter_on_land_or_invalid_points(
    df,
    lat_col = "Latitude",
    lon_col = "Longitude",
    method = "remove"
  )

  expect_equal(nrow(cleaned), nrow(df) - 1)
})

test_that("filter_on_land_or_invalid_points flags invalid coordinates", {

  df <- wtsh
  df$Longitude[1] <- -999

  flagged <- filter_on_land_or_invalid_points(
    df,
    lat_col = "Latitude",
    lon_col = "Longitude",
    method = "flag"
  )

  expect_true("..spatial_outlier" %in% names(flagged))
  expect_true(flagged$..spatial_outlier[1])
})

test_that("filter_on_land_or_invalid_points works with exclusion polygon", {

  # simple polygon around first point
  poly <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(
          -159, 21,
          -157, 21,
          -157, 22,
          -159, 22,
          -159, 21
        ),
        ncol = 2,
        byrow = TRUE
      )))
    ),
    crs = 4326
  )

  flagged <- filter_on_land_or_invalid_points(
    wtsh,
    polygon = poly,
    method = "flag"
  )

  expect_true(any(flagged$..on_excluded_area))
})

test_that("filter_on_land_or_invalid_points errors with bad polygon", {

  expect_error(
    filter_on_land_or_invalid_points(
      wtsh,
      polygon = "not_a_polygon"
    ),
    "polygon must be an sf object"
  )
})

test_that("flag_low_quality_fixes creates QA columns", {

  flagged <- flag_low_quality_fixes(
    wtsh,
    max_speed = 5000,
    max_distance = 100
  )

  expect_true("..qa_high_speed" %in% names(flagged))
  expect_true("..qa_high_distance" %in% names(flagged))
  expect_true("..qa_invalid_type" %in% names(flagged))
  expect_true("..qa_low_essential" %in% names(flagged))
  expect_true("..qa_any" %in% names(flagged))
})

test_that("flag_low_quality_fixes identifies high speed fixes", {

  flagged <- flag_low_quality_fixes(
    wtsh,
    max_speed = 5000,
    max_distance = NULL
  )

  expect_true(any(flagged$..qa_high_speed))
})

test_that("flag_low_quality_fixes identifies invalid type codes", {

  flagged <- flag_low_quality_fixes(
    wtsh,
    max_speed = NULL,
    max_distance = NULL,
    valid_fix_types = 0
  )

  expect_true(any(flagged$..qa_invalid_type))
})

test_that("flag_low_quality_fixes identifies low essential values", {

  df <- wtsh
  df$Essential[1] <- 0

  flagged <- flag_low_quality_fixes(
    df,
    max_speed = NULL,
    max_distance = NULL
  )

  expect_true(flagged$..qa_low_essential[1])
})

test_that("flag_low_quality_fixes warns if no checks applied", {

  expect_warning(
    flag_low_quality_fixes(
      wtsh,
      speed_col = NULL,
      distance_col = NULL,
      fix_type_col = NULL,
      essential_col = NULL
    ),
    "No QA checks were applied"
  )
})
