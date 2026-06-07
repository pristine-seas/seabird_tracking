library(testthat)
library(Shearwater)

test_that("standardize_fishing_effort standardizes effort and preserves gear", {
  df <- data.frame(
    effort = c(5, 8, 0, 12),
    gear = c("longline", "trawl", "purse seine", "longline"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df)

  expect_s3_class(out, "data.frame")
  expect_true("effort" %in% names(out))
  expect_true("gear" %in% names(out))
  expect_true("effort_std" %in% names(out))
  expect_equal(nrow(out), nrow(df))
  expect_type(out$effort, "double")
  expect_type(out$effort_std, "double")
})

test_that("standardize_fishing_effort can apply gear recoding", {
  df <- data.frame(
    effort = c(5, 8, 0, 12),
    gear = c("ll", "trawl", "ps", "ll"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(
    df,
    gear_map = c(
      ll = "longline",
      ps = "purse seine"
    )
  )

  expect_true(all(c("longline", "trawl", "purse seine") %in% out$gear))
})

test_that("standardize_fishing_effort errors on invalid inputs", {
  expect_error(
    standardize_fishing_effort("not data"),
    "data frame",
    ignore.case = TRUE
  )

  expect_error(
    standardize_fishing_effort(data.frame(gear = "longline")),
    "effort_col",
    ignore.case = TRUE
  )

  expect_error(
    standardize_fishing_effort(data.frame(effort = 1)),
    "gear_col",
    ignore.case = TRUE
  )
})
