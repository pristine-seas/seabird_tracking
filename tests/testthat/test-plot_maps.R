# ------------------------------------------------------------------------------
# TESTS FOR plot_core_use_hotspots()
# ------------------------------------------------------------------------------

make_hotspot_polys <- function() {
  p1 <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  p2 <- sf::st_polygon(list(rbind(
    c(3, 3),
    c(5, 3),
    c(5, 5),
    c(3, 5),
    c(3, 3)
  )))

  sf::st_sf(
    id = c("trip_A", "trip_B"),
    level_pct = c(50, 95),
    area_type = c("core_area", "home_range"),
    geometry = sf::st_sfc(p1, p2, crs = 4326)
  )
}

test_that("plot_core_use_hotspots creates a ggplot from sf polygons", {
  hotspot_sf <- make_hotspot_polys()

  p <- plot_core_use_hotspots(hotspot_sf)

  expect_s3_class(p, "ggplot")
})

test_that("plot_core_use_hotspots respects an explicit fill column", {
  hotspot_sf <- make_hotspot_polys()

  p <- plot_core_use_hotspots(
    hotspot_sf,
    fill_col = "area_type",
    title = "Core and Home Range Test"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Core and Home Range Test")
})

test_that("plot_core_use_hotspots errors on non-sf inputs", {
  expect_error(
    plot_core_use_hotspots(data.frame(level_pct = c(50, 95))),
    "sf object"
  )
})

test_that("plot_core_use_hotspots errors when fill_col is missing", {
  hotspot_sf <- make_hotspot_polys()

  expect_error(
    plot_core_use_hotspots(hotspot_sf, fill_col = "missing_col"),
    "not found"
  )
})

test_that("plot_hotspot_map remains backward compatible with plot_core_use_hotspots", {
  hotspot_sf <- make_hotspot_polys()

  p <- plot_hotspot_map(hotspot_sf)

  expect_s3_class(p, "ggplot")
})


# ------------------------------------------------------------------------------
# TESTS FOR plot_jurisdiction_summary()
# ------------------------------------------------------------------------------

test_that("plot_jurisdiction_summary creates a ggplot from jurisdiction summaries", {
  jurisdiction_df <- data.frame(
    bird_id = c("bird_A", "bird_A", "bird_B", "bird_B"),
    jurisdiction = c("EEZ", "ABNJ", "EEZ", "ABNJ"),
    total_hours = c(12, 5, 8, 9),
    stringsAsFactors = FALSE
  )

  p <- plot_jurisdiction_summary(jurisdiction_df)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Jurisdiction Exposure Summary")
})

test_that("plot_jurisdiction_summary supports faceting by bird id", {
  jurisdiction_df <- data.frame(
    bird_id = c("bird_A", "bird_A", "bird_B", "bird_B"),
    jurisdiction = c("EEZ", "ABNJ", "EEZ", "ABNJ"),
    total_hours = c(12, 5, 8, 9),
    stringsAsFactors = FALSE
  )

  p <- plot_jurisdiction_summary(
    jurisdiction_df,
    facet_col = "bird_id",
    title = "Exposure by Bird"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Exposure by Bird")
})

test_that("plot_jurisdiction_summary supports custom x, y, and fill columns", {
  policy_df <- data.frame(
    bird_id = c("A", "B", "C"),
    zone_type = c("MPA", "Priority Area", "ABNJ"),
    pct_time = c(20.5, 33.1, 46.4),
    risk_group = c("low", "medium", "high"),
    stringsAsFactors = FALSE
  )

  p <- plot_jurisdiction_summary(
    policy_df,
    x_col = "zone_type",
    y_col = "pct_time",
    fill_col = "risk_group",
    title = "Policy Exposure",
    y_label = "Percent of Time"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Policy Exposure")
  expect_equal(p$labels$y, "Percent of Time")
  expect_equal(p$labels$fill, "risk_group")
})

test_that("plot_jurisdiction_summary coerces numeric-like y columns", {
  jurisdiction_df <- data.frame(
    jurisdiction = c("EEZ", "ABNJ"),
    total_hours = c("12.5", "7.5"),
    stringsAsFactors = FALSE
  )

  p <- plot_jurisdiction_summary(jurisdiction_df)

  expect_s3_class(p, "ggplot")
})

test_that("plot_jurisdiction_summary errors on invalid input types and missing columns", {
  jurisdiction_df <- data.frame(
    jurisdiction = c("EEZ", "ABNJ"),
    total_hours = c(12, 7),
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_jurisdiction_summary(matrix(1:4, ncol = 2)),
    "data frame|tibble",
    ignore.case = TRUE
  )

  expect_error(
    plot_jurisdiction_summary(jurisdiction_df, x_col = "missing_zone"),
    "missing required columns|missing_zone",
    ignore.case = TRUE
  )

  expect_error(
    plot_jurisdiction_summary(jurisdiction_df, y_col = "missing_hours"),
    "missing required columns|missing_hours",
    ignore.case = TRUE
  )
})

test_that("plot_jurisdiction_summary errors when y_col cannot be converted to numeric", {
  jurisdiction_df <- data.frame(
    jurisdiction = c("EEZ", "ABNJ"),
    total_hours = c("many", "few"),
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_jurisdiction_summary(jurisdiction_df),
    "numeric|numeric-like"
  )
})

test_that("plot_jurisdiction_summary errors when fill_col or facet_col is missing", {
  jurisdiction_df <- data.frame(
    bird_id = c("bird_A", "bird_B"),
    jurisdiction = c("EEZ", "ABNJ"),
    total_hours = c(12, 7),
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_jurisdiction_summary(jurisdiction_df, fill_col = "missing_fill"),
    "fill_col"
  )

  expect_error(
    plot_jurisdiction_summary(jurisdiction_df, facet_col = "missing_facet"),
    "facet_col"
  )
})
