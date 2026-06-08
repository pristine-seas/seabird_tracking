
assert_crs <- function(x, y) {
  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("Both x and y must be sf objects.")
  }

  if (is.na(sf::st_crs(x))) {
    stop("x has no CRS.")
  }

  if (is.na(sf::st_crs(y))) {
    stop("y has no CRS.")
  }

  if (sf::st_crs(x) != sf::st_crs(y)) {
    stop("CRS mismatch between x and y.")
  }

  invisible(TRUE)
}


label_day_night_period <- function(data,
                                   datetime_col = "timestamp",
                                   day_start = 6,
                                   night_start = 18) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!datetime_col %in% names(data)) {
    stop("datetime_col not found in data.")
  }

  dt <- as.POSIXct(data[[datetime_col]], tz = "UTC")

  if (all(is.na(dt))) {
    stop("Could not parse datetime column as POSIXct.")
  }

  hrs <- as.integer(format(dt, "%H"))

  data$diel_period <- ifelse(
    hrs >= day_start & hrs < night_start,
    "day",
    "night"
  )

  data
}
