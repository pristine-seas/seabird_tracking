------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# Shearwater

<!-- badges: start -->

[![R-CMD-check](https://github.com/cooper61204/Shearwater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cooper61204/Shearwater/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Shearwater is an R package for analyzing Wedge-tailed Shearwater GPS tracking data. The package helps researchers move from raw GPS fixes to cleaned tracks, trip segmentation, movement metrics, space-use estimates, and fisheries overlap summaries.

The package was designed for seabird movement ecology and conservation workflows, especially projects that use GPS tracking data to understand foraging behavior, home ranges, and overlap with human activities such as fishing effort. Main features

Shearwater provides tools to:

Import and standardize GPS tracking data Clean duplicate fixes and filter unrealistic movement speeds Regularize GPS tracks to consistent time intervals Identify colony visits and segment trips Classify trip phases such as commuting and foraging Calculate movement metrics such as trip distance, path length, trip duration, and maximum distance from colony Estimate space-use areas using kernel utilization distributions Extract home-range and core-use isopleths Analyze fisheries overlap and gear-specific risk Export spatial outputs for mapping and conservation planning

## Installation

You can install the development version of Shearwater from [GitHub](https://github.com/) with:

# install.packages("devtools") devtools::install_github("YOUR-USERNAME/Shearwater")

Then load the package library(Shearwater)

``` r
# install.packages("pak")
pak::pak("cooper61204/Shearwater")
```

## Example

The package includes an example Wedge-tailed Shearwater GPS dataset which was provided by National Geographic stored in:

system.file("extdata", "example_wtsh_data.csv", package = "Shearwater")

You can load it with:

gps_raw \<- read.csv( system.file("extdata", "example_wtsh_data.csv", package = "Shearwater"), stringsAsFactors = FALSE )

head(gps_raw)

This is a basic example which shows you how to solve a common problem:

``` r
library(Shearwater)
library(dplyr)
library(geosphere)

gps <- gps_raw %>% mutate( track_id = "bird_001", datetime_gmt = as.POSIXct( paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "UTC" ), 
longitude = Longitude, 
latitude = Latitude, 
altitude = Altitude, 
speed = Speed, 
course = Course, 
fix_type = Type, 
step_distance = Distance, 
essential = Essential ) %>% 
arrange(track_id, datetime_gmt) head(gps)

## basic example code
```

The standardized object now contains the original spreadsheet columns, plus the columns expected by the movement-analysis functions:

track_id datetime_gmt longitude latitude altitude speed course fix_type step_distance essential

Next, define the colony location. In this example, the first GPS point is used as the colony location. In a real analysis, this should come from known colony coordinates.

``` r
colony_coords <- c( lon = gps$longitude[1], lat = gps$latitude[1] ) 

colony_coords
```

Then calculate each point's distance from the colony and create an at_colony flag. Here, points within 75 meters of the colony are labeled as colony points.

``` r
gps <- gps %>%
  mutate(
    dist_to_colony_m = geosphere::distHaversine(
      cbind(longitude, latitude),
      matrix(
        c(colony_coords["lon"], colony_coords["lat"]),
        nrow = n(),
        ncol = 2,
        byrow = TRUE
      )
    ),
    at_colony = dist_to_colony_m <= 75
  )

table(gps$at_colony)
```

### **Segment trips**

Trips can be identified using segment_trips(). This function uses the at_colony column to determine when the bird leaves and returns to the colony.

``` r
segmented <- segment_trips(
  gps,
  bird_id_col = "track_id",
  datetime_col = "datetime_gmt",
  colony_flag_col = "at_colony",
  trip_id_col = "trip_id"
)

head(segmented)
```

Rows where the bird is at the colony receive NA for trip_id, while points away from the colony are assigned numbered trip IDs.

Calculate movement metrics

After trips are segmented, the package can calculate trip-level movement metrics using the standardized coordinate and datetime columns.

``` r
trip_distance <- calc_trip_distance(
  trip_data = segmented,
  bird_id_col = "track_id",
  trip_id_col = "trip_id",
  datetime_col = "datetime_gmt",
  lon_col = "longitude",
  lat_col = "latitude"
)

trip_duration <- calc_trip_duration(
  trip_data = segmented,
  bird_id_col = "track_id",
  trip_id_col = "trip_id",
  datetime_col = "datetime_gmt",
  units = "hours"
)

path_length <- calc_path_length(
  track_data = segmented,
  bird_id_col = "track_id",
  trip_id_col = "trip_id",
  datetime_col = "datetime_gmt",
  lon_col = "longitude",
  lat_col = "latitude"
)

max_distance <- calc_max_distance_from_colony(
  trip_data = segmented,
  colony_coords = colony_coords,
  bird_id_col = "track_id",
  trip_id_col = "trip_id",
  lon_col = "longitude",
  lat_col = "latitude"
)

trip_metrics <- trip_distance %>%
  left_join(trip_duration, by = c("track_id", "trip_id")) %>%
  left_join(path_length, by = c("track_id", "trip_id")) %>%
  left_join(max_distance, by = c("track_id", "trip_id"))

trip_metrics
```

These metrics describe different aspects of movement:

calc_trip_distance(): calculates straight-line distance from the first point of a trip to the last point. calc_trip_duration(): calculates how long each trip lasted. calc_path_length(): calculates the total distance traveled along the GPS path. calc_max_distance_from_colony(): calculates the farthest distance reached from the colony.

### **Space-use analysis**

To run spatial functions, convert the standardized GPS table into an sf object using the standardized longitude and latitude columns.

``` r
tracks_sf <- sf::st_as_sf(
  segmented,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

tracks_sf <- tracks_sf %>%
  mutate(time = datetime_gmt)

kud <- calculate_kud(
  tracks = tracks_sf,
  ref = "href"
)

isopleths <- get_isopleths(
  kud = kud,
  levels = c(50, 95)
)

isopleths <- calculate_area_metrics(isopleths)

isopleths
```

The 50 percent isopleth can be interpreted as a core-use area, while the 95 percent isopleth represents a broader home-range estimate.

### **Fisheries overlap analysis**

Shearwater also includes tools for analyzing overlap between seabird tracks and fisheries effort data.

``` r
fisheries_clean <- standardize_fishing_effort(
  fisheries_data,
  effort_col = "effort",
  gear_col = "gear"
)

fisheries_sf <- as_fisheries_sf(
  fisheries_clean,
  lon_col = "longitude",
  lat_col = "latitude",
  crs = 4326
)

joined <- join_tracks_to_fishing_grid(
  track_data = tracks_sf,
  fisheries_data = fisheries_sf,
  join_type = "nearest"
)

overlap <- calc_fisheries_overlap(
  joined_data = joined,
  track_id_col = "track_id",
  effort_col = "effort_std",
  gear_col = "gear"
)

overlap
```

These outputs can be used to identify where bird movement overlaps with fishing activity and which gear types may contribute most to potential risk.

### **Vignettes**

The package includes vignettes that walk through different parts of the analysis workflow:

browseVignettes("Shearwater")

Suggested vignettes include:

Data preparation Trip segmentation Movement metrics Fisheries overlap and conservation summaries Development

To run tests:

devtools::test()

To rebuild documentation:

devtools::document()

To check the package:

devtools::check()
