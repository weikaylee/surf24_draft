# load libraries
library(terra)
library(sf)
library(tidyverse)

raw_data_path <- file.path("analysis", "data")
zipcodes_raw <- file.path(raw_data_path,
                          "la_zipcode_boundaries",
                          "LA_County_ZIP_Codes.shp") %>% read_sf %>% select(ZIPCODE, geometry)
sf_use_s2(FALSE) # switch off spherical geo (s2), to avoid merging and area errors

suppressMessages({
  # "although coordinates are longitude/latitude, st_union, st_intersection assume that they are planar"
  # merge geos of zipcodes with multiple geos
  zipcodes_merged <- zipcodes_raw %>% group_by(ZIPCODE) %>% summarize(geometry = st_union(geometry))
  
  # drop zipcodes with < 80% coverage
  raw_rast <- file.path(raw_data_path,
                        "la_atmax",
                        "LA_ATMAX_2003",
                        "LA_ATMAX_2003_DOY001.tif") %>% rast
  raw_rast_sf <- raw_rast %>% as.polygons %>% st_as_sf
  dropped_zipcodes <- zipcodes_merged[FALSE, ] # df with just colnames, no rows
  
  for (i in 1:nrow(zipcodes_merged)) {
    zipcode <- zipcodes_merged[i, ]
    suppressWarnings ({
      # "attribute variables are assumed to be spatially constant throughout all geometries"
      tot_area <- st_area(zipcode)
      intersect_area <- st_intersection(raw_rast_sf, zipcode) %>% st_area %>% sum
    })
    if (as.numeric(intersect_area / tot_area) < 0.8) {
      dropped_zipcodes <- rbind(dropped_zipcodes, zipcode)
    }
  }
})

zipcodes_processed <- setdiff(zipcodes_merged, dropped_zipcodes)
# plot(raw_rast)
# plot(zipcodes_processed, add=TRUE)
