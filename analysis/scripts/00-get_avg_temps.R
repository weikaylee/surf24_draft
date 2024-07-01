# load libraries
library(raster)

# consts
years <- c(2013:2020)

base_paths <- list(
  min = file.path("analysis", "data", "la_atmin"),
  max = file.path("analysis", "data", "la_atmax"),
  avg = file.path("analysis", "processed", "la_atavg")
)

# create list of vectors of temperature paths for each year
yearly_data <- list() 
for (year in years) {
  yearly_data[[as.character(year)]] <- c(
    MIN = file.path(base_paths[["min"]], paste("LA_ATMIN", year, sep="_")), 
    MAX = file.path(base_paths[["max"]], paste("LA_ATMAX", year, sep="_")), 
    AVG = file.path(base_paths[["avg"]], paste("LA_ATAVG", year, sep="_"))
  )
}

# create dirs for avg rasters
for (year in yearly_data) {
  path <- year[["AVG"]]
  if (!dir.exists(path)) {
    dir.create(path, recursive=TRUE)
  }
}

# get and store avg raster
for (year in yearly_data) {
  min_files <- list.files(year[["MIN"]], full.names=TRUE)
  max_files <- list.files(year[["MAX"]], full.names=TRUE)
  avg_path <- year[["AVG"]]
  
  print(paste0("Writing to ", avg_path, "..."))
  for (i in seq_along(min_files)) {
    min_file = min_files[i]
    max_file = max_files[i]
    
    filename <- sub("ATMIN", "ATAVG", basename(min_file))
    filepath <- file.path(avg_path, filename)
    
    # verify that the files match (are from the same date)
    min_matches <- regmatches(basename(min_file), gregexpr("\\d", basename(min_file)))
    max_matches <- regmatches(basename(max_file), gregexpr("\\d", basename(max_file)))
    if (!identical(min_matches, max_matches)) {
      stop("Files do not match") # no doy 299 in 2013 min (will have one less raster for avg temps)
    }
    
    # calculate and save avg raster
    min_raster <- raster(min_file)
    max_raster <- raster(max_file)
    avg_raster <- (min_raster + max_raster) / 2
    
    # save raster
    writeRaster(avg_raster, filepath, overwrite=TRUE)
  }
}

# check if calculation worked 
min_test <- raster(file.path("analysis", "data", "la_atmin", "LA_ATMIN_2003", "LA_ATMIN_2003_DOY001.tif"))
max_test <- raster(file.path("analysis", "data", "la_atmax", "LA_ATMAX_2003", "LA_ATMAX_2003_DOY001.tif"))
avg_test <- raster(file.path("analysis", "processed", "la_atavg", "LA_ATAVG_2003", "LA_ATAVG_2003_DOY001.tif"))

min_test <- setMinMax(min_test)
max_test <- setMinMax(max_test)
avg_test <- setMinMax(avg_test)

print(min_test)
print(max_test)
print(avg_test)
