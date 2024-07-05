# load libraries
library(raster)

# consts
years <- c(2003:2020)

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

# function to find file in file_list that matches doy of file_to_match
# (search file_list linearly in case of large mismatches between dirs)
get_matching_file <- function(file_list, file_to_match) {
  doy_to_match <- regmatches(basename(file_to_match), gregexpr("DOY(//d+)", basename(file_to_match)))
  for (i in 1:length(file_list)) {
    file <- file_list[i]
    file_doy <- regmatches(basename(file), gregexpr("DOY(//d+)", basename(file)))
    if (identical(doy_to_match, file_doy)) {
      return(file)
    }
  }
  return(NULL)
}

# get and store avg raster
for (year in yearly_data) {
  min_files <- list.files(year[["MIN"]], full.names=TRUE)
  max_files <- list.files(year[["MAX"]], full.names=TRUE)
  avg_path <- year[["AVG"]]
  
  print(paste0("Writing to ", avg_path, "..."))
  for (i in 1:length(min_files)) { 
    min_file = min_files[i]
    max_file = get_matching_file(max_files, min_file) 
    
    if (is.null(max_file)) {
      next
    }
    
    filename <- sub("ATMIN", "ATAVG", basename(min_file))
    filepath <- file.path(avg_path, filename)
    
    # calculate and save avg raster
    min_raster <- raster(min_file)
    max_raster <- raster(max_file)
    avg_raster <- (min_raster + max_raster) / 2
    
    # save raster
    writeRaster(avg_raster, filepath, overwrite=TRUE)
  }
}

# verify that calculation worked 
# first, check that we have the expected number of files 
# (minimum of max temp files and min temp files) 
for (year in names(yearly_data)) {
  year_paths <- yearly_data[[year]]
  min_files <- list.files(year_paths[["MIN"]])
  print(length(min_files))
  max_files <- list.files(year_paths[["MAX"]])
  avg_files <- list.files(year_paths[["AVG"]])
  num_files <- min(length(min_files), length(max_files))
  actual_files <- length(avg_files)
  
  if (num_files != actual_files) {
    stop(paste("Number of files do not match in", year))
  }
}

# check that raster info is consistent
min_test <- raster(file.path("analysis", "data", "la_atmin", "LA_ATMIN_2004", "LA_ATMIN_2004_DOY001.tif"))
max_test <- raster(file.path("analysis", "data", "la_atmax", "LA_ATMAX_2004", "LA_ATMAX_2004_DOY001.tif"))
avg_test <- raster(file.path("analysis", "processed", "la_atavg", "LA_ATAVG_2004", "LA_ATAVG_2004_DOY001.tif"))

min_test <- setMinMax(min_test)
max_test <- setMinMax(max_test)
avg_test <- setMinMax(avg_test)

print(min_test)
print(max_test)
print(avg_test)

