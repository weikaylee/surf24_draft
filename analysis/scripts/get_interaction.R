# load libraries
library(raster)

# consts
years <- c(2003:2020)

base_paths <- list(
  max = file.path("analysis", "data", "la_atmax"),
  rng = file.path("analysis", "processed", "la_dtrng"), 
  max_rng = file.path("analysis", "processed", "la_max_rng")
)

# create list of vectors of temperature paths for each year
yearly_data <- list() 
for (year in years) {
  yearly_data[[as.character(year)]] <- c(
    MAX = file.path(base_paths[["max"]], paste("LA_ATMAX", year, sep="_")), 
    RNG = file.path(base_paths[["rng"]], paste("LA_DTRNG", year, sep="_")), 
    MAX_RNG = file.path(base_paths[["max_rng"]], paste("LA_MAX_RNG", year, sep="_"))
  )
}

# create dirs for max_rng (interaction) rasters
for (year in yearly_data) {
  path <- year[["MAX_RNG"]]
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

# get and store interaction raster
for (year in yearly_data[2:length(yearly_data)]) {
# for (year in yearly_data) {
  max_files <- list.files(year[["MAX"]], full.names=TRUE)
  rng_files <- list.files(year[["RNG"]], full.names=TRUE)
  max_rng_path <- year[["MAX_RNG"]]
  
  print(paste0("Writing to ", max_rng_path, "..."))
  for (i in 1:length(max_files)) { 
    max_file = max_files[i]
    rng_file = get_matching_file(rng_files, max_file) 
    
    if (is.null(max_file)) {
      next
    }
    
    filename <- sub("ATMAX", "MAX_RNG", basename(max_file))
    filepath <- file.path(max_rng_path, filename)
    
    # calculate and save avg raster
    max_raster <- raster(max_file)
    rng_raster <- raster(rng_file)
    max_rng_raster <- (max_raster) * (1 / rng_raster)
    
    # save raster
    writeRaster(max_rng_raster, filepath, overwrite=TRUE)
  }
}


# check that raster info is consistent
max_test <- raster(file.path("analysis", "data", "la_atmax", "LA_ATMAX_2004", "LA_ATMAX_2004_DOY001.tif"))
rng_test <- raster(file.path("analysis", "processed", "la_dtrng", "LA_DTRNG_2004", "LA_DTRNG_2004_DOY001.tif"))
max_rng_test <- raster(file.path("analysis", "processed", "la_max_rng", "LA_MAX_RNG_2004", "LA_MAX_RNG_2004_DOY001.tif"))

max_test <- setMinMax(max_test)
rng_test <- setMinMax(rng_test)
max_rng_test <- setMinMax(max_rng_test)

print(min_test)
print(max_test)
print(rng_test)

