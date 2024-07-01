# load libraries
library(raster)
library(terra)

# consts
years <- c(2003:2004)
temp_bins <- c(-Inf, 0:40, Inf)
bin_labels <- c("-Inf_0", paste0(seq(0, 39), "_", seq(1, 40)), "40_Inf")

base_paths <- list(
  min = file.path("analysis", "data", "la_atmin"),
  max = file.path("analysis", "data", "la_atmax"),
  avg = file.path("analysis", "data", "la_atavg")
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

# sort vector of pixel values into (-Inf, 0), [0, 1), [1, 2), ... , [40, Inf) bins
get_temp_bins <- function(pixel_vec) {
  bins <- cut(pixel_vec, breaks=temp_bins, include.lowest=TRUE, right=FALSE) 
  bins <- table(bins, useNA="no")
  return(bins)
}

# get and save raster brick for all temperature paths and years, where each 
# layer of the raster brick represents a different bin and where each pixel 
# value represents the number of days in that bin at that location
for (year in names(yearly_data)) {
  folder_path <- file.path("analysis", "processed", year, "daily_temp_bins") # should these be created beforehand..? (eg in run.do?? / master script that runs entire analysis?)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive=TRUE)
  }
  
  for (path in names(yearly_data[[year]])) {
    files <- list.files(yearly_data[[year]][[path]], full.names=TRUE)
    raster_stack <- stack(files, quick=TRUE)
    raster_path <- file.path(folder_path, paste("LA_DAILY_TEMP_BINS", path, year, sep="_"))

    print(paste0("Getting binned raster for ", yearly_data[[year]][[path]], "..."))
    # apply fun on vector of pixel vals across the raster stack; save output as raster brick
    binned_raster <- calc(raster_stack, fun=get_temp_bins)
    
    # convert to terra raster to save layer names, for visualizations 
    # (otherwise, layer names stay as default when loading the file later)
    binned_raster <- rast(binned_raster) 
    bin_labels_raster <- paste(basename(raster_path), bin_labels, sep="_")
    names(binned_raster) <- bin_labels_raster
    
    # save raster brick
    raster_path <- paste0(raster_path, ".tif")
    writeRaster(binned_raster, raster_path, overwrite=TRUE)
  }
}

# check if bins make sense visually
test_files <- list.files("analysis/data/la_atmin/LA_ATMIN_2003", full.names=TRUE)
test_stack <- stack(test_files, quick=TRUE)
test_brick <- brick("analysis/processed/2003/daily_temp_bins/LA_DAILY_TEMP_BINS_MIN_2003.tif")

# create 4 by 4 layout, for 16 total plots at a time
par(mfrow = c(4, 4))

# get distribution of pixel vals for first 16 days, see which values have highest freq
for (i in 1:9) {
  hist(test_stack[[i]], xlab="Value (C)", ylab="Frequency")
}

# get distribution of bin counts, verify that they match above graphs
plot(test_brick)

