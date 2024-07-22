# load libraries
library(terra)

# consts
years <- c(2003)
temp_bins <- c(-Inf, 0:40, Inf)
bin_labels <- c("Inf_0", paste0(seq(0, 39), "_", seq(1, 40)), "40_Inf")

base_paths <- list(
  min = file.path("analysis", "data", "la_atmin"),
  max = file.path("analysis", "data", "la_atmax"),
  avg = file.path("analysis", "processed", "la_atavg"),
  output = file.path("analysis", "processed", "daily_temp_bins")
)

# create list of vectors of temperature paths for each year
yearly_data <- list() 
for (year in years) {
  # get input data
  yearly_data[[as.character(year)]] <- c(
    MIN = file.path(base_paths[["min"]], paste("LA_ATMIN", year, sep="_")), 
    MAX = file.path(base_paths[["max"]], paste("LA_ATMAX", year, sep="_")), 
    AVG = file.path(base_paths[["avg"]], paste("LA_ATAVG", year, sep="_"))
  )
  
  # get and create dirs for output data
  output_path <- file.path(base_paths[["output"]], year)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive=TRUE)
  }
}

# sort vector of pixel values into (-Inf, 0), [0, 1), [1, 2), ... , [40, Inf) bins
get_temp_bins <- function(pixel_vec) {
  bins <- cut(pixel_vec, breaks=temp_bins, include.lowest=TRUE, right=FALSE) 
  bins <- table(bins, useNA="no") # must get rid of NAs
  return(bins)
}

# get and save raster brick for all temperature paths and years, where each 
# layer of the raster brick represents a different bin and where each pixel 
# value represents the number of days in that bin at that location
for (year in names(yearly_data)) {
  for (path in names(yearly_data[[year]])) {
    files <- list.files(yearly_data[[year]][[path]], full.names=TRUE)
    folder_path <- file.path(base_paths[["output"]], year)
    raster_stack <- rast(files)
    raster_basename <- paste("LA_DAILY_TEMP_BINS", path, year, sep="_")
    raster_path <- file.path(folder_path, paste0(raster_basename, ".tif"))

    print(paste0("Getting binned raster for ", yearly_data[[year]][[path]], "..."))
    # apply fun on vector of pixel vals across the raster stack; save output as raster brick
    binned_raster <- app(raster_stack, fun=get_temp_bins)
    
    # name layers for visualizations; better to use terra for naming layers
    # (in raster, layer names stay as default when loading the file later on)
    names(binned_raster) <- paste(raster_basename, bin_labels, sep="_")

    # save raster brick
    writeRaster(binned_raster, raster_path, overwrite=TRUE)
  }
}

# verify that bins are correct; check if day counts across the raster brick
# for each pixel location sum to 365 (or the number of days with available data -- here, either 364 or 365)
for (year in names(yearly_data)) {
  for (path in names(yearly_data[[year]])) {
    # get total num days with available data (expected day count)
    num_days <- length(list.files(yearly_data[[year]][[path]]))
    
    # get corresponding raster brick
    binned_filename <- paste("LA_DAILY_TEMP_BINS", path, year, sep="_")
    binned_filename <- paste0(binned_filename, ".tif")
    binned_raster_path <- file.path(base_paths[["output"]], year, binned_filename)
    binned_raster <- rast(binned_raster_path)

    # get raster layer, where each pixel val is the result of fun
    day_counts <- app(binned_raster, fun=sum) 

    # check if all pixel vals in raster layer = expected num days
    num_days_raster <- (day_counts == num_days) # returns a raster layer consisting of 1s (true) and 0s (false) for each pixel

    # check that resulting raster consists of all 1s
    if (all(values(num_days_raster) != 1)) {
      stop(paste("Actual total number of days across bins does not match expected for", path, year))
    }
  }
}

# check that distribution and trends of bin counts, pixel vals match (are roughly normal)
test_files <- list.files(file.path("analysis", "data", "la_atmin", "LA_ATMIN_2003"), full.names=TRUE)
test_stack <- rast(test_files)
test_brick <- rast(file.path(base_paths[["output"]], "2003", "LA_DAILY_TEMP_BINS_MIN_2003.tif"))
plot(test_brick)
plot(test_stack)
# create layout
nrows <- 4
ncols <- 4
offset <- 10
ncells_half = (nrows * ncols) / 2
par(mfcol = c(nrows, ncols), mar=c(2, 2, 2, 2))

# plot distr of pixel values 
for (i in 1:ncells_half) {
  hist(test_stack[[i + offset]], xlab="Value (C)", ylab="Frequency")
}

# plot distr of bin counts 
for (i in 1:ncells_half) {
  hist(test_brick[[i + offset]], xlab="Value (C)", ylab="Frequency")
}
