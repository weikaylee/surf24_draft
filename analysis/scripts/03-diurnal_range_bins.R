# load libraries
library(terra)

# consts
years <- c(2003:2020)
temp_bins <- c(-Inf, 0:40, Inf) # QUESTION: is the range for bins still ok, for diurnal range temps? since i imagine the actual range of values would be different / smaller? but also i guess it might be negligible? or as long as everything looks normal it's okay?
bin_labels <- c("Inf_0", paste0(seq(0, 39), "_", seq(1, 40)), "40_Inf")

base_path <- file.path("analysis", "processed", "la_dtrng")
output_path <- file.path("analysis", "processed", "diurnal_range_bins")

# create list of paths of input data for each year
yearly_data <- list()
for (year in years) {
  yearly_data[[as.character(year)]] <- file.path(base_path, paste("LA_DTRNG", year, sep =
                                                                    "_"))
}

# create output dir
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# sort vector of pixel values into (-Inf, 0), [0, 1), [1, 2), ... , [40, Inf) bins
get_temp_bins <- function(pixel_vec) {
  bins <- cut(
    pixel_vec,
    breaks = temp_bins,
    include.lowest = TRUE,
    right = FALSE
  )
  bins <- table(bins, useNA = "no") # must get rid of NAs
  return(bins)
}

# get and save raster brick for all paths and years, where each
# layer of the raster brick represents a different bin and where each pixel
# value represents the number of days in that bin at that location
for (year in names(yearly_data)) {
  files <- list.files(yearly_data[[year]], full.names = TRUE)
  raster_stack <- rast(files) # equivalent to stack() in raster package
  raster_path_basename <- paste("LA_DTRNG_BINS", year, sep = "_")
  raster_path <- file.path(output_path, paste0(raster_path_basename, ".tif"))
  
  print(paste0("Getting binned raster for ", yearly_data[[year]], "..."))
  # apply fun on vector of pixel vals across the spatraster stack and save as spatraster brick
  binned_raster <- app(raster_stack, fun = get_temp_bins) # equivalent to stack() in raster package
  
  # name layers for visualizations; better to use terra for naming layers
  # (in raster, layer names stay as default when loading the file later on)
  names(binned_raster) <- paste(raster_path_basename, bin_labels, sep =
                                  "_")
  
  # save raster brick
  writeRaster(binned_raster, raster_path, overwrite = TRUE)
}

# verify that bins are correct; check if day counts across the raster brick
# for each pixel location sum to 365 (or the number of days with available data -- here, either 364 or 365)
for (year in names(yearly_data)) {
  # get total num days with available data (expected day count)
  num_days <- length(list.files(yearly_data[[year]]))
  
  # get corresponding raster brick
  raster_path <- file.path(output_path, paste0(paste("LA_DTRNG_BINS", year, sep =
                                                       "_"), ".tif"))
  binned_raster <- rast(raster_path)
  
  # get raster layer, where each pixel val is the result of fun
  day_counts <- app(binned_raster, fun = sum)
  
  # check if all pixel vals in raster layer = expected num days
  num_days_raster <- (day_counts == num_days) # returns a raster layer consisting of 1s (true) and 0s (false) for each pixel
  
  # check that resulting raster consists of all 1s
  if (all(values(num_days_raster) != 1)) {
    stop(paste(
      "Actual total number of days across bins does not match expected for",
      year
    ))
  }
}

# check that distribution and trends of bin counts, pixel vals match (are roughly normal)
test_files <- list.files(file.path("analysis", "processed", "la_dtrng", "LA_DTRNG_2003"),
                         full.names = TRUE)
test_stack <- rast(test_files)
test_brick <- rast(
  file.path(
    "analysis",
    "processed",
    "diurnal_range_bins",
    "LA_DTRNG_BINS_2003.tif"
  )
)
names(test_brick) <- paste("LA_DTRNG_BINS_2003", bin_labels, sep="_")
plot(test_stack)
plot(test_brick)


# create layout
nrows <- 4
ncols <- 4
offset <- 10
ncells_half = (nrows * ncols) / 2
par(mfcol = c(nrows, ncols), mar = c(2, 2, 2, 2))

# plot distr of pixel values
for (i in 1:ncells_half) {
  hist(test_stack[[i + offset]], xlab = "Value (C)", ylab = "Frequency")
}

# plot distr of bin counts
for (i in 1:ncells_half) {
  hist(test_brick[[i + offset]], xlab = "Value (C)", ylab = "Frequency")
}
