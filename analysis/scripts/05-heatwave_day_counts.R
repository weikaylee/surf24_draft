# load libraries
library(terra)
library(sf)

# consts
years <- c(2003:2020)
input_path <- file.path("analysis", "data", "la_atmin")
output_path <- file.path("analysis", "processed", "heatwave_counts")
# shp_file <- file.path("analysis", "data", "la_county_boundary", "County_Boundary.shp")
shp_file <- file.path("analysis", "data", "la_census_shp", "LA_HRS_income.shp")
threshold_file <- file.path("analysis", "data", "summer_85p.tif")

# read shp file
la_shp <- read_sf(shp_file)
la_geo <- vect(la_shp[["geometry"]]) # convert SpatVector to pass into terra's extract()

# get threshold for la, by extracting from hannah's work (uses epa def of heatwave)
thresholds <- rast(threshold_file)
print("Getting heatwave threshold for LA county...")
extracted_vals <- extract(thresholds, la_geo, weights=TRUE) # returns df, with temp of corresponding cells and the fraction of how much cell is covered
sum_weights <- sum(extracted_vals$weight)
extracted_vals$temp_weights <- extracted_vals$weight / sum_weights
if (sum(extracted_vals$temp_weights) != 1) {
  stop(paste("Sum of temp weights is not 1. Actual sum is", sum_temp_weights))
}
extracted_vals$weighted_temp <- extracted_vals$summer_85p * extracted_vals$temp_weights
la_threshold <- sum(extracted_vals$weighted_temp, na.rm=TRUE) # relatively low, since using daily min temps (usually collected during nighttime)

# create list of paths for input data
yearly_data <- list()
for (year in years) {
  yearly_data[[as.character(year)]] <- file.path(input_path, paste("LA_ATMIN", year, sep = "_"))
}

# create output dir
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# get heatwave counts for vector of pixels 
get_heatwave_counts <- function(pixel_vec) {
  num_heatwaves <- 0
  is_unique_heatwave <- TRUE
  for (i in 1:(length(pixel_vec) - 1)) {
    if (!is.nan(pixel_vec[i]) && !is.nan(pixel_vec[i+1])) {
      if (is_unique_heatwave && (pixel_vec[i] > la_threshold && pixel_vec[i+1] > la_threshold)) {
        num_heatwaves <- num_heatwaves + 1
        is_unique_heatwave <- FALSE
      } 
      else if (pixel_vec[i] <= la_threshold) {
        is_unique_heatwave <- TRUE # reset
      }
    }
  } 
  return(num_heatwaves)
}

for (year in names(yearly_data)) {
  files <- list.files(yearly_data[[year]], full.names = TRUE)
  raster_stack <- rast(files) 
  raster_path_basename <- paste("LA_HEATWAVE_CNT", year, sep = "_")
  raster_path <- file.path(output_path, paste0(raster_path_basename, ".tif"))
  
  print(paste0("Getting heatwave counts for ", yearly_data[[year]], "..."))
  binned_raster <- app(raster_stack, fun=get_heatwave_counts) # equivalent to stack() in raster package
  writeRaster(binned_raster, raster_path, overwrite = TRUE)
}

# plot multiple rasters, observe any trends 
nrows = 3
ncols = 4
par(mfrow = c(nrows, ncols), mar = c(2, 2, 2, 2))
sequence <- seq(from = 1, by = 3, length.out = 6)
for (i in sequence) {
  year <- years[i]
  plot_raster_path <- file.path("analysis", "processed", "heatwave_counts", paste0("LA_HEATWAVE_CNT_", year, ".tif"))
  plot_raster <- rast(plot_raster_path)
  plot(plot_raster, main=basename(plot_raster_path))
  hist(plot_raster, main=basename(plot_raster_path))
}

# # create dummy raster to test alg
# ncols <- 2
# nrows <- 2
# # create layers
# test_raster_l1 <- rast(nrows = nrows, ncols = ncols)
# values(test_raster_l1) <- c(99, 99, 99, -99)
# test_raster_l2 <- rast(nrows = nrows, ncols = ncols)
# values(test_raster_l2) <- c(99, 99, 0, 99)
# test_raster_l3 <- rast(nrows = nrows, ncols = ncols)
# values(test_raster_l3) <- c(-99, 99, 0, -99)
# test_raster_l4 <- rast(nrows = nrows, ncols = ncols)
# values(test_raster_l4) <- c(99, 99, 99, 99)
# test_raster_l5 <- rast(nrows = nrows, ncols = ncols)
# values(test_raster_l5) <- c(99, 99, 99, -99)
# 
# test_stack <- c(test_raster_l1, test_raster_l2, test_raster_l3, test_raster_l4, test_raster_l5)
# test_binned_raster <- app(test_stack, fun=get_heatwave_counts)
# plot(test_binned_raster) # matches expected: 2, 1, 1, 0
