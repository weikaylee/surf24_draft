# load libraries
library(terra)

# consts
years <- c(2003:2020)
input_path <- file.path("analysis", "data", "la_atmin")
output_path <- file.path("analysis", "processed", "heatwave_counts")

threshold <- 22

# create list of paths for input data
yearly_data <- list()
for (year in years) {
  yearly_data[[as.character(year)]] <- file.path(input_path, paste("LA_ATMIN", year, sep =
                                                                    "_"))
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
    # TODO, should i do > or >= threshold?
    if (!is.nan(pixel_vec[i]) && !is.nan(pixel_vec[i+1])) {
      if (is_unique_heatwave && (pixel_vec[i] >= threshold && pixel_vec[i+1] >= threshold)) {
        num_heatwaves <- num_heatwaves + 1
        is_unique_heatwave <- FALSE
      } 
      else if (pixel_vec[i] < threshold) {
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

# create dummy raster to test alg
ncols <- 2
nrows <- 2
# create layers
test_raster_l1 <- rast(nrows = nrows, ncols = ncols)
values(test_raster_l1) <- c(99, 99, 99, -99)
test_raster_l2 <- rast(nrows = nrows, ncols = ncols)
values(test_raster_l2) <- c(99, 99, 0, 99)
test_raster_l3 <- rast(nrows = nrows, ncols = ncols)
values(test_raster_l3) <- c(-99, 99, 0, -99)
test_raster_l4 <- rast(nrows = nrows, ncols = ncols)
values(test_raster_l4) <- c(99, 99, 99, 99)
test_raster_l5 <- rast(nrows = nrows, ncols = ncols)
values(test_raster_l5) <- c(99, 99, 99, -99)

test_stack <- c(test_raster_l1, test_raster_l2, test_raster_l3, test_raster_l4, test_raster_l5)
test_binned_raster <- app(test_stack, fun=get_heatwave_counts)
plot(test_binned_raster) # matches expected: 2, 1, 1, 0

# plot example raster 
ex_raster <- rast(file.path("analysis", "processed", "heatwave_counts", "LA_HEATWAVE_CNT_2003.tif"))
plot(ex_raster)

