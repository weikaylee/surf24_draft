# load libraries
library(raster)
library(terra)
library(ggplot2)
library(gridExtra)
library(ggplotify)


get_temp_bins <- function(pixel_vec) {
  temp_bins <- c(-Inf, 0:40, Inf)
  bins <- cut(pixel_vec, breaks=temp_bins, include.lowest=TRUE, right=FALSE)
  bins <- table(bins, useNA="no")
  return(bins)
}

dir_path <- "./analysis/data/la_atmin/LA_ATMIN_2003/"
files <- list.files(dir_path, full.names=TRUE)
raster_stack <- stack(files, quick=TRUE)
binned_raster <- calc(raster_stack, fun=get_temp_bins) # applies fun to vector of pixel values across entire stack, saves output as rasterbrick
binned_raster <- rast(binned_raster) # from terra, lets us save layer names 

raster_path <- "C:/Users/scarl/Dropbox/surf24/analysis/processed/2003/daily_temp_bins/test.tif"
bin_labels <- c("Inf_0", paste0(seq(0, 39), "_", seq(1, 40)), "40_Inf")
bin_labels <- paste(basename(raster_path), bin_labels, sep="_")
names(binned_raster) <- bin_labels

writeRaster(binned_raster, raster_path, overwrite=TRUE) # uses terra's ver

# testing
test <- brick("C:/Users/scarl/Dropbox/surf24/analysis/processed/2003/daily_temp_bins/test.tif")
# get distribution of most pixels
par(mfrow = c(3, 3))
for (i in 1:9) {
  hist(raster_stack[[i]], xlab="Value (C)", ylab="Frequency")
}

for (i in 1:9) {
  plot(test[[i]], xlab="Value (C)", ylab="Frequency")
}

hist(test)

plot(raster_stack) # get distribution of where the most pixels are, in the first 16 layers
plot(hist(test)) # verify that there are greater count nums at those same pixels
# TODO confirm that this testing works, and then implement back int eh other file
# also things to ask hannah: am out of dropbox storage lmao? reproducibility / did i organize my data right?