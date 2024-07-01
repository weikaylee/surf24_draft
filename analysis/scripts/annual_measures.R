# load libraries
library(raster)
library(sf)
library(terra)
library(Dict)

# TODO: update everything later to get stats for *all* years

# import data 
# yearly_data <- dict(
#   "2003" = dict(
#     min_path="./analysis/data/la/la_atmin/LA_ATMIN_2003/",
#     max_path="./analysis/data/la/la_atmax/LA_ATMAX_2003/",
#     avg_path="./analysis/data/la/la_atavg/LA_ATAVG_2003/"
#   ),
#   "2004" = dict(
#     min_path="./analysis/data/la/la_atmin/LA_ATMIN_2004/",
#     max_path="./analysis/data/la/la_atmax/LA_ATMAX_2004/",
#     avg_path="./analysis/data/la/la_atavg/LA_ATAVG_2004/"
#   )
# )

# need list, otherwise harder to access elements in nested vector properly
yearly_data <- list(
  "2003" = c(MIN="./analysis/data/la/la_atmin/LA_ATMIN_2003/",
           MAX="./analysis/data/la/la_atmax/LA_ATMAX_2003/",
           AVG="./analysis/data/la/la_atavg/LA_ATAVG_2003/"),
  "2004" = c(MIN="./analysis/data/la/la_atmin/LA_ATMIN_2004/",
             MAX="./analysis/data/la/la_atmax/LA_ATMAX_2004/",
             AVG="./analysis/data/la/la_atavg/LA_ATAVG_2004/")
)

# # create folders for avg_path
for (year in yearly_data) {
  path <- year["AVG"]
  if (!dir.exists(path)) {
    print(paste("Creating folder at:", path))
    dir.create(path)
  }
}

# TODO, update this to match above strucutre...
# create raster of avg temps, using min and max
for (year in yearly_data) {
  min_files <- list.files(year["min"], full.names=TRUE)
  max_files <- list.files(year["max"], full.names=TRUE)

  for (i in seq_along(min_files)) {
    min_file = min_files[i]
    max_file = max_files[i]

    # check if file has already been processed; if yes, then skip (rob delete this in final ver?)
    filename <- sub("ATMIN", "ATAVG", basename(min_file))
    filepath <- paste(avg_path, filename, sep="")
    if (!file.exists(filepath)) {
      # check if the files match (are from the same date)
      min_matches <- regmatches(basename(min_file), gregexpr("\\d", basename(min_file)))
      max_matches <- regmatches(basename(max_file), gregexpr("\\d", basename(max_file)))
      if (!identical(min_matches, max_matches)) {
        stop("Files do not match")
      }

      min_raster <- setMinMax(raster(min_file))
      max_raster <- setMinMax(raster(max_file))
      avg_raster <- (min_raster + max_raster) / 2

      # save raster
      print(paste("Writing:", filepath))
      writeRaster(avg_raster, filepath)
    }
  }
}


# create daily temp bins for each pixel location 
# TODO ask when these data was taken: in nighttime, daytime, etc
# TODO also ask how this data was processed (eg, how were the "good" / nonocean / noncloud pixels determined), what satellite it was taken from!
for (year in names(yearly_data)) {
  for (path in names(yearly_data[[year]])) { 
    # get all files in that dir, and create raster of bins
    print("Getting pixel values...")
    files <- list.files(yearly_data[[year]][[path]], full.names=TRUE)
    stack <- stack(files, quick=TRUE)
    pixel_vals <- getValues(stack)
    print(class(pixel_vals))
    print(class(pixel_vals[1,]))
    
    for (i in seq_along(pixel_vals)) {
      print(pixel_vals[i, ])
    }
    
    # create raster to store bins in 
    print("Creating raster...")
    ref_raster <- raster(files[1])
    bin_raster <- raster(nrows=ref_raster@nrows, 
                         ncols=ref_raster@ncols, 
                         ext=ref_raster@extent, 
                         crs=crs(ref_raster))
    
    # iterate through matrix of pixel vals
    for (i in seq_along(pixel_vals)) {
      # create bins (dataframe)
      print("Creating bins...")
      init_vals <- matrix(0, 1, 42) # matrix of 42 zeroes in one row (one for each bin)
      bin <- data.frame(init_vals, stringAsFactors=FALSE)
      # for 0-1 C, inclusive of 0, exclusive of 1, and so on
      names(bin) <- c("< 0 C", "0-1 C", "1-2 C", "2-3 C", "3-4 C", "4-5 C", 
                      "5-6 C", "6-7 C", "7-8 C", "8-9 C", "9-10 C", "10-11 C",
                      "11-12 C", "12-13 C", "13-14 C", "14-15 C", "15-16 C", 
                      "16-17 C", "17-18 C", "18-19 C", "19-20 C", "20-21 C", 
                      "21-22 C", "22-23 C", "23-24 C", "24-25 C", "25-26 C", 
                      "26-27 C", "27-28 C", "28-29 C", "29-30 C", "30-31 C", 
                      "31-32 C", "32-33 C", "33-34 C", "34-35 C", "35-36 C", 
                      "36-37 C", "37-38 C", "38-39 C", "39-40 C", "> 40 C")
      
      print("Assigning bins...")
      for (j in pixel_vals[i]) {
        if (!is.na(j)) {
          print("issues here...")
          if (j < 0) {
            bin[1] <- (bin[1] + 1)
          }
          else if (j >= 40) {
            bin[42] <- (bin[42] + 1)
          }
          else {
            idx <- ceiling(j %% 40) + 1
            bin[idx] <- (bin[idx] + 1)
          }
        }
      }
      
      # store bin in raster
      cbind(year=year, bin) # add column with current year, at v front
      bin_raster[i] <- bin
    }
    
    # save / write raster 
    folder_path <- paste(paste("./analysis/processed/", year, sep=""), "daily_temp_bins/") # TODO how to make the dir creation stuff reproducible?
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive=TRUE)
    }
    raster_path <- paste(folder_path, "LA_", path, "_DAILY_TEMP_BINS_", year, sep="")
    print(paste("Writing raster at", raster_path))
    writeRaster(bin_raster, raster_path)
  }
}
