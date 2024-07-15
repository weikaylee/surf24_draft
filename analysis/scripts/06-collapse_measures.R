# load libraries
library(terra)
library(sf)
library(dplyr)


# open and process zipcode shp files
zipcodes_path <- file.path("analysis", "data", "la_zip_code_boundaries", "LA_County_ZIP_Codes.shp")
zipcodes_shp <- read_sf(zipcodes_path)
zipcodes_id_geo <- zipcodes_shp[, c("ZIPCODE", "geometry")]


# # check that all zipcodes have exactly one geo (they dont)
# duplicates <- duplicated(zipcodes_id_geo$ZIPCODE) 
# print(TRUE %in% duplicates)


# merge geos of zipcodes with multiple geos 
print("Processing data...")
sf_use_s2(FALSE) # switch off spherical geo (s2), to avoid summarize() errors
zipcodes_processed <- zipcodes_id_geo %>% group_by(ZIPCODE) %>% summarize(geometry = st_union(geometry))
# zipcodes_grouped <- group_by(zipcodes_id_geo, ZIPCODE) # group by ZIPCODE col
# zipcodes_processed <- summarize(zipcodes_grouped, geometry = st_union(geometry)) # summarize each group to one row using st_union


# # verify that there are no more duplicates 
# duplicates <- duplicated(zipcodes_processed$ZIPCODE) 
# print(TRUE %in% duplicates)


# load all vars
years <- c(2003:2020)
processed_base_path <- file.path("analysis", "processed")
var_base_paths <- list(
  daily_temp_bins = file.path(processed_base_path, "daily_temp_bins"),
  diurnal_range_bins = file.path(processed_base_path, "diurnal_range_bins"),
  heatwave_counts = file.path(processed_base_path, "heatwave_counts")
)


# get list of lists of files for each year in base_path; is filename param is
# missing, then assume data is stored in nested dirs
get_data <- function(base_path, filename) {
  data_list <- list()
  for (year in years) {
    file_path <- file.path(base_path, year)
    files <- list.files(file_path, full.names=TRUE)
    if (!missing(filename)) {
      file_path <- file.path(base_path, paste0(filename, year, ".tif"))
      files <- list(file_path)
    }
    data_list[[as.character(year)]] <- files
  }
  return(data_list)
}


var_data <- list(
  daily_temp_bins = get_data(var_base_paths[["daily_temp_bins"]]), 
  diurnal_range_bins =  get_data(var_base_paths[["diurnal_range_bins"]], "LA_DTRNG_BINS_"), 
  heatwave_counts = get_data(var_base_paths[["heatwave_counts"]], "LA_HEATWAVE_CNT_")
)

var_data_by_year <- list()
for (year in years) {
  yearly_data <- list()
  for (var in var_data) {
    yearly_data <- c(yearly_data, var[[as.character(year)]])
  }
  var_data_by_year[[as.character(year)]] <- yearly_data
}


# create dir for output data
output_path <- file.path("analysis", "processed", "collapsed_measures")
if (!dir.exists(output_path)) {
  dir.create((output_path))
}


# get and store weights for each zipcode
print("Computing area weights...")
sample_rast <- rast(var_data_by_year[[1]][[5]]) # get any heatwave_count file
weights <- list()
for (i in 1:nrow(zipcodes_id_geo)) {
  zipcode_id <- zipcodes_id_geo[i, ][["ZIPCODE"]]
  zipcode_geo <- vect(zipcodes_id_geo[i, ][["geometry"]])
  extracted_vals <- extract(sample_rast, zipcode_geo, weights=TRUE, ID=FALSE)
  sum_weights <- sum(extracted_vals$weight)
  extracted_vals$actual_weights <- extracted_vals$weight / sum_weights
  sum_actual_weights <- sum(extracted_vals$actual_weights)
  if (!between(sum_actual_weights, 0.9999, 1.0001)) {
    stop(paste(
      "Sum of temp weights is not 1. Actual sum is", sum_actual_weights, "for",
      zipcode_id
    ))
  }
  weights[[zipcode_id]] <- extracted_vals$actual_weights 
}


# iterate through each year's files, stack, extract, store 
test_dtemp <- rast(var_data_by_year[[1]][[1]])
test_dtrng <- rast(var_data_by_year[[1]][[4]])
test_heatwave <- rast(var_data_by_year[[1]][[5]])

collapsed_measures_df <- data.frame(matrix(ncol = nlyr(test_dtemp) * 3 + nlyr(test_dtrng) + nlyr(test_heatwave) + 2, nrow = 0))

# for (year in names(var_data_by_year)) {
for (year in 2003:2004) {
  print(paste("Getting data for", year))
  rasts <- lapply(var_data_by_year[[as.character(year)]], rast)
  yearly_rasters <- rast(rasts)
  print("Extracting vals for each zipcode...")
  for (i in 1:nrow(zipcodes_processed)) {
    zipcode_id <- zipcodes_id_geo[i, ][["ZIPCODE"]]
    zipcode_geo <- vect(zipcodes_id_geo[i, ][["geometry"]])
    
    extracted_vals <- extract(yearly_rasters, zipcode_geo, ID=FALSE)
    actual_weights <- weights[[zipcode_id]] # include check / error handling here

    # todo replace with init
    zipcode_df <- data.frame(matrix(ncol=2, nrow=0))
    zipcode_df <- rbind(zipcode_df, c(year, zipcode_id))
    colnames(zipcode_df) = c("YEAR", "ZIPCODE")
    
    # for (j in 1:ncol(extracted_vals)) {
    for (j in 1:length(weights)) {
      new_col <- names(extracted_vals)[[j]]
      weighted_data <- extracted_vals[[j]] * actual_weights
      final_val <- sum(weighted_data, na.rm = TRUE) 
      zipcode_df[[new_col]] <- final_val
    }
    
    if (nrow(collapsed_measures_df) == 0) {
      collapsed_measures_df <- zipcode_df
    }
    else {
      collapsed_measures_df <- bind_rows(collapsed_measures_df, zipcode_df)
    }
  }
  
  # save, just in case
  print("Saving progress...")
  csv_file_path <- file.path(output_path, paste0(year, ".csv"))
  write.csv(collapsed_measures_df, csv_file_path)
}

csv_file_path <- file.path(output_path, "final.csv")
write.csv(collapsed_measures_df, csv_file_path)