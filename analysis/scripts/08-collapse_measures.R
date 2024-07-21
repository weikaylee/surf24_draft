message("Cleaning zipcode shps...")
source(file.path("analysis", "scripts", "06-clean_zipcode_data.R"))
message("Getting data to collapse...")
source(file.path("analysis", "scripts", "07-get_all_processed_data.R"))


# map file name prefix to layer name prefix 
layer_names <- list(
  DAILY_TEMP_BINS_AVG = "DAILY_AVG_TEMP",
  DAILY_TEMP_BINS_MAX = "DAILY_MAX_TEMP",
  DAILY_TEMP_BINS_MIN = "DAILY_MIN_TEMP",
  DTRNG_BINS = "DIURNAL_TEMP_RNG",
  HEATWAVE_CNT = "HEATWAVE_CNT"
)

# get col names based on layers for each raster var
generate_colnames <- function(files, bin_labels, layer_names, not_bins) {
  col_names <- lapply(files, function(file) {
    for (file_name in names(layer_names)) {
      if (grepl(file_name, file, fixed=TRUE)) {
        layer_name <- paste(layer_names[[file_name]], bin_labels, sep="_")
        if (file_name %in% not_bins) { 
          layer_name <- layer_names[[file_name]]
        }
        return(layer_name)
      }
    }
  })
  return(unlist(col_names))
}

ref_files <- var_data_by_year$"2003"
layer_colnames <- generate_colnames(unlist(ref_files, use.names=FALSE), bin_labels, layer_names, list("HEATWAVE_CNT"))


# get and store weights for each zipcode
message("Computing zipcode weights...")
ref_rast <- rast(ref_files$heatwave_counts)
tolerance <- 1e-5 # to avoid numeric comparison errors in if statement

get_zipcode_weights <- function(ref_rast, zipcode_geo) {
  extracted_vals <- extract(ref_rast, zipcode_geo, exact=TRUE, ID=FALSE) # if using weights = TRUE, might not capture all cells (leads to errors later on)
  sum_weights <- sum(extracted_vals$fraction)
  zipcode_weights <- extracted_vals$fraction / sum_weights
  return(zipcode_weights)
}

get_all_weights <- function(ref_rast, zipcodes_processed, tolerance) {
  weights <- list() 
  for (i in 1:nrow(zipcodes_processed)) {
    zipcode_id <- zipcodes_processed$ZIPCODE[i]
    zipcode_geo <- zipcodes_processed$geometry[i] %>% vect
    zipcode_weights <- get_zipcode_weights(ref_rast, zipcode_geo)
    sum_weights <- sum(zipcode_weights)
    if (abs(sum_weights - 1) > tolerance) {
      message("Sum of temp weights is not 1. Actual sum is ", sum_weights, "for ", zipcode_id)
    }
    weights[[zipcode_id]] <- zipcode_weights
  }
  return(weights)
}

weights <- get_all_weights(ref_rast, zipcodes_processed, tolerance)


# iterate through each year's files, stack, extract, store 
ref_dtemp <- rast(ref_files$daily_temp)
ref_dtrng <- rast(ref_files$diurnal_range)
ref_heatwave <- rast(ref_files$heatwave_counts)

zipcode_colnames <- c("YEAR", "ZIPCODE")
collapsed_measures_colnames <- c(zipcode_colnames, layer_colnames)
collapsed_measures_df <- data.frame(matrix(ncol = length(collapsed_measures_colnames), nrow = 0)) %>% setNames(collapsed_measures_colnames)

for (year in names(var_data_by_year)) {
# for (year in 2003:2004) {
  message("Collapsing measures for ", year, "...")
  yearly_rasters <- lapply(var_data_by_year[[as.character(year)]], rast) %>% rast
  for (i in 1:nrow(zipcodes_processed)) {
    zipcode_id <- zipcodes_processed$ZIPCODE[i]
    zipcode_geo <- zipcodes_processed$geometry[i] %>% vect
    
    extracted_vals <- extract(yearly_rasters, zipcode_geo, ID=FALSE, touches=TRUE)
    actual_weights <- weights[[zipcode_id]] 
    
    zipcode_df <- data.frame(matrix(ncol = length(zipcode_colnames), nrow = 0)) %>% rbind(c(year, zipcode_id)) %>% setNames(zipcode_colnames)

    for (j in 1:ncol(extracted_vals)) {
      new_col <- col_names[[j]]
      weighted_mean <- weighted.mean(extracted_vals[[j]], actual_weights, na.rm=TRUE)
      zipcode_df[[new_col]] <- weighted_mean
    }
    
    collapsed_measures_df <- rbind(collapsed_measures_df, zipcode_df)
  }
  
  # save, just in case
  csv_file_path <- file.path(output_path, paste0(year, "test_3", ".csv"))
  message("Saving progress to ", csv_file_path, "...")
  write.csv(collapsed_measures_df, csv_file_path, row.names=FALSE)
}

csv_file_path <- file.path(output_path, "all_years.csv")
message("Saving final collapsed measures to ", csv_file_path, "...")
write.csv(collapsed_measures_df, csv_file_path, row.names=FALSE)

