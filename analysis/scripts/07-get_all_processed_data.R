# load libraries
library(terra)
library(tidyverse) 

# consts
years <- c(2003:2020)
bin_labels <- c("Inf_0", paste0(seq(0, 39), "_", seq(1, 40)), "40_Inf")
processed_base_path <- file.path("analysis", "processed")
output_path <- file.path(processed_base_path, "collapsed_measures")
dir.create(output_path, showWarnings=FALSE)

var_base_paths <- list(
  daily_temp = file.path(processed_base_path, "daily_temp_bins"),
  diurnal_range = file.path(processed_base_path, "diurnal_range_bins"),
  heatwave_counts = file.path(processed_base_path, "heatwave_counts")
)


# get list of all files in base_path (list of lists if is_nested is true)
get_data <- function(base_path, is_nested) {
  if (is_nested) {
    data_list <- lapply(years, function(year) {
      file.path(base_path, year) %>% list.files(full.names=TRUE)
    })
    return(data_list)
  }
  return(list.files(base_path, full.names=TRUE))
}

# load data 
var_data <- list(
  daily_temp = get_data(var_base_paths$daily_temp, is_nested=TRUE),
  diurnal_range = get_data(var_base_paths$diurnal_range, is_nested=FALSE),
  heatwave_counts = get_data(var_base_paths$heatwave_counts, is_nested=FALSE)
)

# organize data by year
var_data_by_year <- list()
for (i in 1:length(years)) {
  year <- years[i]
  yearly_data <- lapply(var_data, function(var) {
    var[[i]]
  })
  var_data_by_year[[as.character(year)]] <- yearly_data
}