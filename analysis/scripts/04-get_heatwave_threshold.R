# load libraries
library(raster)
library(sf)

# consts
years <- c(1981:2010)
percentile <- 0.85
input_base_path <- file.path("analysis", "data", "PRISM_historical")
shp_file <- file.path("analysis", "data", "la_census_shp", "LA_HRS_income.shp")

# read shp file
la_shp <- read_sf(shp_file)
la_geo <- as_Spatial(la_shp[["geometry"]])

# create list of all input data
yearly_input_data <- list()
for (year in years) {
  yearly_input_data[[as.character(year)]] <- file.path(input_base_path, year)
}

# get yearly avg temps 
yearly_output_data <- list() 
for (year in yearly_input_data) {
  monthly_temps <- list.files(path = year, pattern = "\\.bil$", full.names = TRUE)
  raster_stack <- stack(monthly_temps) 
  print(paste0("Getting avg temp for ", year, "..."))
  # apply shp file to both layers (july and august data)
  extracted_vals <- extract(raster_stack, la_shp, method="simple")
  
  # get avg and store 
  extracted_vals_flattened <- unlist(extracted_vals, recursive = TRUE)
  extracted_vals_sorted <- sort(extracted_vals_flattened, decreasing = FALSE)  
  yearly_output_data[[as.character(year)]] = mean(extracted_vals_sorted)
}

# get 85th percentile 
# https://www.indeed.com/career-advice/career-development/how-to-calculate-percentile lol
yearly_output_data_sorted <- sort(yearly_output_data, decreasing = FALSE) 
idx <- round(percentile * length(yearly_output_data_sorted))
percentile_val <- mean(c(yearly_output_data_sorted[idx], yearly_output_data_sorted[idx + 1]))
print(percentile_val)
