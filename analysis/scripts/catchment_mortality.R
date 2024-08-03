# load libraries
library(terra)
library(sf)
library(tidyverse)
library(sp)
library(gridExtra)

# load mortality data
mortality_path <- file.path("analysis", "processed", "health_data", "mortality_counts.csv")
mortality_df <- read.csv(mortality_path)


# load zipcode data 
zipcodes_path <- file.path("analysis", "processed", "collapsed_measures", "zipcodes_processed.shp")
zipcodes <- read_sf(zipcodes_path)


# load zipcode population data 
pop_path <- file.path("analysis", "data", "pca", "ACSDP5Y2022.DP05-Data.csv")
pop_df <- read.csv(pop_path)

# format pop data 
cols_to_keep = c("NAME", "DP05_0001E") # zipcode name, zipcode pop 
pop_df = pop_df[, cols_to_keep]

# remove first row (descriptions of col names) 
pop_df = pop_df[-1, ]
row.names(pop_df) <- NULL # reset index

# format zipcodes (remove ZCTA5_)
pop_df["NAME"] = lapply(pop_df["NAME"], function(x) {
  return(str_remove(x, "ZCTA5 "))
})

# rename columns
pop_df <- pop_df %>% rename(
  ZIPCODE = NAME,
  TOTPOP = DP05_0001E
)


# filter out zipcodes with pop = 0; is bc those zipcodes r for universities, stadiums, etc (removes 7 zips from og data)
pop_df <- subset(pop_df, TOTPOP > 0)


# for zipcode data, filter out zipcodes wo population data (removes 11 zips from og data)
zipcodes <- subset(zipcodes, ZIPCODE %in% pop_df$ZIPCODE)
zips_only <- zipcodes$ZIPCODE
geos_only <- zipcodes[, "geometry"] %>% st_transform(6423) # la crs: https://epsg.io/6423

# get centroids for all zipcodes
zip_centroids <- data.frame(zipcodes)
zip_centroids[, "centroid"] <- lapply(geos_only, st_centroid)

# for mortality data, filter out zipcodes wo pop data (removes 1 -- 90095, aka ucla) 
mortality_df_filtered <- subset(mortality_df, Zipcode %in% pop_df$ZIPCODE)


# iterate through all rate cols
years <- c(2005:2020)
years <- lapply(years, toString)

catchment_mort_df <- data.frame(matrix(ncol=2, nrow=0))
colnames(catchment_mort_df) <- c("Year", "Zipcode")
rate_cols <- colnames(mortality_df_filtered)[3:length(colnames(mortality_df_filtered))]

for (col in rate_cols) {
  message("Getting rates for ", col)
  # df <- get_rates(mortality_df_filtered, colname, years)
  yearly_zip_data <- list()
  df_col <- mortality_df_filtered[, c("Year", "Zipcode", col)]
  for (year in years) {
    # get mortality data for that year only, in that col
    yearly_mortality <- subset(df_col, Year == year)  
    
    # get zipcodes
    yearly_zips <- subset(yearly_mortality, !is.na(yearly_mortality[, col]))$Zipcode
    yearly_missing_zips <- subset(yearly_mortality, !Zipcode %in% yearly_zips)$Zipcode
    
    # check that total num zips is constant 
    if ((length(yearly_zips) + length(yearly_missing_zips)) != length(zips_only)) {
      message("expected ", length(zips_only), " num zips but got ", length(yearly_zips) + length(yearly_missing_zips), " num zips")
    }
    
    # store 
    yearly_data <- list(
      curr_zips = yearly_zips, 
      missing_zips = yearly_missing_zips
    )
    
    yearly_zip_data[[year]] = yearly_data
  }
  
  
  # create catchment groups for each year, mapping focal zip to missing zips 
  catchments = list() 
  for (year in years) {
    yearly_zips <- yearly_zip_data[[year]]
    curr_zips <- yearly_zips$curr_zips
    missing_zips <- yearly_zips$missing_zips
    missing_zips_centroids <- subset(zip_centroids, ZIPCODE %in% missing_zips)
    curr_zips_centroids <- subset(zip_centroids, ZIPCODE %in% curr_zips)
    
    # get closest nonmissing zipcode by centroid for each missing zipcode
    nearest <- st_nearest_feature(missing_zips_centroids$centroid, curr_zips_centroids$centroid)
    nearest_zips <- curr_zips_centroids[nearest, ]$ZIPCODE

    # add to catchment group
    yearly_catchments <- list()
    for (i in 1:length(nearest_zips)) {
      focal_zip = nearest_zips[[i]]
      catchment_zip = missing_zips[[i]]
      if (!focal_zip %in% names(yearly_catchments)) {
        yearly_catchments[[toString(focal_zip)]] <- c(catchment_zip)
      }
      else {
        yearly_catchments[[toString(focal_zip)]] <- c(yearly_catchments[[toString(focal_zip)]], catchment_zip)
      }
    }
    
    # get focal zipcodes wo group 
    if (TRUE %in% (!curr_zips %in% nearest_zips)) {
      zips_wo_group <- curr_zips[!curr_zips %in% nearest_zips]
      for (i in 1:length(zips_wo_group)) {
        yearly_catchments[[toString(zips_wo_group[[i]])]] <- list()
      }
    }
    
    # add to main catchment group 
    catchments[[year]] <- yearly_catchments
  }
  
  
  # use pop data to compute weights 
  df_full_col <- subset(df_col, !is.na(df_col[, col]))
  data_to_add <- list()
  idx <- 1
  for (year in years) {
    yearly_catchment <- catchments[[year]]
    yearly_mortality <- subset(df_full_col, Year == year)
    for (zip in names(yearly_catchment)) {
      # get pop data from pop_df
      pop_data <- subset(pop_df, ZIPCODE %in% c(zip, yearly_catchment[[zip]]))
      
      # get tot pop in group 
      tot_pop <- pop_data["TOTPOP"] %>% lapply(as.numeric) %>% unlist %>% sum
      
      # get mortaltiy data for focal zip 
      zip_mort <- subset(yearly_mortality, Zipcode == zip)
      
      # compute group's new rate 
      new_rate <- (zip_mort / tot_pop) * 1e5
      
      # add to list 
      new_rate["Zipcode"] <- zip
      new_rate["Year"] <- year
      data_to_add[[idx]] <- new_rate
      idx = idx + 1
      
      for (catch_zip in yearly_catchment[[zip]]) {
        new_rate["Zipcode"] <- catch_zip
        data_to_add[[idx]] <- new_rate
        idx = idx + 1
      }
    }
  }

  # bind rows to df 
  data_to_add_df <- do.call(rbind, data_to_add) 
  row.names(data_to_add_df) <- NULL
  
  # sort by year and zipcode 
  df_sorted <- data_to_add_df[order(data_to_add_df$Year, data_to_add_df$Zipcode), ]
  row.names(df_sorted) <- NULL
  catchment_mort_df <- merge(catchment_mort_df, df_sorted, by=c("Year", "Zipcode"), all=TRUE)
}


# save final df!
save_path <- file.path("analysis", "processed", "health_data", "mortality_rates_catchment_correct_final.csv")
write.csv(catchment_mort_df, save_path, row.names = FALSE)


# plot some data across zipcodes, across years
# create spatialdf
to_plot = mortality_df_filtered
geos = pull(zipcodes, "geometry") %>% rep(time=16) # extract col as vector, so it repeats
spat_polys <- as_Spatial(geos)
row.names(spat_polys) <- row.names(to_plot)
spat_df <- SpatialPolygonsDataFrame(spat_polys, to_plot)

# split df by year
df_list <- split(spat_df, spat_df$Year)

# create list of plots
plots <- lapply(names(df_list), function(year) {
  spplot(df_list[[year]], zcol = "Total",
         main = paste(year, "Total Mortality Rate"))
})

do.call(grid.arrange, plots)
# if nothing plots, try storing output of do.call and then call in console...
