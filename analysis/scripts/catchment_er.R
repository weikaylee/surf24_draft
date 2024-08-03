# load libraries
library(terra)
library(sf)
library(tidyverse)
library(sp)
library(gridExtra)

# load er data
er_path <- file.path("analysis", "processed", "health_data", "er_health_data_counts.csv")
er_df <- read.csv(er_path)


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
plot(zipcodes)
plot(geos_only)

# for er data, filter out zipcodes wo pop data (removes 1 -- 90095, aka ucla) 
er_df <- subset(er_df, ZIPCODE %in% pop_df$ZIPCODE)


# for each year, get curr zipcodes and missing zipcodes 
years <- c(2005:2020)
years <- lapply(years, toString)

yearly_zip_data <- list()
for (year in years) {
  # get er data for that year only 
  yearly_er <- subset(er_df, YEAR == year)
  
  # get zipcodes 
  yearly_zips <- yearly_er$ZIPCODE
  yearly_missing_zips <- zips_only[!zips_only %in% yearly_zips]
  
  # check that total num zips is constant 
  if ((length(yearly_zips) + length(yearly_missing_zips)) != length(zips_only)) {
    message("expected ", length(zips_only), " num zips but got ", length(yearly_zips) + length(yearly_missing_zips), " num zips")
  }
  
  # store 
  yearly_data <- list(
    curr_zips = yearly_zips, 
    missing_zips = yearly_missing_zips
  )
  
  yearly_zip_data[[as.character(year)]] = yearly_data
}


# get centroids for all zipcodes
zip_centroids <- data.frame(zipcodes)
zip_centroids[, "centroid"] <- lapply(geos_only, st_centroid)
plot(zip_centroids$geometry)
plot(zip_centroids$centroid, add=TRUE)


# create catchment groups for each year, mapping focal zip to missing zips 
catchments = list() 
for (year in years) {
  yearly_zips <- yearly_zip_data[[year]]
  curr_zips <- yearly_zips$curr_zips
  missing_zips <- yearly_zips$missing_zips
  missing_zips_centroids <- subset(zip_centroids, ZIPCODE %in% missing_zips)
  curr_zips_centroids <- subset(zip_centroids, ZIPCODE %in% curr_zips)
  
  # get closest nonmissing zipcode by centroid
  nearest <- st_nearest_feature(missing_zips_centroids$centroid, curr_zips_centroids$centroid)

  # get corresponding zipcodes
  nearest_zips <- curr_zips_centroids[nearest, ]$ZIPCODE
  
  # add to catchment group
  count = 0
  yearly_catchments <- list()
  for (i in 1:length(nearest_zips)) {
    focal_zip = nearest_zips[[i]]
    catchment_zip = missing_zips[[i]]
    if (!focal_zip %in% names(yearly_catchments)) {
      yearly_catchments[[toString(focal_zip)]] <- c(toString(catchment_zip))
      count = count + 1
    }
    else {
      yearly_catchments[[toString(focal_zip)]] <- c(yearly_catchments[[toString(focal_zip)]], toString(catchment_zip))
      count = count + 1
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



# compute new rate for each catchment group 
new_rates <- list()
idx <- 1
for (year in years) {
  message("Getting rates for ", year)
  yearly_catchment <- catchments[[year]]
  yearly_df <- subset(er_df, YEAR == year)
  for (zip in names(yearly_catchment)) {
    # get pop data from pop_df
    pop_data <- subset(pop_df, ZIPCODE %in% c(zip, yearly_catchment[[zip]]))

    # get tot pop in group 
    tot_pop <- pop_data["TOTPOP"] %>% lapply(as.numeric) %>% unlist %>% sum

    # get rates for focal zip 
    zip_er <- subset(yearly_df, ZIPCODE == zip)
    zip_er_counts <- zip_er[, 3:length(colnames(zip_er))]
    
    # get new rate for catchment group 
    rate <- (zip_er_counts / tot_pop) * 1e5 
    
    # add new rates for each zipcode to list 
    rate <- cbind(ZIPCODE = zip, rate)
    rate <- cbind(YEAR = year, rate)
    new_rates[[idx]] <- rate
    idx = idx + 1

    for (catch_zip in yearly_catchment[[zip]]) {
      rate$ZIPCODE = catch_zip
      # zip_rate <- cbind(ZIPCODE = catch_zip, rate)
      # zip_rate <- cbind(YEAR = year, zip_rate)
      new_rates[[idx]] <- rate
      idx = idx + 1
    }
  }
}

# convert new rates to df 
new_rates_df <- do.call(rbind, new_rates) 
row.names(new_rates_df) <- NULL

# sort by year and zipcode 
new_rates_df_sorted <- new_rates_df[order(new_rates_df$YEAR, new_rates_df$ZIPCODE), ]
row.names(new_rates_df_sorted) <- NULL

# save final er data 
save_path <- file.path("analysis", "processed", "health_data", "er_rates_catchment_correct_final.csv")
write.csv(new_rates_df_sorted, save_path, row.names = FALSE)


# 
# # plot a few catchments for one yr, to double check 
# par(mfrow= c(3,1))
# catchments_2020 <- catchments[["2020"]][1:3]
# for (catch in catchments_2020) {
#   # get geos
#   geos_to_plot <- subset(zipcodes, ZIPCODE %in% catch)
#   plot(geos_to_plot)
# }


# plot some data across zipcodes, across years 
# create spatialdf
df <- read.csv(file.path("analysis", "processed", "health_data", "er_rates_catchment_correct_final.csv")) %>% data.frame
geos = pull(zipcodes, "geometry") %>% rep(time=16) # extract col as vector, so it repeats 
spat_polys <- as_Spatial(geos)
row.names(spat_polys) <- row.names(df)
spat_df <- SpatialPolygonsDataFrame(spat_polys, df)

# split df by year
df_list <- split(spat_df, spat_df$YEAR)

# create list of plots 
plots <- lapply(names(df_list), function(year) {
  spplot(df_list[[year]], zcol = "EDvisits",
         main = paste(year, "ED Visits"))
})

store <- do.call(grid.arrange, plots)
