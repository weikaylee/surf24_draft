# load libraries
library(terra)
library(sf)
library(tidyverse)
library(sp)
library(gridExtra)

# load er data
er_path <- file.path("analysis", "processed", "health_data", "er_health_data_rates.csv")
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
  yearly_zips <- yearly_zip_data[[as.character(year)]]
  curr_zips <- yearly_zips$curr_zips
  missing_zips <- yearly_zips$missing_zips
  missing_zips_centroids <- subset(zip_centroids, ZIPCODE %in% missing_zips)
  curr_zips_centroids <- subset(zip_centroids, ZIPCODE %in% curr_zips)
  
  # iterate through each missing zipcode, get closest nonmissing zipcode by centroid
  nearest <- st_nearest_feature(missing_zips_centroids$centroid, curr_zips_centroids$centroid)

  # get corresponding zipcodes
  nearest_zips <- curr_zips_centroids[nearest, ]$ZIPCODE

  # add to catchment group
  yearly_catchments <- list()
  for (i in 1:length(nearest_zips)) {
    focal_zip = nearest_zips[[i]]
    catchment_zip = missing_zips[[i]]
    if (!focal_zip %in% names(yearly_catchments)) {
      yearly_catchments[[focal_zip]] <- c(catchment_zip)
    }
    else {
      yearly_catchments[[focal_zip]] <- c(yearly_catchments[[focal_zip]], catchment_zip)
    }
  }
  
  # add to main catchment group 
  catchments[[as.character(year)]] <- yearly_catchments
}


# then, use pop data to compute weights 
years <- lapply(years, toString)
tolerance <- 1e-5
weights <- list()
for (year in years) {
  yearly_catchment <- catchments[[year]]
  yearly_weights <- list()
  for (zip in names(yearly_catchment)) {
    # get pop data from pop_df
    pop_data <- subset(pop_df, ZIPCODE %in% c(zip, yearly_catchment[[zip]]))

    # get tot pop in group 
    tot_pop <- pop_data["TOTPOP"] %>% lapply(as.numeric) %>% unlist %>% sum

    # get weights 
    catch_weights <- list()
    zip_pop <- (pop_data %>% filter(ZIPCODE == zip) %>% pull) %>% as.numeric
    catch_weights[[zip]] <- zip_pop / tot_pop
    for (catch_zip in yearly_catchment[[zip]]) {
      zip_pop <- pop_data %>% filter(ZIPCODE == catch_zip) %>% pull %>% as.numeric
      weight <-  zip_pop / tot_pop
      catch_weights[[catch_zip]] = weight
    }
    
    # check that weights sum to one 
    sum_weights <- catch_weights %>% unlist %>% sum
    if (abs(sum_weights - 1) > tolerance) {
      message("Sum of temp weights is not 1. Actual sum is ", sum_weights, "for group", zip)
    }

    yearly_weights[[zip]] <- catch_weights
  }
  weights[[year]] <- yearly_weights
}


# compute new rates using weights 
years <- c(2005:2005) 
years <- lapply(years, toString)
er_df_full <- data.frame(er_df)
data_to_add <- list()
idx <- 1
for (year in years) {
  yearly_weights <- weights[[year]]
  yearly_er <- subset(er_df, YEAR == year)
  for (zip in names(yearly_weights)) {
    # get er data for focal zip 
    zip_er <- subset(yearly_er, ZIPCODE == zip)
    zip_er_rates <- zip_er[, 3:length(colnames(zip_er))]
    
    # compute group's rate for each var / col 
    zip_weight <- yearly_weights[[zip]][[zip]]
    group_rate <- zip_er_rates / zip_weight 
    print(group_rate)
    
    catch_weights <- yearly_weights[[zip]]
    # for each catch zip, multiply group rate by zip's weight 
    for (catch_zip in names(catch_weights)) {
      if (catch_zip != zip) {
        zip_rate <- group_rate * catch_weights[[catch_zip]]
        zip_rate <- cbind(ZIPCODE = catch_zip, zip_rate)
        zip_rate <- cbind(YEAR = year, zip_rate)

        data_to_add[[idx]] <- zip_rate
        idx = idx + 1
      }
    }
  }
}

# bind rows to df 
data_to_add_df <- do.call(rbind, data_to_add) 
row.names(data_to_add_df) <- NULL
er_df_full <- rbind(er_df_full, data_to_add_df)

# sort by year and zipcode 
er_df_sorted <- er_df_full[order(er_df_full$YEAR, er_df_full$ZIPCODE), ]
row.names(er_df_sorted) <- NULL

# save final er data 
save_path <- file.path("analysis", "processed", "health_data", "er_rates_catchment.csv")
write.csv(er_df_sorted, save_path, row.names = FALSE)



# plot a few catchments for one yr, to double check 
par(mfrow= c(3,1))
catchments_2020 <- catchments[["2020"]][1:3]
for (catch in catchments_2020) {
  # get geos
  geos_to_plot <- subset(zipcodes, ZIPCODE %in% catch)
  plot(geos_to_plot)
}


# plot some data across zipcodes, across years 
yrs_to_plot <- seq(2005, 2020, by = 5)

# create spatialdf
to_plot = er_df_sorted
geos = pull(zipcodes, "geometry") %>% rep(time=16) # extract col as vector, so it repeats 
spat_polys <- as_Spatial(geos)
row.names(spat_polys) <- row.names(to_plot)
spat_df <- SpatialPolygonsDataFrame(spat_polys, to_plot)

# split df by year
df_list <- split(spat_df, spat_df$YEAR)

# # get data for 5 years 
# df_to_plot <- list(
#   "2005" = df_list[["2005"]], 
#   "2010" = df_list[["2010"]], 
#   "2015" = df_list[["2015"]], 
#   "2020" = df_list[["2020"]]
# )

# create list of plots 
plots <- lapply(names(df_list), function(year) {
  spplot(df_list[[year]], zcol = "EDvisits",
         main = paste(year, "ED Visits"))
})

do.call(grid.arrange, plots)
