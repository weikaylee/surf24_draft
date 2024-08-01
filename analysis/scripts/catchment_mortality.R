# load libraries
library(terra)
library(sf)
library(tidyverse)
library(sp)
library(gridExtra)

# load mortality data
mortality_path <- file.path("analysis", "processed", "health_data", "mortality_rates.csv")
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


# mortality data already has data for each zip, but since each zip has na vals for certain cols, iterate through 
# each col and get catchment groups for each col

# get_rates <- function(df, col, years) { # TODO update params
#   # for each year in col, get curr zipcodes and missing zipcodes (zipcodes w na val) 
#   yearly_zip_data <- list()
#   df_col <- df[, c("Year", "Zipcode", col)]
#   for (year in years) {
#     # get mortality data for that year only, in that col
#     yearly_mortality <- subset(df_col, Year == year)
#     
#     # get zipcodes wo na val in col 
#     yearly_zips <- subset(yearly_mortality, !is.na(yearly_mortality[, col]))$Zipcode
#     yearly_missing_zips <- subset(yearly_mortality, is.na(yearly_mortality[, col]))$Zipcode
#     
#     # check that total num zips is constant 
#     if ((length(yearly_zips) + length(yearly_missing_zips)) != length(zips_only)) {
#       message("expected ", length(zips_only), " num zips but got ", length(yearly_zips) + length(yearly_missing_zips), " num zips")
#     }
#     
#     # store 
#     yearly_data <- list(
#       curr_zips = yearly_zips, 
#       missing_zips = yearly_missing_zips
#     )
#     
#     yearly_zip_data[[as.character(year)]] = yearly_data
#   }
#   
#   
#   # create catchment groups for each year, mapping focal zip to missing zips 
#   catchments = list() 
#   for (year in years) {
#     yearly_zips <- yearly_zip_data[[year]]
#     curr_zips <- yearly_zips$curr_zips
#     missing_zips <- yearly_zips$missing_zips
#     missing_zips_centroids <- subset(zip_centroids, ZIPCODE %in% missing_zips)
#     curr_zips_centroids <- subset(zip_centroids, ZIPCODE %in% curr_zips)
#     
#     # get closest nonmissing zipcode by centroid for each missing zipcode
#     nearest <- st_nearest_feature(missing_zips_centroids$centroid, curr_zips_centroids$centroid)
#     nearest_zips <- curr_zips_centroids[nearest, ]$ZIPCODE
#     
#     print(nearest_zips)
#     # add to catchment group
#     yearly_catchments <- list()
#     for (i in 1:length(nearest_zips)) {
#       focal_zip = nearest_zips[[i]]
#       catchment_zip = missing_zips[[i]]
#       if (!focal_zip %in% names(yearly_catchments)) {
#         yearly_catchments[[focal_zip]] <- c(catchment_zip)
#       }
#       else {
#         yearly_catchments[[focal_zip]] <- c(yearly_catchments[[focal_zip]], catchment_zip)
#       }
#     }
#     
#     # add to main catchment group 
#     catchments[[year]] <- yearly_catchments
#   }
# 
#   
#   # use pop data to compute weights 
#   tolerance <- 1e-5
#   weights <- list()
#   for (year in years) {
#     yearly_catchment <- catchments[[year]]
#     yearly_weights <- list()
#     for (zip in names(yearly_catchment)) {
#       # get pop data from pop_df
#       pop_data <- subset(pop_df, ZIPCODE %in% c(zip, yearly_catchment[[zip]]))
#       
#       # get tot pop in group 
#       tot_pop <- pop_data["TOTPOP"] %>% lapply(as.numeric) %>% unlist %>% sum
#       
#       # get weights 
#       catch_weights <- list()
#       zip_pop <- (pop_data %>% filter(ZIPCODE == zip) %>% pull) %>% as.numeric
#       catch_weights[[zip]] <- zip_pop / tot_pop
#       for (catch_zip in yearly_catchment[[zip]]) {
#         zip_pop <- pop_data %>% filter(ZIPCODE == catch_zip) %>% pull %>% as.numeric
#         weight <-  zip_pop / tot_pop
#         catch_weights[[catch_zip]] = weight
#       }
#       
#       # check that weights sum to one 
#       sum_weights <- catch_weights %>% unlist %>% sum
#       if (abs(sum_weights - 1) > tolerance) {
#         message("Sum of temp weights is not 1. Actual sum is ", sum_weights, "for group", zip)
#       }
#       
#       yearly_weights[[zip]] <- catch_weights
#     }
#     weights[[year]] <- yearly_weights
#   }
#   
#   
#   # compute new rates using weights 
#   df_full_col <- subset(df_col, !is.na(df_col[, col]))
#   data_to_add <- list()
#   idx <- 1
#   for (year in years) {
#     yearly_weights <- weights[[year]]
#     yearly_mortality <- subset(df_full_col, Year == year)
#     for (zip in names(yearly_weights)) {
#       # get mortality data for focal zip 
#       zip_mort <- subset(yearly_mortality, Zipcode == zip)
#       zip_mort_rates <- zip_mort[, col]
#       
#       # compute group's rate for each var / col 
#       catch_weights <- yearly_weights[[zip]]
#       zip_weight <- catch_weights[[zip]]
#       group_rate <- zip_mort_rates / zip_weight 
#       
#       # for each catch zip, multiply group rate by zip's weight 
#       for (catch_zip in names(catch_weights)) {
#         if (catch_zip != zip) {
#           zip_rate <- group_rate * catch_weights[[catch_zip]]
#           zip_rate <- cbind(ZIPCODE = catch_zip, zip_rate)
#           zip_rate <- cbind(YEAR = year, zip_rate)
#           
#           data_to_add[[idx]] <- zip_rate
#           idx = idx + 1
#         }
#       }
#     }
#   }
#   
#   # bind rows to df 
#   data_to_add_df <- do.call(rbind, data_to_add) 
#   row.names(data_to_add_df) <- NULL
#   df_full_col <- rbind(df_full_col, data_to_add_df)
#   
#   # sort by year and zipcode 
#   df_full_col_sorted <- df_full_col[order(df_full_col$YEAR, df_full_col$ZIPCODE), ]
#   row.names(df_full_col_sorted) <- NULL
#   
#   return(df_full_col_sorted)
# }


# iterate through all rate cols, merge together into one df
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
    
    yearly_zip_data[[as.character(year)]] = yearly_data
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
        yearly_catchments[[focal_zip]] <- c(catchment_zip)
      }
      else {
        yearly_catchments[[focal_zip]] <- c(yearly_catchments[[focal_zip]], catchment_zip)
      }
    }
    
    # add to main catchment group 
    catchments[[year]] <- yearly_catchments
  }
  
  
  # use pop data to compute weights 
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
      catch_weights[[as.character(zip)]] <- zip_pop / tot_pop
      for (catch_zip in yearly_catchment[[zip]]) {
        zip_pop <- pop_data %>% filter(ZIPCODE == catch_zip) %>% pull %>% as.numeric
        weight <-  zip_pop / tot_pop
        catch_weights[[as.character(catch_zip)]] = weight
      }
      
      # check that weights sum to one 
      sum_weights <- catch_weights %>% unlist %>% sum
      if (abs(sum_weights - 1) > tolerance) {
        message("Sum of temp weights is not 1. Actual sum is ", sum_weights, "for group", zip)
      }
      
      yearly_weights[[as.character(zip)]] <- catch_weights
    }
    weights[[year]] <- yearly_weights
  }
  
  
  # compute new rates using weights 
  df_full_col <- subset(df_col, !is.na(df_col[, col]))
  data_to_add <- list()
  idx <- 1
  for (year in years) {
    yearly_weights <- weights[[year]]
    yearly_mortality <- subset(df_full_col, Year == year)
    for (zip in names(yearly_weights)) {
      # get mortality data for focal zip 
      zip_mort <- subset(yearly_mortality, Zipcode == zip)
      # zip_mort_rates <- zip_mort[, col]
      
      # compute group's rate for each var / col 
      catch_weights <- yearly_weights[[zip]]
      zip_weight <- catch_weights[[zip]]
      group_rate <- zip_mort / zip_weight 
      
      # for each catch zip, multiply group rate by zip's weight 
      for (catch_zip in names(catch_weights)) {
        if (catch_zip != zip) {
          zip_rate <- group_rate * catch_weights[[catch_zip]]
          zip_rate["Zipcode"] <- catch_zip
          zip_rate["Year"] <- year
          
          data_to_add[[idx]] <- zip_rate
          idx = idx + 1
        }
      }
    }
  }
  
  # bind rows to df 
  data_to_add_df <- do.call(rbind, data_to_add) 
  row.names(data_to_add_df) <- NULL
  df_full_col <- rbind(df_full_col, data_to_add_df)
  
  # sort by year and zipcode 
  df_full_col_sorted <- df_full_col[order(df_full_col$Year, df_full_col$Zipcode), ]
  row.names(df_full_col_sorted) <- NULL
  catchment_mort_df <- merge(catchment_mort_df, df_full_col_sorted, by=c("Year", "Zipcode"), all=TRUE)
}


# save final df!
save_path <- file.path("analysis", "processed", "health_data", "mortality_rates_catchment.csv")
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

# # get data for 5 years
# df_to_plot <- list(
#   "2005" = df_list[["2005"]],
#   "2010" = df_list[["2010"]],
#   "2015" = df_list[["2015"]],
#   "2020" = df_list[["2020"]]
# )

# create list of plots
plots <- lapply(names(df_list), function(year) {
  spplot(df_list[[year]], zcol = "Total",
         main = paste(year, "Total Mortality Rate"))
})

do.call(grid.arrange, plots)
