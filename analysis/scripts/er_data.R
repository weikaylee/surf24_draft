# load libraries
library(rio)
library(tidyverse)

# convert raw xlsx to csv 
raw_data_path <- file.path("analysis", "data")
raw_data <- file.path(raw_data_path, "emergency_department_characteristics")
# xls <- dir(raw_data, pattern = "xlsx", full.names=TRUE) 
# mapply(convert, xls, gsub("xlsx", "csv", xls))

# load all csvs 
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# jsut load one csv for now 
csv_path <- file.path(raw_data, "2005-hospital-emergency-department-characteristics-by-facility-pivot-profile.csv")
csv <- read.csv(csv_path)

# extract principal diag groups
zipcode_col <- "FAC_ZIPCODE"
groups_2005_to_2015 <- c(
  "Dx_All_Pregnancies",
  "Dx_Births",
  "Dx_Blood_Bloodforming_organs",
  "Dx_Circulatory",
  "Dx_Congenital_anomalies",
  "Dx_Digestive",
  "Dx_Endocrine_Metabolism",
  "Dx_Genitourinary",
  "Dx_Infections",
  "Dx_Injuries_Drugs_Complications",
  "Dx_Musculoskeletal",
  "Dx_Neoplasms",
  "Dx_Nervous_Sensory_Systems",
  "Dx_Other_Reasons",
  "Dx_Perinatal_disorders",
  "Dx_Psychoses_Neuroses",
  "Dx_Respiratory",
  "Dx_Skin_disorders",
  "Dx_Symptoms"
)

extracted_county <- subset(csv, COUNTY == "Los Angeles")
extracted_info <- extracted_county[c(zipcode_col, groups_2005_to_2015)]
test <- extracted_info$FAC_ZIPCODE
for (i in 1:length(test)) {
  if (grepl("-", test[[i]], fixed=TRUE)) {
    print("hi")
    test[[i]] <- sub("(^[^-]+)-.*", "\\1", test[[i]]) # get everything before hyphen
  }
}

TODO group by zipcodes, and merge counts
# PSEUDOCODE
# 1. process data, extract measures
# first, iterate through each year's files and get prinicpal diagnosis groups 
# (can just do this in terminal) 
# rename and merge columns as necessary 
# 
# first, extract rows for la county facilities 
# merge counts of rows with the same zipcode 
# ensure that all zipcodes are accounted for, by checking aginst zipcodes_processed 
# (if a zipcode is missing, add it in with a NA value)
# 
# get all-cause counts (check that ed_vists corresponds to this) 
# get all-cause age-specific counts, by merging columns 
# 
# 3. standardize
# get population from acs for corresponding year, for corresponding zipcode
# standardize all counts per 100k population using this formula: 
# count / (tot_pop / 100k) = count * 100k / tot_pop
# (think: if zipcode has 200k ppl, then standardized rate is count / 2, for a rate of x per 100k ppl)
# 
# 4. merge
# add final df to a larger df (maybe add in a col for year)
# 
# after iterating through all years and adding to the same larger df, join 
# the larger df to the collapsed_measures_df, by aligning years and zipcodes 
# done!
