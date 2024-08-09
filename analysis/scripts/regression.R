# load libraries
library(lfe)
library(stringr)

# get panel data (just annual measures + mortality for now) 
panel_path <- file.path("analysis", "processed", "merged", "measures_mortality.csv")
panel <- read.csv(panel_path)

# merge bins into five degree bins 
panel_bins <- panel[, grepl("DAILY|DIURNAL", names(panel))]
panel_final <- panel[, !colnames(panel) %in% colnames(panel_bins)] # get remaining cols

# get indices 
num_idxs <- 2 * ((length(names(panel[, grepl("AVG", names(panel))])) - 2) / 5) - 2 # aka 14; get num idxs needed for sixteen total 5-degree bins per binned group (excluding first and last groups, which have inf; these are defined manually below)
root_idxs <- c(1, 6)
for (i in 2:num_idxs) {
  if (i %% 2 == 0) {
    root_idxs <- c(root_idxs, root_idxs[i] + 1)
  }
  else {
    root_idxs <- c(root_idxs, root_idxs[i] + 4)
  }
}
root_idxs <- c(root_idxs, 42)

idxs <- c(root_idxs, root_idxs + 42, root_idxs + 2 * 42, root_idxs + 3 * 42)
for (i in seq(1, length(idxs) - 1, by=2)) {
  idx <- idxs[i]
  idx_1 <- idxs[i + 1]
  bins <- paste(sub(".*_(.*)_.*", "\\1", names(panel_bins)[idx]), sub(".*_(.*).*", "\\1", names(panel_bins)[idx_1]), sep="_")
  colname <- paste(sub("^([^_]*_[^_]*_[^_]*)_.*","\\1", names(panel_bins)[idx]), bins, sep="_")
  panel_final[, colname] <- c(panel_bins[, idx:idx_1] %>% rowSums)
}

# run models, with multiple fixed effects!
temp_colnames <- names(panel_final)[grepl("HEATWAVE", names(panel_final))]
vars <- paste(temp_colnames, collapse = " + ")
print(vars)
all_temp <- felm(Total ~ DAILY_AVG_TEMP_Inf_5 + DAILY_AVG_TEMP_5_10 +
                    DAILY_AVG_TEMP_10_15 + DAILY_AVG_TEMP_15_20 +
                    DAILY_AVG_TEMP_20_25 + DAILY_AVG_TEMP_25_30 +
                    DAILY_AVG_TEMP_30_35 + DAILY_AVG_TEMP_35_Inf +
                    DAILY_MAX_TEMP_Inf_5 + DAILY_MAX_TEMP_5_10 +
                    DAILY_MAX_TEMP_10_15 + DAILY_MAX_TEMP_15_20 +
                    DAILY_MAX_TEMP_20_25 + DAILY_MAX_TEMP_25_30 +
                    DAILY_MAX_TEMP_30_35 + DAILY_MAX_TEMP_35_Inf +
                    DAILY_MIN_TEMP_Inf_5 + DAILY_MIN_TEMP_5_10 +
                    DAILY_MIN_TEMP_10_15 + DAILY_MIN_TEMP_15_20 +
                    DAILY_MIN_TEMP_20_25 + DAILY_MIN_TEMP_25_30 +
                    DAILY_MIN_TEMP_30_35 + DAILY_MIN_TEMP_35_Inf |
                    ZIPCODE + YEAR, data = panel_final)
summary(all_temp)

avg_temp <- felm(Total ~ DAILY_AVG_TEMP_Inf_5 + DAILY_AVG_TEMP_5_10 + DAILY_AVG_TEMP_10_15 + DAILY_AVG_TEMP_15_20 + DAILY_AVG_TEMP_20_25 + DAILY_AVG_TEMP_25_30 + DAILY_AVG_TEMP_30_35 + DAILY_AVG_TEMP_35_Inf | ZIPCODE + YEAR, data = panel_final)
summary(avg_temp) 

min_temp <- felm(Total ~ DAILY_MIN_TEMP_Inf_5 + DAILY_MIN_TEMP_5_10 + DAILY_MIN_TEMP_10_15 + DAILY_MIN_TEMP_15_20 + DAILY_MIN_TEMP_20_25 + DAILY_MIN_TEMP_25_30 + DAILY_MIN_TEMP_30_35 + DAILY_MIN_TEMP_35_Inf | ZIPCODE + YEAR, data = panel_final)
summary(min_temp) 

max_temp <- felm(Total ~ DAILY_MAX_TEMP_Inf_5 + DAILY_MAX_TEMP_5_10 + DAILY_MAX_TEMP_10_15 + DAILY_MAX_TEMP_15_20 + DAILY_MAX_TEMP_20_25 + DAILY_MAX_TEMP_25_30 + DAILY_MAX_TEMP_30_35 + DAILY_MAX_TEMP_35_Inf | ZIPCODE + YEAR, data = panel_final)
summary(max_temp)

diurnal <- felm(Total ~ DIURNAL_TEMP_RNG_Inf_5 + DIURNAL_TEMP_RNG_5_10 + DIURNAL_TEMP_RNG_10_15 + DIURNAL_TEMP_RNG_15_20 + DIURNAL_TEMP_RNG_20_25 + DIURNAL_TEMP_RNG_25_30 + DIURNAL_TEMP_RNG_30_35 + DIURNAL_TEMP_RNG_35_Inf | ZIPCODE + YEAR, data = panel_final)
summary(diurnal) 

heatwave <- felm(Total ~ HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR, data = panel_final)
summary(heatwave)

temp_heatwave <- felm(Total ~ DAILY_AVG_TEMP_Inf_5 + DAILY_AVG_TEMP_5_10 +
                    DAILY_AVG_TEMP_10_15 + DAILY_AVG_TEMP_15_20 +
                    DAILY_AVG_TEMP_20_25 + DAILY_AVG_TEMP_25_30 +
                    DAILY_AVG_TEMP_30_35 + DAILY_AVG_TEMP_35_Inf +
                    DAILY_MAX_TEMP_Inf_5 + DAILY_MAX_TEMP_5_10 +
                    DAILY_MAX_TEMP_10_15 + DAILY_MAX_TEMP_15_20 +
                    DAILY_MAX_TEMP_20_25 + DAILY_MAX_TEMP_25_30 +
                    DAILY_MAX_TEMP_30_35 + DAILY_MAX_TEMP_35_Inf +
                    DAILY_MIN_TEMP_Inf_5 + DAILY_MIN_TEMP_5_10 +
                    DAILY_MIN_TEMP_10_15 + DAILY_MIN_TEMP_15_20 +
                    DAILY_MIN_TEMP_20_25 + DAILY_MIN_TEMP_25_30 +
                    DAILY_MIN_TEMP_30_35 + DAILY_MIN_TEMP_35_Inf + HEATWAVE_CNT 
                    + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3)|
                    ZIPCODE + YEAR, data = panel_final)
summary(temp_heatwave)

# TODO, try using different bins (7 degree? 3 degree?)
# TODO try plotting
# TODO try messing around with additional columns -- age? cause-sepcific mrotality? can see relationship used in paper

# dif models: temp bins, one for avg and max and min
# one for all temp bins
# one for diurnal range
# one for heatwave counts
# one for temp + diurnal, then tmep + heatwave, then heatwave + diurnal 
# one for all three 
