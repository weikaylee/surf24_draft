# load libraries
library(lfe)
library(stringr)

# get panel data (just annual measures + mortality for now) 
panel_path <- file.path("analysis", "processed", "merged", "measures_mortality.csv")
panel <- read.csv(panel_path)


# funcs to create dfs with degree bins 
create_binned_df <- function(num_degrees, df_full) {
  # create copy of df bins, to modify
  df_bins <- df_full[, grepl("DAILY|DIURNAL", names(df_full))]
  df_not_bins <- df_full[, !colnames(df_full) %in% colnames(df_bins)]
  df_final <- data.frame(df_not_bins) 
  
  # get relevant idxs...do some math... 
  num_idxs <- 2 * ((length(names(df_full[, grepl("AVG", names(df_full))]))) %/% num_degrees) - 2 
  root_idxs <- c(1, 1 + num_degrees)
  for (i in 2:num_idxs) {
    if (i %% 2 == 0) {
      root_idxs <- c(root_idxs, root_idxs[i] + 1)
    }
    else {
      root_idxs <- c(root_idxs, root_idxs[i] + (num_degrees - 1))
    }
  }
  root_idxs <- c(root_idxs, 42)
  idxs <- c(root_idxs, root_idxs + 42, root_idxs + 2 * 42, root_idxs + 3 * 42)
  
  # sum relevant rows, and join to df_final
  for (i in seq(1, length(idxs) - 1, by=2)) {
    idx <- idxs[i]
    idx_1 <- idxs[i + 1]
    bins <- paste(sub(".*_(.*)_.*", "\\1", names(df_bins)[idx]), sub(".*_(.*).*", "\\1", names(df_bins)[idx_1]), sep="_")
    colname <- paste(sub("^([^_]*_[^_]*_[^_]*)_.*","\\1", names(df_bins)[idx]), bins, sep="_")
    df_final[, colname] <- c(df_bins[, idx:idx_1] %>% rowSums)
  }
  return(df_final)
}

three_deg <- create_binned_df(3, panel)
five_deg <- create_binned_df(5, panel)
seven_deg <- create_binned_df(7, panel)
nine_deg <- create_binned_df(9, panel)
twelve_deg <- create_binned_df(12, panel)

# models!









three_deg_temp_formula <- as.formula(paste("Total ~", paste(names(three_deg)[grepl("DAILY|DIURNAL", names(three_deg))], collapse="+"), "| ZIPCODE + YEAR"))
five_deg_temp_formula <- as.formula(paste("Total ~", paste(names(five_deg)[grepl("DAILY|DIURNAL", names(five_deg))], collapse="+"), "| ZIPCODE + YEAR"))
seven_deg_temp_formula <- as.formula(paste("Total ~", paste(names(seven_deg)[grepl("DAILY|DIURNAL", names(seven_deg))], collapse="+"), "| ZIPCODE + YEAR"))
nine_deg_temp_formula <- as.formula(paste("Total ~", paste(names(nine_deg)[grepl("DAILY|DIURNAL", names(nine_deg))], collapse="+"), "| ZIPCODE + YEAR"))
twelve_deg_temp_formula <- as.formula(paste("Total ~", paste(names(twelve_deg)[grepl("DAILY|DIURNAL", names(twelve_deg))], collapse="+"), "| ZIPCODE + YEAR"))

three_deg_temp <- felm(three_deg_temp_formula, data = three_deg)
summary(three_deg_temp)
five_deg_temp <- felm(five_deg_temp_formula, data = five_deg)
summary(five_deg_temp)
seven_deg_temp <- felm(seven_deg_temp_formula, data = seven_deg)
summary(seven_deg_temp)
nine_deg_temp <- felm(nine_deg_temp_formula, data = nine_deg)
summary(nine_deg_temp)
twelve_deg_temp <- felm(twelve_deg_temp_formula, data = twelve_deg)
summary(twelve_deg_temp)

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
