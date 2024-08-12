# load libraries
library(lfe)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggrepel)

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


# get bin names to use for plotting 
get_bin_names <- function(curr_names) {
  bin_names <- c()
  for (name in curr_names) {
    first_num <- sub(".*_(.*)_.*", "\\1", name)
    second_num <- sub(".*_(.*).*", "\\1", name)
    if (first_num == "Inf") {
      bin_names <- c(bin_names, paste0("<", second_num))
    }
    else if (second_num == "Inf") {
      bin_names <- c(bin_names, paste0("\u2265", first_num))
    }
    else {
      bin_names <- c(bin_names, paste0("[", first_num, ",", second_num, ")"))
    }
  }
  return(bin_names)
}


# func to generate grid of plots for all bins 
get_bin_graphs <- function(avg_eqn, min_eqn, max_eqn, drn_eqn, bin_names, deg) { 
  # create df from eqn to use for plotting
  avg_df <- data.frame(bin=bin_names, coeff=avg_eqn$beta[, 1], se=avg_eqn$se, 
                       pval=format(round(avg_eqn$pval, digits=2), nsmall=2))
  min_df <- data.frame(bin=bin_names, coeff=min_eqn$beta[, 1], se=min_eqn$se, 
                       pval=format(round(min_eqn$pval, digits=2), nsmall=2))
  max_df <- data.frame(bin=bin_names, coeff=max_eqn$beta[, 1], se=max_eqn$se, 
                       pval=format(round(max_eqn$pval, digits=2), nsmall=2))
  drn_df <- data.frame(bin=bin_names, coeff=drn_eqn$beta[, 1], se=drn_eqn$se, 
                       pval=format(round(drn_eqn$pval, digits=2), nsmall=2))
                       
  avg_res_df <- data.frame(avg_eqn$residuals, avg_eqn$fitted.values)
  colnames(avg_res_df) <- c("residual", "fitted")
  min_res_df <- data.frame(min_eqn$residuals, min_eqn$fitted.values)
  colnames(min_res_df) <- c("residual", "fitted")
  max_res_df <- data.frame(max_eqn$residuals, max_eqn$fitted.values)
  colnames(max_res_df) <- c("residual", "fitted")
  drn_res_df <- data.frame(drn_eqn$residuals, drn_eqn$fitted.values)
  colnames(drn_res_df) <- c("residual", "fitted")
  
  avg_df$bin <- factor(avg_df$bin, levels=avg_df$bin)
  min_df$bin <- factor(min_df$bin, levels=min_df$bin)
  max_df$bin <- factor(max_df$bin, levels=max_df$bin)
  drn_df$bin <- factor(drn_df$bin, levels=drn_df$bin)
  
  # create plots for eqach eqn 
  avg <- ggplot(na.omit(avg_df), aes(x=bin, y=coeff, group=1)) + 
    geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
    geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
    geom_line() + 
    geom_point(shape=21, fill="white") +
    labs(title=paste0("Daily avg ", deg, "-deg temp bins"),
         x ="Daily avg temp bins", y = "Change in annual mortality rate per additional day") +
    theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
    geom_text_repel(aes(label=na.omit(avg_df)$pval), size=3)
  
  avg_res <- ggplot(avg_res_df, aes(x=fitted, y=residual, group=1)) +
    labs(title=paste0("Daily avg ", deg, "-deg residuals"), 
         x="Fitted", y="Residual") +
    theme(plot.title=element_text(hjust=0.5)) +
    geom_point()
    
  
  min <- ggplot(na.omit(min_df), aes(x=bin, y=coeff, group=1)) + 
    geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
    geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
    geom_line() + 
    geom_point(shape=21, fill="white") +
    labs(title=paste0("Daily min ", deg, "-deg temp bins"),
         x ="Daily min temp bins", y = "Change in annual mortality rate per additional day") +
    theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
    geom_text_repel(aes(label=na.omit(min_df)$pval), size=3)
  
  min_res <- ggplot(min_res_df, aes(x=fitted, y=residual, group=1)) +
    labs(title=paste0("Daily min ", deg, "-deg residuals"), 
         x="Fitted", y="Residual") +
    theme(plot.title=element_text(hjust=0.5)) +
    geom_point()
    
    
  max <- ggplot(na.omit(max_df), aes(x=bin, y=coeff, group=1)) + 
    geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
    geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
    geom_line() + 
    geom_point(shape=21, fill="white") +
    labs(title=paste0("Daily max ", deg, "-deg temp bins"),
         x ="Daily max temp bins", y = "Change in annual mortality rate per additional day") +
    theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
    geom_text_repel(aes(label=na.omit(max_df)$pval), size=3)
  
  max_res <- ggplot(max_res_df, aes(x=fitted, y=residual, group=1)) +
    labs(title=paste0("Daily max ", deg, "-deg residuals"), 
         x="Fitted", y="Residual") +
    theme(plot.title=element_text(hjust=0.5)) +
    geom_point()
    
    
  drn <- ggplot(na.omit(drn_df), aes(x=bin, y=coeff, group=1)) + 
    geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
    geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
    geom_line() + 
    geom_point(shape=21, fill="white") +
    labs(title=paste0("Diurnal range ", deg, "-deg temp bins"),
         x ="Diurnal range temp bins", y = "Change in annual mortality rate per additional day") +
    theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
    geom_text_repel(aes(label=na.omit(drn_df)$pval), size=3)
  
  drn_res <- ggplot(drn_res_df, aes(x=fitted, y=residual, group=1)) +
    labs(title=paste0("Diurnal range ", deg, "-deg residuals"), 
         x="Fitted", y="Residual") +
    theme(plot.title=element_text(hjust=0.5)) +
    geom_point()
    
    
  grid.arrange(avg, avg_res, min, min_res, max, max_res, drn, drn_res, ncol=2, nrow=4)
}


# three deg models
three_dg_df <- create_binned_df(3, panel)
three_avg_formula <- as.formula(paste("Total ~", paste(names(three_dg_df)[grepl("AVG", names(three_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
three_min_formula <- as.formula(paste("Total ~", paste(names(three_dg_df)[grepl("MIN", names(three_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
three_max_formula <- as.formula(paste("Total ~", paste(names(three_dg_df)[grepl("MAX", names(three_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
three_drn_formula <- as.formula(paste("Total ~", paste(names(three_dg_df)[grepl("DIURNAL", names(three_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))

three_avg_eqn <- felm(three_avg_formula, data=three_dg_df)
three_min_eqn <- felm(three_min_formula, data=three_dg_df)
three_max_eqn <- felm(three_max_formula, data=three_dg_df)
three_drn_eqn <- felm(three_drn_formula, data=three_dg_df)

get_bin_graphs(three_avg_eqn, three_min_eqn, three_max_eqn, three_drn_eqn, get_bin_names(names(three_avg_eqn$beta[, 1])), 3)


# four deg models
four_dg_df <- create_binned_df(4, panel)
four_avg_formula <- as.formula(paste("Total ~", paste(names(four_dg_df)[grepl("AVG", names(four_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
four_min_formula <- as.formula(paste("Total ~", paste(names(four_dg_df)[grepl("MIN", names(four_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
four_max_formula <- as.formula(paste("Total ~", paste(names(four_dg_df)[grepl("MAX", names(four_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
four_drn_formula <- as.formula(paste("Total ~", paste(names(four_dg_df)[grepl("DIURNAL", names(four_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))

four_avg_eqn <- felm(four_avg_formula, data=four_dg_df)
four_min_eqn <- felm(four_min_formula, data=four_dg_df)
four_max_eqn <- felm(four_max_formula, data=four_dg_df)
four_drn_eqn <- felm(four_drn_formula, data=four_dg_df)

get_bin_graphs(four_avg_eqn, four_min_eqn, four_max_eqn, four_drn_eqn, get_bin_names(names(four_avg_eqn$beta[, 1])), 4)


# five deg models
five_dg_df <- create_binned_df(5, panel)
five_avg_formula <- as.formula(paste("Total ~", paste(names(five_dg_df)[grepl("AVG", names(five_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
five_min_formula <- as.formula(paste("Total ~", paste(names(five_dg_df)[grepl("MIN", names(five_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
five_max_formula <- as.formula(paste("Total ~", paste(names(five_dg_df)[grepl("MAX", names(five_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))
five_drn_formula <- as.formula(paste("Total ~", paste(names(five_dg_df)[grepl("DIURNAL", names(five_dg_df))], collapse="+"), "| ZIPCODE + YEAR"))

five_avg_eqn <- felm(five_avg_formula, data=five_dg_df)
five_min_eqn <- felm(five_min_formula, data=five_dg_df)
five_max_eqn <- felm(five_max_formula, data=five_dg_df)
five_drn_eqn <- felm(five_drn_formula, data=five_dg_df)

get_bin_graphs(five_avg_eqn, five_min_eqn, five_max_eqn, five_drn_eqn, get_bin_names(names(five_avg_eqn$beta[, 1])), 5)
  

# seven deg
seven_deg_df <- create_binned_df(7, panel)
seven_avg_formula <- as.formula(paste("Total ~", paste(names(seven_deg_df)[grepl("AVG", names(seven_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
seven_min_formula <- as.formula(paste("Total ~", paste(names(seven_deg_df)[grepl("MIN", names(seven_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
seven_max_formula <- as.formula(paste("Total ~", paste(names(seven_deg_df)[grepl("MAX", names(seven_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
seven_drn_formula <- as.formula(paste("Total ~", paste(names(seven_deg_df)[grepl("DIURNAL", names(seven_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))

seven_avg_eqn <- felm(seven_avg_formula, data=seven_deg_df)
seven_min_eqn <- felm(seven_min_formula, data=seven_deg_df)
seven_max_eqn <- felm(seven_max_formula, data=seven_deg_df)
seven_drn_eqn <- felm(seven_drn_formula, data=seven_deg_df)

get_bin_graphs(seven_avg_eqn, seven_min_eqn, seven_max_eqn, seven_drn_eqn, get_bin_names(names(seven_avg_eqn$beta[, 1])), 7)


# nine deg 
nine_deg_df <- create_binned_df(9, panel)
nine_avg_formula <- as.formula(paste("Total ~", paste(names(nine_deg_df)[grepl("AVG", names(nine_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
nine_min_formula <- as.formula(paste("Total ~", paste(names(nine_deg_df)[grepl("MIN", names(nine_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
nine_max_formula <- as.formula(paste("Total ~", paste(names(nine_deg_df)[grepl("MAX", names(nine_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))
nine_drn_formula <- as.formula(paste("Total ~", paste(names(nine_deg_df)[grepl("DIURNAL", names(nine_deg_df))], collapse="+"), "| ZIPCODE + YEAR"))

nine_avg_eqn <- felm(nine_avg_formula, data=nine_deg_df)
nine_min_eqn <- felm(nine_min_formula, data=nine_deg_df)
nine_max_eqn <- felm(nine_max_formula, data=nine_deg_df)
nine_drn_eqn <- felm(nine_drn_formula, data=nine_deg_df)

get_bin_graphs(nine_avg_eqn, nine_min_eqn, nine_max_eqn, nine_drn_eqn, get_bin_names(names(nine_avg_eqn$beta[, 1])), 7)


# trying to get multiple lines on one plot guh.. 
# ggplot() + 
#   geom_ribbon(data=na.omit(three_avg_df), aes(ymin = coeff - se, ymax = coeff + se), fill = "lightblue") +
#   geom_errorbar(data=na.omit(three_avg_df), aes(ymin=coeff-se, ymax=coeff+se), width=.1, color="lightblue") + 
#   geom_line(data=na.omit(three_avg_df), aes(x=bin, y=coeff, group=1), color="blue") + 
#   geom_point(data=na.omit(three_avg_df), shape=21, fill="white") +
# 
#   geom_ribbon(data=na.omit(three_max_df), aes(ymin = coeff - se, ymax = coeff + se), fill = "pink") +
#   geom_errorbar(data=na.omit(three_max_df), aes(ymin=coeff-se, ymax=coeff+se), width=.1, color="pink") +
#   geom_line(data=na.omit(three_max_df), aes(x=bin, y=coeff, group=1), color="blue") +
#   geom_point(data=na.omit(three_max_df, shape=21, fill="white"))


# heatwave model
heatwave <- felm(Total ~ HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR, data = panel)
summary(heatwave)

# choose a degree bin, based on p-vals...record this in a table? 
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

# TODO try messing around with additional columns -- age? cause-sepcific mrotality? can see relationship used in paper
# TODO choose a num degrees temp bin and which bin to use! and then plot with heatwaves! 
# (maybe try with all of them hmmm, and then compareee)
