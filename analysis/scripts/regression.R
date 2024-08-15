# load libraries
library(lfe)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(fixest)
library(corrplot)
library(dplyr)

# get panel data (just annual measures + mortality for now) 
panel_path <- file.path("analysis", "processed", "merged", "measures_mortality.csv")
panel <- read.csv(panel_path)

# get correlation matrix for each type of bin, and visualize
get_correlation_matrix <- function(df, title) {
  # filter out cols with sd = 0 (cols w all same entries)
  df <- Filter(function(x) sd(x) != 0, df)
  names(df) <- get_bin_names(names(df))
  corrplot(cor(df), method="color", tl.cex=0.5, main=title, mar=c(2, 2, 2, 2)) # tl.cex makes text smaller 
}

get_correlation_matrix(panel[grepl("AVG", names(panel))], "Avg one-deg temp bins")
get_correlation_matrix(panel[grepl("MIN", names(panel))], "Min one-deg temp bins")
get_correlation_matrix(panel[grepl("MAX", names(panel))], "Max one-deg temp bins")
get_correlation_matrix(panel[grepl("DIURNAL", names(panel))], "Diurnal one-deg temp bins")


# create df with num_degrees degree bins 
get_binned_df <- function(num_degrees, df_full) {
  # create copy of df_full, to modify
  df_bins <- df_full[, grepl("DAILY|DIURNAL", names(df_full))]
  df_not_bins <- df_full[, !colnames(df_full) %in% colnames(df_bins)]
  df_final <- data.frame(df_not_bins) 
  
  # get relevant idxs for one bin "type" (here, AVG) 
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
  
  # get idxs for remaining bin "types" (min, max, diurnal temp bins)
  idxs <- c(root_idxs, root_idxs + 42, root_idxs + 2 * 42, root_idxs + 3 * 42)
  
  # sum relevant rows, and join to df_final
  for (i in seq(1, length(idxs) - 1, by=2)) {
    idx <- idxs[i]
    idx_1 <- idxs[i + 1]
    
    # get str before second-to-last underscore
    prefix <- str_extract(names(df_bins)[idx], ".*(?=_[^_]*_[^_]*$)") 
    # get str between second-to-last and last underscore
    first_num <- str_extract(names(df_bins)[idx], "(?<=_)[^_]*(?=_[^_]*$)")
    # get str after last underscore
    second_num <-  str_extract(names(df_bins)[idx_1], "(?<=_)[^_]+$")
    
    # get prefix of colname 
    colname <- paste(prefix, first_num, second_num, sep="_")
    df_final[, colname] <- c(df_bins[, idx:idx_1] %>% rowSums)
  }
  
  return(df_final)
}

# get bins in interval notation to use for visualizations 
get_bin_names <- function(curr_names) {
  bin_names <- c()
  for (name in curr_names) {
    first_num <- str_extract(name, "(?<=_)[^_]*(?=_[^_]*$)")
    second_num <-  str_extract(name, "(?<=_)[^_]+$")
    
    if (first_num == "Inf") {
      bin_names <- c(bin_names, paste0("<", second_num))
    }
    else if (second_num == "Inf") {
      # use greater than or equal symbol's unicode 
      bin_names <- c(bin_names, paste0("\u2265", first_num)) 
    }
    else {
      bin_names <- c(bin_names, paste0("[", first_num, ",", second_num, ")"))
    }
  }
  return(bin_names)
}

# get age groups in interval notation, to use for visualizations
get_age_names <- function(curr_names) {
  age_names <- c()
  for (i in 1:length(curr_names)) {
    name <- curr_names[[i]]
    first_num <- str_extract(name, "(?<=_)[^_]*(?=_[^_]*$)")
    second_num <-  str_extract(name, "(?<=_)[^_]+$")
    
    if (i == 1) {
      age_names <- c(age_names, paste0("\u2264", second_num))
    }
    else if (i == length(curr_names)) {
      # get first nums only, since very laste age group follows "Age_85_and_Over" format
      num <- str_extract(name, "\\d+") 
      age_names <- c(age_names, paste0("\u2265", num))
    }
    else {
      age_names <- c(age_names, paste0("[", first_num, ",", second_num, "]"))
    }
  }
  return(age_names)
}

#' Merge consecutive columns with zero variance with nearest column
#' with non-zero variance, to eliminate NA coeffs in regression
#'
#' @param binned_df Dataframe; checks each col's variance
#' @return A dataframe with merged cols. Does not change original dataframe
merge_no_var_cols <- function(binned_df) {
  # get idxs of cols with sd = 0 
  zero_sd <- (sapply(binned_df, sd) == 0) %>% which

  # group together indices that r consecutive, aka indices that have a difference = 1 with neighbor index
  consec <- c(0, cumsum(diff(zero_sd) > 1))
  idxs <- split(zero_sd, consec)
  
  if (length(idxs) != 1) {
    # merge cols using idxs
    for (i in 1:length(idxs)) {
      # merge interval with the col either before interval or after interval
      first_idx <- idxs[[i]][1]
      second_idx <- idxs[[i]][length(idxs[[i]])]
      
      if (first_idx == 1) {
        second_idx <- second_idx + 1
        idxs[[i]] <- append(idxs[[i]], second_idx) %>% sort
      }
      else {
        first_idx <- first_idx - 1
        idxs[[i]] <- append(idxs[[i]], first_idx) %>% sort
      }
      
      old_col_name_1 <- names(binned_df)[first_idx]
      old_col_name_2 <- names(binned_df)[second_idx]
      
      # get str before second-to-last underscore
      prefix <- str_extract(old_col_name_1, ".*(?=_[^_]*_[^_]*$)") 
      # get str between second-to-last and last underscore
      first_num <- str_extract(old_col_name_1, "(?<=_)[^_]*(?=_[^_]*$)")
      # get str after last underscore
      second_num <-  str_extract(old_col_name_2, "(?<=_)[^_]+$")
      
      new_col_name <- paste(prefix, first_num, second_num, sep="_")
      new_col <- rowSums(binned_df[first_idx:second_idx])
      
      binned_df[new_col_name] <- new_col
    }
    
    # drop cols, reorder 
    binned_df <- binned_df[, -unlist(idxs)]
    reordered_names <- names(binned_df)[names(binned_df) %>% str_extract("\\d+") %>% as.numeric %>% order]
    binned_df <- binned_df[, reordered_names]
    
  }
  return(binned_df)
}

#' Omit bin containing reference bin to avoid collinearity
#' 
#' @param binned_df Dataframe to filter bins from 
#' @param bin String indicator for bin "type" 
#' @param ref Number representing reference bin
#' @return A vector of filtered bin names
omit_bin <- function(binned_df, bin, ref) {
  curr_bin_names <- names(binned_df)[grepl(bin, names(binned_df))]
  
  # get nums corresponding to each bin name
  parsed <- lapply(curr_bin_names, function(x) {
    nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+", x))))
    return(c(min(nums), max(nums)))
  })
  
  # get all names except for name containing ref 
  filtered_bin_names <- curr_bin_names[!sapply(parsed, function(x) {
    x[1] <= ref & ref < x[2]
  })]
  
  return(filtered_bin_names)
}


#' Generate all-cause mortality vs temp bins, for all bin types (and heatwave counts)
#' 
#' @param df Panel data (dataframe) to extract data from 
#' @param bins A vector of identifiers for each bin "type" ("AVG" for avg temp bins, "MIN" for min temp bins, etc)
#' @param deg A number representing how large each bin interval should be
#' @param ref A number representing the bin to omit, to serve as our reference bin
#' @param response A string representing the response variable column name in df
#' @return summaries A list of summaries for each linear regression
get_bin_graphs <- function(df, bins, deg, ref, response) { 
  grids <- list() 
  summaries <- list() 
  
  # get binned df, where cols with 0 variance are merged together
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols()

  # get plot for each bin
  for (i in 1:length(bins)) {
    bin <- bins[i]
    bin_names <- names(binned_df)[grepl(bin, names(binned_df))]
  
    # omit bin containing ref to avoid collinearity 
    filt_bin_names <- omit_bin(binned_df, bin, ref)
    bin_renamed <- get_bin_names(filt_bin_names)
    
    # create eqn 
    eqn <- as.formula(paste(response, "~", paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
    reg <- felm(eqn, data=binned_df)
    summaries[[bin]] <- summary(reg)
    
    # create df from reg 
    reg_df <- data.frame(bin=bin_renamed, coeff=reg$coefficients[, 1], se=reg$se,
                         pval=format(round(reg$pval, digits=2), nsmall=2)) %>% na.omit
    reg_df$bin <- factor(reg_df$bin, levels=reg_df$bin) # ensure that x ticks r labelled how we want
    
    # get plot
    plt_title <- paste0("Daily ", tolower(bin), " ", deg, "-degree temp bins")
    plt_x <- paste("Daily", tolower(bin), "temp bins")
    plt_y <- "Change in annual mortality\nrate per additional day"
    plt <- ggplot(reg_df, aes(x=bin, y=coeff, group=1)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
      geom_line() + 
      geom_point(shape=21, fill="white") +
      labs(title=plt_title, x=plt_x, y=plt_y) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
      geom_text_repel(aes(label=pval), size=3)
    
    grids[[i]] <- plt
  }
  
  do.call("grid.arrange", grids)
  return(summaries)
}

# run some models
bins <- c("AVG", "MIN", "MAX", "DIURNAL")
three <- get_bin_graphs(panel, bins, 3, 12, "Total")
four <- get_bin_graphs(panel, bins, 4, 12, "Total")
five <- get_bin_graphs(panel, bins, 5, 12, "Total")
seven <- get_bin_graphs(panel, bins, 7, 12, "Total")


#' Get all-cause heatwave graph 
#' @param df Panel dataframe
#' @param response A string representing the response variable column name in df
#' @return Summary of heatwave regression
get_heatwave_graph <- function(df, response) {
  eqn <- as.formula(paste(response, "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR"))
  heatwave <- felm(formula=eqn, data=df)
  heatwave_df <- data.frame(x=df[, "HEATWAVE_CNT"], y=df[, response])
  
  # get data from predicted polynomial
  x_vals <- seq(01, 16, by=0.5)
  predict_vals <- function(x) {
    as.numeric(heatwave$coefficients[1])*x +
      as.numeric(heatwave$coefficients[2])*x^2 +
      as.numeric(heatwave$coefficients[3])*x^3
  }
  y_vals <- predict_vals(x_vals)
  line_data <- data.frame(x=x_vals, y=y_vals) 
  
  # get plot 
  plt <- ggplot(heatwave_df, aes(x, y)) +
    geom_point() +
    geom_line(line_data, mapping=aes(x, y)) +
    labs(title=paste("Mortality rates vs heatwave day counts"),
         x ="Heatwave day counts", y = "Mortality rate") +
    theme(plot.title=element_text(hjust=0.5))
  
  grid.arrange(plt)
  return(summary(heatwave))
}

heatwave <- get_heatwave_graph(panel, "Total")

#' Get age-stratified mortaltiy rates vs bins 
#' @param df Panel data dataframe
#' @param deg Number representing degree itnerval width
#' @param bins Vector of bin "types" (avg, min, max, diurnal)
get_age_stratified_graphs <- function(df, deg, bins, ref) {
  # group bins based on deg 
  binned_df <- get_binned_df(deg, df) 
  
  # get formulas and eqns for each age group, for each bin
  grids <- list()
  eqns <- list()
  for (i in 1:length(bins)) {
    # omit ref  
    bin <- bins[i]
    bin_df <- binned_df[grepl(bin, names(binned_df))] %>% merge_no_var_cols
    filtered_bin_names <- omit_bin(bin_df, bin, ref) 
    # TODO check that filtered bin names is working right urmmmm

    # get col names (for bins) 
    bin_names <- get_bin_names(filtered_bin_names)
    age_names <- get_age_names(names(binned_df)[grepl("Age", names(binned_df))])
    actual_age_names <- names(binned_df)[grepl("Age", names(binned_df))]
    
    
    plt_df <- data.frame()
    for (j in 1:length(age_names)) {
      # get formula, eqn 
      age_formula <- as.formula(paste(actual_age_names[j], "~", paste(filtered_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
      age_eqn <- felm(age_formula, data=binned_df)
      eqns[[paste(bin, actual_age_names[j])]] <- summary(age_eqn)
      
      # add to df 
      age_df <- data.frame(bin=bin_names, coeff=age_eqn$beta[, 1], se=age_eqn$se, 
                           pval=format(round(age_eqn$pval, digits=2), nsmall=2),
                           age_group=age_names[j]) %>% na.omit
      age_df$bin <- factor(age_df$bin, levels=age_df$bin)
      plt_df <- rbind(plt_df, age_df)
    }

    # create plot 
    plt_title <- paste("Age-stratified", bin %>% tolower, paste0(deg, "-deg"), "temp bins")
    plt_x <- paste("Daily", bin %>% tolower, "temp bins")
    plt_y <- "Change in annual mortality\nrate per additional day"

    plt <- ggplot(plt_df, aes(x=bin, y=coeff, group=age_group)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se, fill=age_group), alpha=0.3) + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se, color=age_group), show.legend=FALSE, width=.1) + 
      geom_line(aes(color=age_group), show.legend=FALSE) + 
      geom_point(aes(color=age_group), show.legend=FALSE) +
      labs(title=plt_title, x=plt_x, y=plt_y) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
      scale_fill_discrete(breaks=age_names, name="Age group") 
    
    grids[[i]] <- plt
  }
  
  do.call("grid.arrange", grids)
  return(eqns)
}

get_age_stratified_heatwave_graph <- function(df) {
  # get age col names  
  age_names <- get_age_names(names(df)[grepl("Age", names(df))])
  actual_age_names <- names(df)[grepl("Age", names(df))]
  summaries <- list()
  
  # get heatwave plot
  heatwave_df <- data.frame()
  for (i in 1:length(age_names)) {
    # get heatwave eqn, df 
    heatwave_formula <- as.formula(paste(actual_age_names[i], "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3)", "| ZIPCODE + YEAR"))
    heatwave <- felm(heatwave_formula, data = df)
    
    # get predicted y vals
    x_vals <- seq(0, 16, by=0.5)
    predict_vals <- function(x) {
      as.numeric(heatwave$coefficients[1])*x +
      as.numeric(heatwave$coefficients[2])*x^2 +
      as.numeric(heatwave$coefficients[3])*x^3
    }
    y_vals <- predict_vals(x_vals)

    data <- data.frame(x=x_vals, y=y_vals, age_group=age_names[i]) 
    heatwave_df <- rbind(heatwave_df, data)
    
    # add eqn to list 
    summaries[[actual_age_names[i]]] <- summary(heatwave)
  }
  
  # get heatwave plot!
  heatwave_df$age_group <- factor(heatwave_df$age_group, levels=age_names)
  
  plt <- ggplot(heatwave_df, aes(x, y, group=age_group)) + 
    geom_line(aes(color=age_group)) + 
    labs(title="Age-stratified heatwave day counts vs mortality rates", x="Heatwave day counts", y="Mortality rate") +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_fill_discrete(name="Age group") 
  
  grid.arrange(plt)
  return(summaries)
}

bins <- c("AVG", "MIN", "MAX", "DIURNAL")
heat <- get_age_stratified_heatwave_graph(panel) 
age_graphs <- get_age_stratified_graphs(panel, 5, bins, 12)

#' Get all-age cause-specific graphs for avg temp  
#' bins should be a list mapping og name to wanted name (avg -> average)
get_cause_specific_graphs <- function(df, bins, deg, ref) {
  # iterate through names of causes, which will serve as response var
  
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols
  
  # omit reference 

  # get relevant cols 
  not_cause_df <- binned_df[grepl("DAILY|DIURNAL|Age|HEATWAVE|YEAR|ZIPCODE", names(binned_df))]
  cause_df <- binned_df[!names(binned_df) %in% names(not_cause_df)]
  cause_names_unordered <- names(cause_df)
  cause_names <- cause_names_unordered[order(cause_names_unordered)]
  
  
  summaries <- list()
  grids <- list()
  for (i in 1:length(cause_names)) {
    cause <- cause_names[[i]]
    cause_df <- data.frame()
    bin_summaries <- list()
    for (bin in names(bins)) {
      bin_df <- binned_df[grepl(bin, names(binned_df))]
      bin_names <- get_bin_names(names(bin_df))
      
      # get eqn and dfs 
      eqn <- as.formula(paste(cause, "~", paste(names(bin_df), collapse="+"), "| ZIPCODE + YEAR"))
      reg <- felm(eqn, data=binned_df)
      
      plt_df <- data.frame(bin=bin_names, coeff=reg$coefficients[, 1], se=reg$se, bin_type=bins[[bin]]) %>% na.omit
      plt_df$bin <- factor(plt_df$bin, levels=plt_df$bin)
      
      bin_summaries[[bin]] <- summary(reg)
      cause_df <- rbind(cause_df, plt_df)
    }
    summaries[[cause]] <- bin_summaries
    
    # create plot contianing data for all bins 
    plt_title <- cause
    plt_x <- paste0("Daily ", deg, "-degree temp bins")
    plt_y <- "Change in annual mortality\nrate per additional day"
    
    plt <- ggplot(cause_df, aes(x=bin, y=coeff, group=bin_type)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se, fill=bin_type), alpha=0.3) + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se, color=bin_type), show.legend=FALSE, width=.1) + 
      geom_line(aes(color=bin_type), show.legend=FALSE) + 
      geom_point(aes(color=bin_type), show.legend=FALSE) +
      labs(title=plt_title, x=plt_x, y=plt_y) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
      scale_fill_discrete(name="Daily temp bins") 
    
    grids[[i]] <- plt
  }
  
  do.call("grid.arrange", grids)
  return(summaries)
}

bins_dict <- list(
  "AVG" = "Average",
  "MIN" = "Minimum",
  "MAX" = "Maximum", 
  "DIURNAL" = "Diurnal"
)
store <- get_cause_specific_graphs(panel, bins_dict, 5)



# iterate through each col, and get formula (plot all bins on one) 
# for (cause in names(cause_df)) {
#   # get formula and eqn
#   cause_formula <- as.formula(cause, "~", paste(names(binned_df)[grepl(bin, names(binned_df))], collapse="+"), "| ZIPCODE + YEAR"))
#   age_eqn <- felm(age_formula, data=binned_df)
#   eqns[[paste(bin, actual_age_names[j])]] <- summary(age_eqn)
#   
#   # add to df 
#   age_df <- data.frame(bin=bin_names, coeff=age_eqn$beta[, 1], se=age_eqn$se, 
#                        pval=format(round(age_eqn$pval, digits=2), nsmall=2),
#                        age_group=age_names[j]) %>% na.omit
#   age_df$bin <- factor(age_df$bin, levels=age_df$bin)
#   plt_df <- rbind(plt_df, age_df)
#   
#   # get df 
#   
#   # get plot, and append to grid  
# }

# plot grid of plots


# after dinner, work with cause speicfic mrotaltiy! and then try er data, and then try to choose best model 
# (can also clean up first get_graphs func) 

# TODO try messing around with additional columns -- age? cause-sepcific mrotality? can see relationship used in paper
# TODO try incorporating er data (the ones u have rn?) 
# TODO choose a num degrees temp bin and which bin to use! and then plot with heatwaves! 
# (maybe try with all of them hmmm, and then compareee)
# TODO incorporate itneractions w pca scoreee 
# TODO check otu presentation and final paper guh... 
# 
