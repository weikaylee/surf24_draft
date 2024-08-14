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


# funcs to create dfs with degree bins 
get_binned_df <- function(num_degrees, df_full) {
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

get_age_names <- function(curr_names) {
  age_names <- c()
  for (i in 1:length(curr_names)) {
    name <- curr_names[[i]]
    first_num <- sub(".*_(.*)_.*", "\\1", name)
    second_num <- sub(".*_(.*).*", "\\1", name)
    if (i == 1) {
      age_names <- c(age_names, paste0("\u2264", second_num))
    }
    else if (i == length(curr_names)) {
      age_names <- c(age_names, paste0("\u2265", sub(".*?([0-9]+).*", "\\1", name)))
    }
    else {
      age_names <- c(age_names, paste0("[", first_num, ",", second_num, "]"))
    }
  }
  return(age_names)
}

merge_no_var_cols <- function(binned_df) {
  # get and store idxs of interval of cols with 0 variance (aka all vals are same) 
  idxs <- c()
  i <- 1
  j <- i
  num_cols <- ncol(binned_df)
  
  while (j <= num_cols) {
    if (sd(binned_df[[j]]) == 0) {
      k <- j + 1
      while (k <= num_cols && sd(binned_df[[k]]) == 0) {
        k <- k + 1
      }
      idxs[[i]] <- c(j:(k - 1))
      i <- i + 1
      j <- k
    }
    j <- j + 1
  }
  
  # merge cols using idxs (intervals of cols that need merging)
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
  reorderd_names <- names(binned_df)[names(binned_df) %>% str_extract("\\d+") %>% as.numeric %>% order]
  binned_df <- binned_df[, reordered_names]

  return(binned_df)
}

# func to generate grid of plots for all bins
# TODO, abstract more by including argument of cols that u want as output for model! 
get_bin_graphs <- function(df, bins, deg, ref) { 
  grids <- list() 
  summaries <- list() 
  
  # get binned df, where cols with 0 variance are merged together
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols
  
  for (i in 1:length(bins)) {
    bin <- bins[i]
    bin_names <- names(binned_df)[grepl(bin, names(binned_df))]
  
    # omit bin containing reference (ref) to avoid collinearity 
    parsed <- lapply(bin_names, function(x) {
      nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+", x))))
      return(c(min(nums), max(nums)))
    })
    
    filt_bin_names <- bin_names[!sapply(parsed, function(x) {
      x[1] <= ref & ref < x[2]
    })]
      
    bin_renamed <- get_bin_names(filt_bin_names)
    
    # create eqn 
    eqn <- as.formula(paste("Total ~", paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
    reg <- felm(eqn, data=binned_df)
    summaries[[bin]] <- summary(reg)
    
    # get df from reg 
    reg_df <- data.frame(bin=bin_renamed, coeff=reg$coefficients[, 1], se=reg$se,
                         pval=format(round(reg$pval, digits=2), nsmall=2)) %>% na.omit
    reg_df$bin <- factor(reg_df$bin, levels=reg_df$bin) # ensures that x ticks r labelled how we want
    
    # plot
    plt_title <- paste0("Daily ", tolower(bin), " ", deg, "-deg temp bins")
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

# three deg models
bins <- c("AVG", "MIN", "MAX", "DIURNAL")
three <- get_bin_graphs(panel, bins, 3, 12)
four <- get_bin_graphs(panel, bins, 4, 12)
five <- get_bin_graphs(panel, bins, 5, 12)
seven <- get_bin_graphs(panel, bins, 7, 12)
# TODO, try to avoid nas in summary -- if std deviation of a row is 0, ccombine col with prior cols until sd isn't 0 anymore! 


# TODO make a func for this! 
# heatwave model
zip = "90001"
heatwave <- feols(Total ~ HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR, data = panel)
heatwave_df <- data.frame(x=subset(panel, ZIPCODE==zip)$HEATWAVE_CNT, y=subset(panel, ZIPCODE==zip)$Total)
# heatwave_df <- data.frame(x=panel$HEATWAVE_CNT, y=panel$Total)

# x_vals <- runif(length(panel$HEATWAVE_CNT), min = 0, max = 20)
x_vals <- seq(1, 16)
# line_data <- data.frame(x=x_vals, y=predict(heatwave, newdata=data.frame(YEAR=subset(panel, ZIPCODE==zip)$YEAR, HEATWAVE_CNT=x_vals, ZIPCODE=zip)))
# line_data <- data.frame(x=x_vals, y=predict(heatwave, newdata=data.frame(ZIPCODE=panel$ZIPCODE, YEAR=panel$YEAR, HEATWAVE_CNT=x_vals)))

line_data <- data.frame(x=x_vals, y=x_vals*heatwave$coefficients[1] + (x_vals^2)*heatwave$coefficients[2] + (x_vals^3)*heatwave$coefficients[3])

heatwave_zip <- ggplot(heatwave_df, aes(x, y)) +
  geom_point() +
  geom_line(line_data, mapping=aes(x=x, y=y)) +
  labs(title=paste("Mortality rates vs heatwave days for", zip),
       x ="Heatwave day counts", y = "Mortality rate") +
  theme(plot.title=element_text(hjust=0.5))

heatwave_res <- ggplot(data.frame(fitted=heatwave$fitted.values, residual=heatwave$residuals), aes(x=fitted, y=residual, group=1)) +
  labs(title="Heatwave residuals",
       x="Fitted", y="Residual") +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_point()

grid.arrange(heatwave_zip, heatwave_res, ncol=2, nrow=1)


# get age-stratified mortality vs bins (and vs heatwaves) 
get_age_stratified_graphs <- function(df, deg, bins) {
  # group bins based on deg, get long df based on groups 
  binned_df <- get_binned_df(deg, df) 
  
  # get col names (for bins) 
  bin_names <- get_bin_names(names(binned_df)[grepl("DIURNAL", names(binned_df))])
  age_names <- get_age_names(names(binned_df)[grepl("Age", names(binned_df))])
  actual_age_names <- names(binned_df)[grepl("Age", names(binned_df))]

  # get formulas and eqns for each age group, for each bin (start w one bin for now) 
  grids <- list()
  eqns <- list()
  for (i in 1:length(bins)) {
    bin <- bins[i]
    plt_df <- data.frame()
    for (j in 1:length(age_names)) {
      # get formula, eqn 
      age_formula <- as.formula(paste(actual_age_names[j], "~", paste(names(binned_df)[grepl(bin, names(binned_df))], collapse="+"), "| ZIPCODE + YEAR"))
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
  
  # get heatwave plot
  heatwave_df <- data.frame()
  for (i in 1:length(age_names)) {
    # get heatwave eqn, df 
    heatwave_formula <- as.formula(paste(actual_age_names[i], "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3)", "| ZIPCODE + YEAR"))
    heatwave <- felm(heatwave_formula, data = binned_df)
    
    # get predicted y vals usi
    x_vals <- seq(0, 16, by=0.1)
    predict_vals <- function(x) {
      as.numeric(heatwave$coefficients[1])*x +
      as.numeric(heatwave$coefficients[2])*x^2 +
      as.numeric(heatwave$coefficients[3])*x^3
    }
    y_vals <- predict_vals(x_vals)
    heat_df <- data.frame(x=x_vals, y=y_vals, age_group=age_names[i]) # df of perdicted vals in each age group 
    heatwave_df <- rbind(heatwave_df, heat_df)
    
    # add eqn to list 
    eqns[[paste("HEATWAVE", actual_age_names[i])]] <- summary(heatwave)
    
  }
  
  # get heatwave plot!
  heatwave_df$age_group <- factor(heatwave_df$age_group, levels=age_names)

  heat_plt <- ggplot(heatwave_df, aes(x, y, group=age_group)) + 
    geom_line(aes(color=age_group)) + 
    labs(title="Age-stratified heatwave counts vs mortality rates", x="Heatwave day counts", y="Mortality rate") +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_fill_discrete(name="Age group") 
    
  
  grids[[length(grids) + 1]] <- heat_plt
  
  do.call("grid.arrange", grids)
  return(eqns)
}

bins <- c("AVG", "MIN", "MAX", "DIURNAL")
eqns <- get_age_stratified_graphs(panel, 5, bins) 


# get cause specific! (both all-age and age-specific) 
# get relevant cols 
not_cause_df <- panel[grepl("DAILY|DIURNAL|Age|HEATWAVE|YEAR|ZIPCODE", names(panel))]
cause_df <- panel[!names(panel) %in% names(not_cause)]

get_cause_specific_bin_graphs <- function(df, deg, bins) {
  # group bins based on deg
  binned_df <- get_binned_df(deg, df) 
  
  # get col names (for bins) 
  bin_names <- get_bin_names(names(binned_df)[grepl("DIURNAL", names(binned_df))])

  # get cause cols and name
  not_cause_df <- binned_df[grepl("DAILY|DIURNAL|Age|HEATWAVE|YEAR|ZIPCODE", names(binned_df))]
  cause_df <- binned_df[!names(binned_df) %in% names(not_cause)] # includes total 
  
  # get formulas and eqns for cause, for each bin 
  # TODO start here! eeeeeeek
  grids <- list()
  eqns <- list()
  for (i in 1:length(bins)) {
    bin <- bins[i]
    plt_df <- data.frame()
    for (j in 1:length(age_names)) {
      # get formula, eqn 
      age_formula <- as.formula(paste(actual_age_names[j], "~", paste(names(binned_df)[grepl(bin, names(binned_df))], collapse="+"), "| ZIPCODE + YEAR"))
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
  
  # get heatwave plot
  heatwave_df <- data.frame()
  for (i in 1:length(age_names)) {
    # get heatwave eqn, df 
    heatwave_formula <- as.formula(paste(actual_age_names[i], "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3)", "| ZIPCODE + YEAR"))
    heatwave <- felm(heatwave_formula, data = binned_df)
    
    # get predicted y vals usi
    x_vals <- seq(0, 16, by=0.1)
    predict_vals <- function(x) {
      as.numeric(heatwave$coefficients[1])*x +
        as.numeric(heatwave$coefficients[2])*x^2 +
        as.numeric(heatwave$coefficients[3])*x^3
    }
    y_vals <- predict_vals(x_vals)
    heat_df <- data.frame(x=x_vals, y=y_vals, age_group=age_names[i]) # df of perdicted vals in each age group 
    heatwave_df <- rbind(heatwave_df, heat_df)
    
    # add eqn to list 
    eqns[[paste("HEATWAVE", actual_age_names[i])]] <- summary(heatwave)
    
  }
  
  # get heatwave plot!
  heatwave_df$age_group <- factor(heatwave_df$age_group, levels=age_names)
  
  heat_plt <- ggplot(heatwave_df, aes(x, y, group=age_group)) + 
    geom_line(aes(color=age_group)) + 
    labs(title="Age-stratified heatwave counts vs mortality rates", x="Heatwave day counts", y="Mortality rate") +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_fill_discrete(name="Age group") 
  
  
  grids[[length(grids) + 1]] <- heat_plt
  
  do.call("grid.arrange", grids)
  return(eqns)
}

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
