### EMPutator beta ###

rm(list=ls()) # clear R environment

# Load packages

pkgs <- c("dplyr", "ggplot2", "readr", "tidyr", "tibble", "compareDF", "stargazer", "gridExtra", "rmarkdown")
lapply(pkgs, require, character.only = TRUE) # One-liner to load all packages

# Load app functions

source("./modules/functions.R")

# Load data from inputs folder

source("./modules/data_import.R")

# Main filtering

country_input <- select_country(FM_raw, country_input)
OC2_input <- select_sector(FM_raw, country_input)
start_year <- 1995
end_year <- 2020

# Subseries-related analyses

source("./modules/subseries_analysis.R")

#### Estimations parameters ####

  # Linear regression

share_valid_reg <- 0.3 # Proportion of years with official data necessary to run regression (to avoid generating estimates from too little information)
obs_threshold_linreg <- round(length(years_all) * share_valid_reg) # Do not modify
reg_type <- 1 # Regression type 1 = automatic (runs predetermined models and selects the one with best fit), 2 = manual (see below)
trend <- seq(start_year:end_year) # Do not modify
reg_dynamic <- emp_value ~ prod_value + trend # Specify manual regression by choosing independent variables from: trend, prod_value, labor_value, fleet_value (separated by "+")
fit_threshold_reg <- 0 # R2-squared threshold for the regression to be taken in consideration

  # Polynomial trends

share_valid_trend <- 0.3 # Proportion of years with official data necessary to run regression (to avoid generating estimates from too little information)
obs_threshold_trend <- round(length(years_all) * share_valid_trend) # Do not modify
fit_threshold_trend <- 0 # R2-squared threshold for the regression to be taken in consideration

  # Historical growth/average

histavg_threshold <- 5 # Number of previous years on which to base historical average estimates
histgrowth_threshold <- 5 # Number of previous years on which to base historical growth estimates

#####

# Aggregated imputation

source("./modules/processing_aggregated.R")

agg_imputation_type <- 1 # Select 1 to apply estimation to all missing consecutive years, select 2 to apply estimation separately to each year in the period with consecutive missing years.

source("./modules/imputation_aggregated.R")

# Subseries imputation

source("./modules/imputation_subseries.R")

# Final data export and report generation

source("./modules/final_data_export_viz.R")
