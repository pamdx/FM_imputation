### EMPutator beta ###

rm(list=ls()) # clear R environment

### Load packages

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(compareDF)
library(stargazer)
library(gridExtra)

theme_set(theme_bw()) # set simple theme for charts

# Load app functions

source("./modules/functions.R")

# Data import

source("./modules/data_import.R")

# Main filtering

country_input <- "France"
OC2_input <- "Marine fishing"
start_year <- 1995
end_year <- 2020

# Subseries-related analyses

source("./modules/subseries_analysis.R")

#### Estimations parameters ####

  # Linear regression

share_valid_reg <- 0.5 # Proportion of years with official data necessary to run regression (to avoid generating estimates from too little information)
obs_threshold_linreg <- round(length(years_all) * share_valid_reg) # Do not modify
reg_type <- 1 # Regression type 1 = automatic (runs predetermined models and selects the one with best fit), 2 = manual (see below)
trend <- seq(start_year:end_year) # Do not modify
reg_dynamic <- emp_value ~ prod_value + labor_value # Specify manual regression by choosing independent variables from: trend, prod_value, labor_value, fleet_value (separated by "+")
fit_threshold_reg <- 0.8 # R2-squared threshold for the regression to be taken in consideration

  # Polynomial trends

share_valid_trend <- 0.5 # Proportion of years with official data necessary to run regression (to avoid generating estimates from too little information)
obs_threshold_trend <- round(length(years_all) * share_valid_trend) # Do not modify
fit_threshold_trend <- 0.8 # R2-squared threshold for the regression to be taken in consideration

  # Historical growth/average

histavg_threshold <- 5 # Number of previous years on which to base historical average estimates
histgrowth_threshold <- 5 # Number of previous years on which to base historical growth  estimates

#####

# Aggregated imputation

source("./modules/processing_aggregated.R")

agg_imputation_type <- 1 # 1 = "Consecutive", 2 = "Year-by-year"

source("./modules/imputation_aggregated.R")

# Subseries imputation

source("./modules/imputation_subseries.R")

# Final data export and report generation

source("./modules/final_data_export_viz.R")
