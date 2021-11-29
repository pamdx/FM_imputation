### EMPutator beta ###

rm(list=ls())

### Load packages

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(compareDF)
library(stargazer)
library(gridExtra)

theme_set(theme_bw())

# Run functions

source("./modules/functions.R")

# Data import

source("./modules/data_import.R")

# Main filtering

country_input <- "Chile"
OC2_input <- "Marine fishing"
start_year <- 1995
end_year <- 2019

# Subseries-related analyses

source("./modules/subseries_analysis.R")

#### Estimations parameters ####

  # Reg

share_valid_reg <- 0
obs_threshold_linreg <- round(length(years_all) * share_valid_reg)
reg_type <- 1 # 1 = automatic, 2 = manual
trend <- seq(start_year:end_year)
reg_dynamic <- emp_value ~ prod_value + labor_value # Choose independent variables from: trend, prod_value, labor_value, fleet_value (separated by "+")
fit_threshold_reg <- 0.1

  # Trend

share_valid_trend <- 0.5
obs_threshold_trend <- round(length(years_all) * share_valid_trend)
fit_threshold_trend <- 0.1

  # Historical growth/avg

histavg_threshold <- 5 # Number of previous years on which to base estimates, make as interactive input in Shiny App
histgrowth_threshold <- 5

#####

# Aggregated imputation

source("./modules/processing_aggregated.R")

agg_imputation_type <- 1 # 1 = "Consecutive", 2 = "Year-by-year"

source("./modules/imputation_aggregated.R")

# Subseries imputation

source("./modules/imputation_subseries.R")

# Final data export and report generation

source("./modules/final_data_export_viz.R")
