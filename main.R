### EMPutator beta ###

rm(list=ls())

### Load packages

# library(tidyverse)

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(compareDF)
library(Rilostat)
library(stargazer)
library(gridExtra)

theme_set(theme_bw())

# App parameters

  # Data import

country_input <- "Afghanistan"
OC1_input <- "Aquaculture"
start_year <- 1995
end_year <- 2018

  # Reg

share_valid_reg <- 0.3
obs_threshold_linreg <- round(length(years_all)*share_valid_reg)
reg_type <- 1
trend <- seq(start_year:end_year)
reg_dynamic <- emp_value ~ trend # Choose independent variables from: trend, prod_value, labor_value

  # Trend

share_valid_trend <- 0.3
obs_threshold_trend <- round(length(years_all)*share_valid_trend)

  # LBFH

histavg_threshold <- 5 # Number of previous years on which to base estimates, make as interactive input in Shiny App
histgrowth_threshold <- 5

# Run functions

source("./modules/functions_shiny.R")

# Data import

source("./modules/data_import_shiny.R")

# Subseries-related analyses

source("./modules/subseries_analysis.R")

# Aggregated imputation

source("./modules/imputation_aggregated.R")

# Subseries imputation

source("./modules/imputation_subseries.R")

# Final data export and visualization

source("./modules/final_data_export_viz.R")
