### EMPutator beta ###

rm(list=ls()) # clear R environment

# Load packages

pkgs <- c("dplyr", "ggplot2", "readr", "tidyr", "tibble", "compareDF", "stargazer", "gridExtra", "rmarkdown")
lapply(pkgs, require, character.only = TRUE) # One-liner to load all packages

# Load app functions

source("./modules/functions.R")

# Load data from inputs folder

source("./modules/data_import.R")

# Load app parameters

source("parameters.R")

# Subseries-related analyses

source("./modules/subseries_analysis.R")

# Aggregated imputation

source("./modules/processing_aggregated.R")

agg_imputation_type <- 1 # Select 1 to apply estimation to all missing consecutive years, select 2 to apply estimation separately to each year in the period with consecutive missing years.

source("./modules/imputation_aggregated.R")

# Subseries imputation

source("./modules/imputation_subseries.R")

# Final data export and report generation

source("./modules/final_data_export_viz.R")
