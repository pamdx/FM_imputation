# Load packages

pkgs <- c("dplyr", "ggplot2", "readr", "tidyr", "tibble", "compareDF", "stargazer", "gridExtra", "rmarkdown")
lapply(pkgs, require, character.only = TRUE) # One-liner to load all packages

# Load app functions

source(path_functions)

# Load app parameters

source(path_parameters_emputator)

# Subseries-related analyses

source(path_subseries_analysis)