### EMPutator beta ###

rm(list=ls()) # clear R environment

#### Paths to scripts and folders ####

input_directory <- "./inputs/"
output_directory <- "./outputs/"

path_data_import <- "./modules/data_import.R"
path_final_data_export_viz <- "./modules/final_data_export_viz.R"
path_functions <- "./modules/functions_FM_imputation.R"
path_imputation_aggregated <- "./modules/imputation_aggregated.R"
path_imputation_subseries <- "./modules/imputation_subseries.R"
path_initialization <- "./modules/initialization.R"
path_inputs_update <- "./modules/inputs_update.R"
path_processing_aggregated <- "./modules/processing_aggregated.R"
path_report_imputation <- "./modules/report_FM_imputation.Rmd"
path_subseries_analysis <- "./modules/subseries_analysis.R"

path_parameters_estimations <- "./parameters/parameters_estimations.R"
path_parameters_imputation <- "./parameters/parameters_FM_imputation.R"

#### ####

#### Update input data (uncomment if needed) ####

# source(path_inputs_update)

#### ####

# Initialize app

source(path_initialization)

# Load estimation parameters

source(path_parameters_estimations)

# Aggregated imputation

source(path_processing_aggregated)

source(path_imputation_aggregated)

# Subseries imputation

source(path_imputation_subseries)

# Final data export and report generation

source(path_final_data_export_viz)
