# FAO-OECD Employment Data Imputation Tool (FO-EDIT)

## Getting started

### Prerequisites

To run the tool, you need the following installed on your computer.
- A recent R installation (I used version 4.1.0 to build the tool)
- A recent RStudio installation (I use 1.4.1103)
- The following R packages installed: dplyr, ggplot2, readr, tidyr, tibble, compareDF, stargazer, gridExtra, Rilostat, OECD. 

You can install them by running the following code in R:  
``` R
install.packages(
  c("dplyr", "ggplot2", "readr", "tidyr", "tibble", "compareDF", "stargazer", "gridExtra", "Rilostat", "OECD")
  )
```

### Installation

1. Extract the contents of the compressed folder to the destination of your choice (e.g. on your Desktop).
2. Double-click the "emputator.Rproj" file in the main folder.
3. The tool will open in RStudio.

## How to use the imputation tool

### Loading basic packages, functions and data

First, in the main.R script, rule the code block below. It will load the packages, functions and data necessary for the tool to run properly.
``` R
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
```

### Setting filtering parameters and generating a chart of existing data

The next step is to tell the tool which country, sector and period needs to be imputed. Specify those in the code block below, and then run it to generate a visualization of the existing official and estimated data. A "rainbow" bar chart will be displayed in the Plots panel of RStudio.

``` R
# Main filtering

country_input <- "France"
OC2_input <- "Marine fishing"
start_year <- 1995
end_year <- 2020

# Subseries-related analyses

source("./modules/subseries_analysis.R")
```

### Setting imputation parameters

Next, the user can change the code below to modify how estimates are calculated. Regardless of whether the code is modified, it needs to be run, otherwise the imputation scripts will fail.

``` R
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
```

### Choosing the imputation mode

There are two ways to perform the imputation of missing values: 
- by generating aggregated imputed values for years with no data, which are then disaggregated based on the weights of subseries for years with official data ("aggregated imputation", most convenient and suitable for most cases) 
- by imputing one subseries at a time ("subseries imputation", more suitable in cases where only some subseries need to be estimated for a given year)

#### Aggregated imputation

To run aggregated imputation, execute the code below. You can choose between two types of aggregated imputations by setting the "agg_imputation_type" variable: 
- choose an imputation method for each series of consecutive years with missing data ("Consecutive", most convenient and suitable for most cases)
- choose an imputation method for each year with missing data ("Year-by-year")

``` R
# Aggregated imputation

source("./modules/processing_aggregated.R")

agg_imputation_type <- 1 # 1 = "Consecutive", 2 = "Year-by-year"

source("./modules/imputation_aggregated.R")
```

#### Subseries imputation

To run subseries imputation, execute the code below. Note that if you run the subseries imputation after the aggregated imputation, the results of the aggregated imputation will be replaced by those of the subseries imputation.

``` R
# Subseries imputation

source("./modules/imputation_subseries.R")
```

### Exporting the imputed data and imputation report

To export the imputed data (CSV) and the imputation report (HTML) in the outputs folder, run the following code block in the main.R script.

``` R
# Final data export and report generation

source("./modules/final_data_export_viz.R")
```

## Contents of the extracted folder

### Main folder
This folder includes the files that run the imputation tool:

| File            | Type           | Description                             |
|-----------------|----------------|-----------------------------------------|
| emputator.Rproj | R project file | Opens the tool in RStudio.              |
| main.R          | R script       | Main script from which the tool is run. |

### Inputs folder
The inputs folder contains the data necessary for the imputation tool:

| File           | Type        | Description                                                                                                             |
|----------------|-------------|-------------------------------------------------------------------------------------------------------------------------|
| FM_DB.rds      | R data file | Contains the shared FAO-OECD employment database on which to perform the imputation.                                    |
| ILO_labor.rds  | R data file | Contains the ILO labor force database to be used in linear models.                                                      |
| OECD_fleet.rds | R data file | Contains the OECD fleet database to be used in linear models.                                                           |
| PROD.rds       | R data file | Contains the FAO capture and aquaculture production database to be used in linear models and productivity computations. |

### Modules folder
The modules folder contains the R scripts that are necessary for the imputation tool to do its computations and produce the desired outputs.

| File                    | Type            | Description                                                                                                                                                                            |
|-------------------------|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| data_import.R           | R script        | Imports data from the inputs folder into the R environment.                                                                                                                            |
| final_data_export_viz.R | R script        | Creates the HTML imputation report and CSV file of the imputed data and saves them in the outputs folder.                                                                              |
| functions.R             | R script        | Includes all the functions that the tool uses to generate imputed data, charts, tables, etc.                                                                                          |
| imputation_aggregated.R | R script        | Runs the imputation in aggregated mode.                                                                                                                                                |
| imputation_subseries.R  | R script        | Runs the imputation by subseries.                                                                                                                                                      |
| inputs_update.R         | R script        | Updates the data located in the inputs folder: online databases from FAO, the OECD and ILO are queried to retrieve the latest production, fleet and labour force data.                 |
| processing_aggregated.R | R script        | Generates all the outputs necessary to run the imputation in aggregated mode: tables with imputed data and their associated "rainbow" bar charts.                                      |
| report.Rmd              | R Markdown file | Generates the HTML imputation report.                                                                                                                                                  |
| subseries_analysis.R    | R script        | Performs a series of basic computation from the data: visualizes the existing estimates, identifies years with missing data, computes the weight of each subseries for each year, etc. |

### Outputs folder
The outputs folder contains the imputation results by country and sector. For each country/sector processed by the tool, an HTML report and a CSV file of the imputed data are saved in this folder.
