# General parameters

source(path_data_import) # Load data from inputs folder

country_input <- select_country(FM_raw, country_input)
OC2_input <- select_sector(FM_raw, country_input)
start_year <- 1995
end_year <- 2021 # MAKE SURE TO UPDATE