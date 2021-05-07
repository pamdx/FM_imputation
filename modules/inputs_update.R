##### UPDATE DATA INPUTS #####

library(readr)
library(Rilostat)
library(OECD)
library(dplyr)

# Import employment data

FM_raw <- read_tsv("./inputs/FM_DB.txt", col_types = cols(
  geographic_area = col_character(),
  OC1 = col_character(),
  OC2 = col_character(),
  working_time = col_character(),
  sex = col_character(),
  year = col_integer(),
  value = col_integer(),
  flag = col_character(),
  comment = col_character()
))

saveRDS(FM_raw, "./inputs/FM_DB.RDS")

# Get ILO labor force data

ILO_labor_raw <- get_ilostat("EAP_2EAP_SEX_AGE_NB_A")

saveRDS(ILO_labor_raw, "./inputs/ILO_labor.RDS")

# Get OECD fleet data

OECD_fleet_raw <- OECD::get_dataset(dataset = "FISH_FLEET") %>%
  filter(FLEET == "TOT_VESSEL", MEASURE == "NUM", TIME_FORMAT == "P1Y", UNIT == "NBR") %>%
  rename(iso3 = COUNTRY, year = obsTime, value = obsValue)

saveRDS(OECD_fleet_raw, "./inputs/OECD_fleet.RDS")