country_names <- read_tsv("./inputs/country_names.txt")

# Import employment data

FM_raw <- readRDS("./inputs/FM_DB.rds")

# FM_raw <- read_tsv("./inputs/FM_DB.txt", col_types = cols(
#   geographic_area = col_character(),
#   OC1 = col_character(),
#   OC2 = col_character(),
#   working_time = col_character(),
#   sex = col_character(),
#   year = col_integer(),
#   value = col_integer(),
#   flag = col_character(),
#   comment = col_character()
# ))

# Import production data

prod_raw <- readRDS("./inputs/PROD.rds")

# Get ILO labor force data

ILO_labor_raw <- get_ilostat("EAP_2EAP_SEX_AGE_NB_A")

target_labor_classif1 <- c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")

# Get OECD fleet data

OECD_fleet_raw <- OECD::get_dataset(dataset = "FISH_FLEET") %>%
  filter(FLEET == "TOT_VESSEL", MEASURE == "NUM", TIME_FORMAT == "P1Y", UNIT == "NBR") %>%
  rename(iso3 = COUNTRY, year = obsTime, value = obsValue) %>%
  left_join(country_names, by = "iso3")

OECD_countries <- unique(OECD_fleet_raw$Country_en)
