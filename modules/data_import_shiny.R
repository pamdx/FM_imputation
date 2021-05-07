country_names <- read_tsv("./inputs/country_names.txt")

# Import employment data

FM_raw <- readRDS("./inputs/FM_DB.rds")

# Import production data

prod_raw <- readRDS("./inputs/PROD.rds")

# Get ILO labor force data

ILO_labor_raw <- readRDS("./inputs/ILO_labor.RDS")

target_labor_classif1 <- c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")

# Get OECD fleet data

OECD_fleet_raw <- readRDS("./inputs/OECD_fleet.RDS") %>%
  left_join(country_names, by = "iso3") %>%
  select(Country_en, year, value)

OECD_countries <- unique(OECD_fleet_raw$Country_en)
