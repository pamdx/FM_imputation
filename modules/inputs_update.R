##### UPDATE DATA INPUTS #####

library(readr)
library(Rilostat)
library(OECD)
library(dplyr)

# Import country names mapping

country_names <- read_tsv("./inputs/country_names.txt")

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

# Get FAO production data

# temp <- tempfile()
# download.file("http://www.fao.org/fishery/static/Data/GlobalProduction_2020.1.0.zip", temp)
# data <- read_csv(unz(temp, "TS_FI_PRODUCTION.csv"), col_types = list(col_integer(), col_integer(), col_integer(), col_character(), col_integer(), col_character(), col_double(), col_character()))
# countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), col_types = list(col_integer(), col_integer(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))
# areas <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv"), col_types = list(col_integer(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))
# 
# prod_raw <- data %>%
#   left_join(countries %>% select(UN_Code, Name_En), by = c("COUNTRY" = "UN_Code"), keep = FALSE) %>%
#   left_join(areas %>% select(Code, InlandMarine_Group), by = c("AREA" = "Code"), keep = FALSE) %>%
#   filter(UNIT == "t", ) %>%
#   mutate(oc1 = case_when(
#     SOURCE == 1 | 2 | 3 | 5 ~ "Aquaculture",
#     SOURCE == 4 ~ "Fishing"
#   )) %>%
#   mutate(oc2 = case_when(
#     oc1 == "Aquaculture" ~ "Aquaculture",
#     (oc1 == "Fishing" & InlandMarine_Group == "Marine") ~ "Marine fishing",
#     (oc1 == "Fishing" & InlandMarine_Group == "Inland") ~ "Inland fishing"
#   )) %>%
#   group_by(Name_En, oc1, oc2, YEAR) %>%
#   summarise(value = sum(QUANTITY)) %>%
#   ungroup() %>%
#   rename(country = Name_En, year = YEAR, prod_value = value)
# 
# unlink(temp)
# 
# saveRDS(prod_raw, "./inputs/PROD.RDS")

# Get ILO labor force data

target_labor_classif1 <- c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")

ILO_labor_raw <- get_ilostat("EAP_2EAP_SEX_AGE_NB_A") %>%
  rename(iso3 = ref_area, year = time) %>%
  merge(country_names) %>%
  filter(classif1 %in% target_labor_classif1) %>% 
  filter(sex != "SEX_T") %>% 
  mutate(labor_value = obs_value * 1000)

saveRDS(ILO_labor_raw, "./inputs/ILO_labor.RDS")

# Get OECD fleet data

OECD_fleet_raw <- OECD::get_dataset(dataset = "FISH_FLEET") %>%
  filter(FLEET == "TOT_VESSEL", MEASURE == "NUM", TIME_FORMAT == "P1Y", UNIT == "NBR") %>%
  rename(iso3 = COUNTRY, year = obsTime, fleet_value = obsValue) %>%
  left_join(country_names, by = "iso3") %>%
  select(Country_en, year, fleet_value)

saveRDS(OECD_fleet_raw, "./inputs/OECD_fleet.RDS")