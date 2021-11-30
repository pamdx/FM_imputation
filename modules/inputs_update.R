##### UPDATE DATA INPUTS #####

library(readr)
library(Rilostat)
library(OECD)
library(dplyr)

# Import country names mapping

country_names <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv") %>%
  select(Name_En, ISO3_Code) %>%
  rename(Country_en = Name_En, iso3 = ISO3_Code)

# Import employment data

FM_raw <- read_tsv("./inputs/FM_DB.txt", col_types = cols(
  geographic_area = col_character(),
  OC2 = col_character(),
  OC3 = col_character(),
  working_time = col_character(),
  sex = col_character(),
  year = col_integer(),
  value = col_integer(),
  flag = col_character(),
  comment = col_character()
))

saveRDS(FM_raw, "./inputs/FM_DB.RDS")

# Get FAO production data

temp <- tempfile()
download.file("http://www.fao.org/fishery/static/Data/GlobalProduction_2021.1.2.zip", temp)
data <- read_csv(unz(temp, "GLOBAL_PRODUCTION_QUANTITY.csv"))
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"))
areas <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv"))

prod_raw <- data %>%
  left_join(countries %>% select(UN_Code, Name_En), by = c("COUNTRY.UN_CODE" = "UN_Code"), keep = FALSE) %>%
  left_join(areas %>% select(Code, InlandMarine_Group_En), by = c("AREA.CODE" = "Code"), keep = FALSE) %>%
  filter(MEASURE == "Q_tlw", ) %>%
  mutate(OC1 = case_when(
    PRODUCTION_SOURCE_DET.CODE %in% c("FRESHWATER", "MARINE", "BRACKISHWATER") ~ "Aquaculture",
    PRODUCTION_SOURCE_DET.CODE == "CAPTURE" ~ "Fishing"
  )) %>%
  mutate(OC2 = case_when(
    OC1 == "Aquaculture" ~ "Aquaculture",
    (OC1 == "Fishing" & InlandMarine_Group_En == "Marine waters") ~ "Marine fishing",
    (OC1 == "Fishing" & InlandMarine_Group_En == "Inland waters") ~ "Inland fishing"
  )) %>%
  group_by(Name_En, OC1, OC2, PERIOD) %>%
  summarise(value = sum(VALUE)) %>%
  ungroup() %>%
  rename(country = Name_En, year = PERIOD, prod_value = value)

unlink(temp)

saveRDS(prod_raw, "./inputs/PROD.RDS")

# Get ILO labor force data

target_labor_classif1 <- c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")

ILO_labor_raw <- get_ilostat("EAP_2EAP_SEX_AGE_NB_A") %>%
  rename(iso3 = ref_area, year = time) %>%
  merge(country_names) %>%
  filter(classif1 %in% target_labor_classif1) %>% 
  filter(sex != "SEX_T") %>% 
  mutate(labor_value = obs_value * 1000)

if (nrow(ILO_labor_raw[is.na(ILO_labor_raw$Country_en),]) > 0) {
  print(ILO_labor_raw[is.na(ILO_labor_raw$Country_en),])
  stop("missing country mapping")
}

saveRDS(ILO_labor_raw, "./inputs/ILO_labor.RDS")

# Get OECD fleet data

OECD_fleet_raw <- OECD::get_dataset(dataset = "FISH_FLEET") %>%
  filter(FLEET == "TOT_VESSEL", MEASURE == "NUM", TIME_FORMAT == "P1Y", UNIT == "NBR") %>%
  rename(iso3 = COUNTRY, year = obsTime, fleet_value = obsValue) %>%
  left_join(country_names, by = "iso3") %>%
  select(Country_en, year, fleet_value)

if (nrow(OECD_fleet_raw[is.na(OECD_fleet_raw$Country_en),]) > 0) {
  print(OECD_fleet_raw[is.na(OECD_fleet_raw$Country_en),])
  stop("missing country mapping")
}

saveRDS(OECD_fleet_raw, "./inputs/OECD_fleet.RDS")
