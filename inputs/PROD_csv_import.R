temp <- tempfile()
download.file("http://www.fao.org/fishery/static/Data/GlobalProduction_2020.1.0.zip", temp)
data <- read_csv(unz(temp, "TS_FI_PRODUCTION.csv"), col_types = list(col_integer(), col_integer(), col_integer(), col_character(), col_integer(), col_character(), col_double(), col_character()))
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), col_types = list(col_integer(), col_integer(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))
areas <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv"), col_types = list(col_integer(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))

data <- data %>%
  left_join(countries %>% select(UN_Code, Name_En), by = c("COUNTRY" = "UN_Code"), keep = FALSE) %>%
  left_join(areas %>% select(Code, InlandMarine_Group), by = c("AREA" = "Code"), keep = FALSE) %>%
  filter(UNIT == "t", ) %>%
  mutate(oc1 = case_when(
    SOURCE == 1 | 2 | 3 | 5 ~ "Aquaculture",
    SOURCE == 4 ~ "Fishing"
  )) %>%
  mutate(oc2 = case_when(
    oc1 == "Aquaculture" ~ "Aquaculture",
    (oc1 == "Fishing" & InlandMarine_Group == "Marine") ~ "Marine fishing",
    (oc1 == "Fishing" & InlandMarine_Group == "Inland") ~ "Inland fishing"
  )) %>%
  group_by(Name_En, oc1, oc2, YEAR) %>%
  summarise(value = sum(QUANTITY)) %>%
  ungroup() %>%
  rename(country = Name_En, year = YEAR, prod_value = value)

unlink(temp)