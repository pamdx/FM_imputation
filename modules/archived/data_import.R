##### Non-reactive

country_names <- read_tsv("./inputs/country_names.txt")

# Import employment data (in final version, link to SWS or online repo)

# FM_raw <- read_tsv("./inputs/FM_DB.txt", col_types = cols(
#   geographic_area = col_character(),
#   OC1 = col_character(),
#   OC2 = col_character(),
#   working_time = col_character(),
#   gender = col_character(),
#   year = col_integer(),
#   value = col_integer(),
#   flag = col_character(),
#   comment = col_character()
# ))

FM_raw <- readRDS("./inputs/FM_DB.rds")

# Import production data (in final version, link to SWS or online repo)

if (!exists("prod_raw")) {
  
  prod_raw <- readRDS("./inputs/PROD.rds")

}

# Get ILO labor force data

if (!exists("ILO_labor_raw")) {
  ILO_labor_raw <- get_ilostat("EAP_2EAP_SEX_AGE_NB_A")
  
  target_labor_classif1 <- c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
  
}

##### Reactive

# Basic FM filter

FM_filtered <- FMfilter()

subseries <- getsubseries()

# Visualization of current estimates

FM_exisiting_estimates <- FMexisitingestimates()

data_viz(data = FM_exisiting_estimates %>%
           unite(subseries, c("OC2", "working_time", "gender"), sep = " | ") %>%
           mutate(estimate = case_when(flag == "B" |  flag == "F" |  flag == "P" |  flag == "E" |  flag == "L"  ~ value)), 
         country_input = country_input, OC1_input = OC1_input, title = "Visualization of current estimates")