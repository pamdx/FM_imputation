# Import employment data

FM_raw <- readRDS("./inputs/FM_DB.rds")

# Import production data

prod_raw <- readRDS("./inputs/PROD.rds")

# Get ILO labor force data

ILO_labor_raw <- readRDS("./inputs/ILO_labor.RDS")

# Get OECD fleet data

OECD_fleet_raw <- readRDS("./inputs/OECD_fleet.RDS")

OECD_countries <- unique(OECD_fleet_raw$Country_en)
