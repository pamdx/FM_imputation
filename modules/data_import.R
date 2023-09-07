# Import employment data

FM_raw <- readRDS(paste0(input_directory, "FM_DB.rds"))

# Import production data

prod_raw <- readRDS(paste0(input_directory, "PROD.rds"))

# Get ILO labor force data

ILO_labor_raw <- readRDS(paste0(input_directory, "ILO_labor.RDS"))

# Get OECD fleet data

OECD_fleet_raw <- readRDS(paste0(input_directory, "OECD_fleet.RDS"))

OECD_countries <- unique(OECD_fleet_raw$Country_en)
