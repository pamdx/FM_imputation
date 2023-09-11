### PARAMETERS ###

theme_set(theme_bw()) # set simple theme for charts
flags_official <- c(NA, "B", "I", "M", "P", "Q", "T", "R")
flag_FAOestimate <- "E"
flag_remove <- "X"

# Simple functions to select country and sector with popup windows

country_input <- ""

select_country <- function(FMraw, countryinput){
  
  select.list(unique(FMraw$geographic_area), preselect = countryinput, multiple = FALSE, graphics = TRUE, title = "Select the country to estimate.")
  
}


select_sector <- function(FMraw, countryinput){
  
  select.list(unique(sort(FMraw$OC2[FMraw$geographic_area == countryinput])), preselect = NULL, multiple = FALSE, graphics = TRUE, title = paste("Select the sector to estimate in", countryinput))
  
}


# Filter FM

FMfilter <- function(FMraw, countryinput, OC2input, startyear, endyear){
  
  FMraw %>%
    filter(geographic_area == countryinput) %>% # In Shiny, replace by input from dropdown menu
    filter(OC2 == OC2input) %>%
    filter(between(year, startyear, endyear)) %>%
    filter(!is.na(value) | !is.na(flag)) %>%
    filter(flag %in% flags_official) %>% # Make dynamic input in Shiny
    unite("subseries", geographic_area, OC2, OC3, working_time, sex, sep = "_", remove = FALSE) %>%
    select(geographic_area:comment, subseries)
  
}

# Get filtered data's subseries

getsubseries <- function(FMfiltered){
  
  pull(distinct(filter(FMfiltered), subseries), subseries)
  
}

# Get dataset of current estimates

FMexisitingestimates <- function(FMraw, countryinput, OC2input, startyear, endyear){
  
  FMraw %>%
    filter(geographic_area == countryinput) %>% # In Shiny, replace by input from dropdown menu
    filter(OC2 == OC2input) %>%
    filter(between(year, startyear, endyear)) %>%
    filter(is.na(flag) | flag != flag_remove) %>% # Make dynamic input in Shiny
    unite("subseries", geographic_area, OC2, OC3, working_time, sex, sep = "_", remove = FALSE) %>%
    select(geographic_area:comment, subseries)
  
}

# Function to generate visualizations of pre- and post-processing data

data_viz <- function(data, countryinput, OC2input, title){

data <- data  %>%
  mutate(OC3_short = case_when(
    OC3 == "Marine Coastal Fishing" ~ "Coastal",
    OC3 == "Marine Deep-Sea Fishing" ~ "Deep-Sea",
    OC3 == "Marine Fishing, nei" ~ "NEI",
    TRUE ~ OC3
  )) %>%
  mutate(subseries = ifelse(OC2 == "Marine fishing",
                            paste(OC3_short, working_time, sex, sep = " | "),
                            paste(working_time, sex, sep = " | ")
  )) %>%
  mutate(alpha = case_when(flag == flag_FAOestimate  ~ 0.35,
                           flag %in% flags_official  ~ 1))

  print(
    ggplot(data, aes(x = year, y = value, fill = subseries, alpha = alpha)) +
      geom_bar(stat="identity", colour="white") +
      labs(title = title, subtitle = paste(countryinput, "|", OC2_input), y = "Employment (people)", caption = "Solid bars indicate official data or alternative sources. Transparent bars indicate estimates.") +
      guides(alpha = "none") +
      scale_alpha_identity() +
      scale_fill_discrete(name = "Subseries") +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks(), minor_breaks = seq(start_year, end_year, 1)) +
      theme(aspect.ratio = 3/4, axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  )
  
}

# Identify aggregated Country/sector/year data where official and estimated data are mixed

mixedflags <- function(FMraw, countryinput, OC2input, startyear, endyear){
  
  FM_mixflags <- FMraw %>%
    filter(geographic_area == countryinput) %>%
    filter(OC2 == OC2input) %>%
    filter(year %in% c(startyear:endyear)) %>%
    group_by(geographic_area, OC2, year) %>%
    summarise(flags = list(unique(flag)))
  
  dropflag <- c()
  
  flagtest <- lapply(FM_mixflags$flags, function(i){
    unlist(i, use.names = FALSE)
  })
  
  dropflag <- sapply(flagtest, function(i){
    append(dropflag, ifelse(NA %in% i & flag_FAOestimate %in% i , "1", "0"))
  })
  
  FM_mixflags["dropflag"] <- dropflag
  FM_mixflags <- FM_mixflags %>%
    unite(conc, c(geographic_area, OC2, year), sep = "_") %>%
    filter(dropflag == "1")
  mixed_flags <- as.data.frame(FM_mixflags$conc)
  mixed_flags <- mixed_flags %>%
    separate("FM_mixflags$conc", c("country","OC2", "year"), sep = "_")
  
  return(mixed_flags)
  
}

# Identify missing years

yearsdata <- function(FMfiltered){
  
  c(unique(FMfiltered$year))
  
}

yearsdataexclmixed <- function(yearsdata, mixedflags){
  
  setdiff(yearsdata, mixedflags$year)
  
}

yearsall <- function(startyear, endyear){
  
  c(startyear:endyear)
  
}

missingyears <- function(yearsall, yearsdata){
  
  setdiff(yearsall, yearsdata)
  
}

missingyearsseries <- function(missingyears){
  
  split(missingyears, cumsum(c(1, diff(missingyears) != 1)))
  
}

missingyearsinclmixed <- function(yearsall, yearsdataexclmixed){
  
  setdiff(yearsall, yearsdataexclmixed)
  
}

missingyearsseriesinclmixed <- function(missingyearsinclmixed){
  
  split(missingyearsinclmixed, cumsum(c(1, diff(missingyearsinclmixed) != 1)))
  
}

# Identify years where the continuity of subseries is interrupted

subseriesbreak <- function(yearsdata, startyear, FMfiltered){
  
  subseries_break <- c()
  
  for (i in yearsdata) {
    
    if (i > startyear) {
      
      prev_subseries <- pull(distinct(filter(FMfiltered, year == i-1), subseries), subseries)
      cur_subseries <- pull(distinct(filter(FMfiltered, year == i), subseries), subseries)
      
      if (!identical(sort(prev_subseries), sort(cur_subseries))) {
        
        subseries_break <- c(subseries_break, i)
        
      }
    }
  }
  
  return(subseries_break)
  
}

# Summarize weight of each subseries for each year

subseriesweights <- function(FMfiltered, yearsdataexclmixed, missingyearsseriesinclmixed, yearsdata){
  
  subseries_weights <- inner_join(FMfiltered,
                                  FMfiltered %>%
                                    filter(year %in% yearsdataexclmixed) %>%
                                    group_by(year) %>%
                                    summarise(value = sum(value)) %>%
                                    rename(value_agg = value),
                                  by = "year") %>% 
    mutate(ratio = value/value_agg) %>%
    select(subseries, year, value, ratio)
  
  # Drag subseries and associated weights to missing years (0.04 sec)
  
  for(i in missingyearsseriesinclmixed){
    
    ## series at tail of data
    
    # Dragging forwards
    if(any(yearsdataexclmixed == (min(i)-1)) & !any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdataexclmixed == min(i)-1)]
      ref_subseries <- pull(distinct(filter(subseries_weights, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights <- subseries_weights %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights$ratio[(subseries_weights$subseries == j & subseries_weights$year == ref_year)]))
        
      }
      
    }
    
    ## series at head of data
    
    # Dragging backwards
    if(any(yearsdataexclmixed == (max(i)+1)) & !any(yearsdataexclmixed == (min(i)-1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdata == max(i)+1)]
      ref_subseries <- pull(distinct(filter(subseries_weights, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights <- subseries_weights %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights$ratio[(subseries_weights$subseries == j & subseries_weights$year == ref_year)]))
        
      }
      
    }
    
    ## series in between data
    
    # Dragging backwards
    if(any(yearsdataexclmixed == (min(i)-1)) & any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdataexclmixed == max(i)+1)]
      ref_subseries <- pull(distinct(filter(subseries_weights, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights <- subseries_weights %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights$ratio[(subseries_weights$subseries == j & subseries_weights$year == ref_year)]))
        
      }
      
    }
  }
  
  return(subseries_weights)
  
}

# Summarize weight of each subseries for each year (with interpolation when possible)

subseriesweightsinterpolated <- function(FMfiltered, yearsdataexclmixed, missingyearsseriesinclmixed, yearsdata){
  
  subseries_weights_interpolated <- inner_join(FMfiltered,
                                  FMfiltered %>%
                                    filter(year %in% yearsdataexclmixed) %>%
                                    group_by(year) %>%
                                    summarise(value = sum(value)) %>%
                                    rename(value_agg = value),
                                  by = "year") %>% 
    mutate(ratio = value/value_agg) %>%
    select(subseries, year, value, ratio)
  
  # Drag subseries and associated weights to missing years
  
  for(i in missingyearsseriesinclmixed){
    
    ## series at tail of data
    
    # Dragging forwards
    if(any(yearsdataexclmixed == (min(i)-1)) & !any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdataexclmixed == min(i)-1)]
      ref_subseries <- pull(distinct(filter(subseries_weights_interpolated, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights_interpolated <- subseries_weights_interpolated %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights_interpolated$ratio[(subseries_weights_interpolated$subseries == j & subseries_weights_interpolated$year == ref_year)]))
        
      }
      
    }
    
    ## series at head of data
    
    # Dragging backwards
    if(any(yearsdataexclmixed == (max(i)+1)) & !any(yearsdataexclmixed == (min(i)-1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdata == max(i)+1)]
      ref_subseries <- pull(distinct(filter(subseries_weights_interpolated, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights_interpolated <- subseries_weights_interpolated %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights_interpolated$ratio[(subseries_weights_interpolated$subseries == j & subseries_weights_interpolated$year == ref_year)]))
        
      }
      
    }
    
    ## series in between data
    
    # Dragging backwards
    if(any(yearsdataexclmixed == (min(i)-1)) & any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year_before <- yearsdataexclmixed[which(yearsdataexclmixed == min(i)-1)]
      ref_subseries_before <- pull(distinct(filter(subseries_weights_interpolated, year == ref_year_before), subseries), subseries)
      
      ref_year_after <- yearsdataexclmixed[which(yearsdataexclmixed == max(i)+1)]
      ref_subseries_after <- pull(distinct(filter(subseries_weights_interpolated, year == ref_year_after), subseries), subseries)
      
      # Linear interpolation of weights when the subseries before and after are identical
      
      if (identical(sort(ref_subseries_before), sort(ref_subseries_after))){
        
        for(j in i){
          
          for(h in ref_subseries_after){
            
            subseries_weights_interpolated <- subseries_weights_interpolated %>% 
              bind_rows(tibble(subseries = h, year = j, value = NA, ratio = (subseries_weights_interpolated$ratio[(subseries_weights_interpolated$subseries == h & subseries_weights_interpolated$year == ref_year_before)] * (ref_year_after - j) + subseries_weights_interpolated$ratio[(subseries_weights_interpolated$subseries == h & subseries_weights_interpolated$year == ref_year_after)] * (j - ref_year_before))/(ref_year_after - ref_year_before)))
            
          }
          
        }
        
      } else
        
        # When subseries before and after aren't identical, just drag the weights backwards
      
        for(j in ref_subseries_after){
          
          subseries_weights_interpolated <- subseries_weights_interpolated %>% 
            bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights_interpolated$ratio[(subseries_weights_interpolated$subseries == j & subseries_weights_interpolated$year == ref_year_after)]))
          
        }
    }
  }
  
  return(subseries_weights_interpolated)
  
}

# Drag subseries and associated weights to missing years (subseries_weights_forward)

subseriesweightsforward <- function(FMfiltered, yearsdataexclmixed, missingyearsseriesinclmixed){
  
  subseries_weights_forward <- inner_join(FMfiltered,
                                          FMfiltered %>%
                                            filter(year %in% yearsdataexclmixed) %>%
                                            group_by(year) %>%
                                            summarise(value = sum(value)) %>%
                                            rename(value_agg = value),
                                          by = "year") %>% 
    mutate(ratio = value/value_agg) %>%
    select(subseries, year, value, ratio)
  
  for(i in missingyearsseriesinclmixed){
    
    ## series at tail of data
    
    # Dragging forwards
    if(any(yearsdataexclmixed == (min(i)-1)) & !any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdataexclmixed == min(i)-1)]
      ref_subseries <- pull(distinct(filter(subseries_weights_forward, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights_forward <- subseries_weights_forward %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights_forward$ratio[(subseries_weights_forward$subseries == j & subseries_weights_forward$year == ref_year)]))
        
      }
      
    }
    
    ## series in between data
    
    # Dragging forwards
    if(any(yearsdataexclmixed == (min(i)-1)) & any(yearsdataexclmixed == (max(i)+1))){
      
      ref_year <- yearsdataexclmixed[which(yearsdataexclmixed == min(i)-1)]
      ref_subseries <- pull(distinct(filter(subseries_weights_forward, year == ref_year), subseries), subseries)
      
      for(j in ref_subseries){
        
        subseries_weights_forward <- subseries_weights_forward %>% 
          bind_rows(tibble(subseries = j, year = i, value = NA, ratio = subseries_weights_forward$ratio[(subseries_weights_forward$subseries == j & subseries_weights_forward$year == ref_year)]))
        
      }
      
    }
  }
  
  return(subseries_weights_forward)
  
}

# Inspect discontinuities in subseries

discontinuoussubseries <- function(ss, FMfiltered){
  
  discontinuous_subseries <- c()
  
  for (i in ss) {
    
    period_covered <- FMfiltered$year[(FMfiltered$subseries == i)]
    
    if (!(all(abs(diff(period_covered)) == 1))) {
      
      discontinuous_subseries <- append(discontinuous_subseries, i)
      
    }
    
  }
  
  return(discontinuous_subseries)
  
}

# Generate wide table showing data by subseries

subseriestable <- function(FMfiltered, ss, yearsall){
  
  FMfiltered %>% 
    select(subseries, year, value) %>%
    right_join(FMfiltered %>% expand(subseries, yearsall) %>% rename(year = yearsall)) %>%
    spread(year, value) %>%
    separate(subseries, into = c("geographic_area", "OC2", "OC3", "working_time", "sex"), sep = "_") %>%
    unite(subseries, c("OC3", "working_time", "sex"), sep = " | ") %>%
    select(subseries:last_col())
  
}

# Aggregate FM data for reg/trend estimates

FMagg <- function(FMfiltered, yearsdataexclmixed){
  
  FMfiltered %>%
    filter(year %in% yearsdataexclmixed) %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    select(year, value)
  
}

# Function to aggregate production data

prodagg <- function(prodraw, countryinput, OC2input, startyear, endyear){
  
  prodraw %>%
    filter(country == countryinput) %>%
    filter(OC2 == OC2input) %>%
    filter(between(year, startyear, endyear)) %>%
    group_by(year) %>%
    summarise(prod_value = sum(prod_value))
}

# Function to generate table of productivities

productivity_table <- function(emp, prod){
  
  left_join(emp, prod, by = "year") %>%
    mutate(productivity = prod_value / emp_value)
  
}

# Function to aggregate ILO labor data

ILOlabor_agg <- function(ILOlaborraw, countryinput, startyear, endyear){
  
  ILO_labor_agg <- ILOlaborraw %>%
    filter(Country_en == countryinput) %>%
    filter(between(year, startyear, endyear)) %>% 
    group_by(year) %>%
    summarise(labor_value = sum(labor_value))
  
  ILO_labor_agg$year <- as.integer(ILO_labor_agg$year) 
  
  return(ILO_labor_agg)
  
}

# Function to aggregate OECD fleet data

OECDfleet_agg <- function(OECDfleetraw, countryinput, startyear, endyear){
  
  OECD_fleet_agg <- OECDfleetraw %>%
    filter(Country_en == countryinput, between(year, startyear, endyear))

  OECD_fleet_agg$year <- as.integer(OECD_fleet_agg$year)
  
  return(OECD_fleet_agg)

}

# Generate dataset for regression

regression_data <- function(yearsall, FMagg, ILOlaboragg, prodagg){
  
  return(data.frame(yearsall) %>%
           rename(year = yearsall) %>%
           left_join(FMagg %>% rename(emp_value = value), by = "year") %>%
           left_join(ILOlaboragg, by = "year") %>%
           left_join(prodagg, by = "year"))
  
}

regression_data_fleet <- function(yearsall, FMagg, ILOlaboragg, prodagg, OECDfleetagg){
  
  return(data.frame(yearsall) %>%
           rename(year = yearsall) %>%
           left_join(FMagg %>% rename(emp_value = value), by = "year") %>%
           left_join(ILOlaboragg, by = "year") %>%
           left_join(prodagg, by = "year") %>%
           left_join(OECDfleetagg, by = "year"))
  
}

# Plots of linear regression variables

reg_variables_viz <- function(regdata, startyear, endyear, OC2input, countryinput){
  
  plot_emp <- ggplot(regdata, aes(x=year,y=emp_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title = paste(OC2input, " employment (FAO data)"), 
         subtitle = countryinput, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  plot_prod <- ggplot(regdata, aes(x=year,y=prod_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title = paste(OC2input, " production (FAO data)"), 
         subtitle = countryinput, 
         y="Tonnes") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  plot_labor <- ggplot(regdata, aes(x=year, y=labor_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title ="Labor force (ILO data)", 
         subtitle = countryinput, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  return(grid.arrange(plot_emp, plot_prod, plot_labor, ncol=2))
  
}

reg_variables_viz_fleet <- function(regdata, startyear, endyear, OC2input, countryinput){
  
  plot_emp <- ggplot(regdata, aes(x=year,y=emp_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title = paste(OC2input, " employment (FAO data)"), 
         subtitle = countryinput, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  plot_prod <- ggplot(regdata, aes(x=year,y=prod_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title = paste(OC2input, " production (FAO data)"), 
         subtitle = countryinput, 
         y="Tonnes") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  plot_labor <- ggplot(regdata, aes(x=year, y=labor_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title ="Labor force (ILO data)", 
         subtitle = countryinput, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  plot_fleet <- ggplot(regdata, aes(x=year, y=fleet_value, group = 1)) + 
    geom_line() + 
    geom_point(alpha = 0.5) + 
    scale_x_continuous(breaks = integer_breaks()) +
    labs(title ="Fleet (OECD data)", 
         subtitle = countryinput, 
         y="Number of vessels") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  
  return(grid.arrange(plot_emp, plot_prod, plot_labor, plot_fleet, ncol=2))
  
}

# Regression with automatic model choice

reg_automatic <- function(regdata, startyear, endyear){
  
  reg_data_ts <- ts(regdata, start = startyear)
  
  trend <- seq(startyear:endyear)
  
  results_list <- vector(mode = "list", length = 4)
  
  if (length(na.omit(regdata$prod_value)) > 0 & length(na.omit(regdata$labor_value)) > 0) {
   
    try(fit_emp_1 <- lm(
      emp_value ~ trend + prod_value + labor_value,
      data = reg_data_ts), silent = TRUE)
    
    results_list[[1]] <- fit_emp_1
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0 & length(na.omit(regdata$labor_value)) > 0) {
   
    try(fit_emp_2 <- lm(
      emp_value ~ prod_value + labor_value,
      data = reg_data_ts), silent = TRUE)
    
    results_list[[2]] <- fit_emp_2
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0) {
   
    try(fit_emp_3 <- lm(
      emp_value ~ trend + prod_value,
      data = reg_data_ts), silent = TRUE)
    
    results_list[[3]] <- fit_emp_3
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0) {
   
    try(fit_emp_4 <- lm(
      emp_value ~ prod_value,
      data = reg_data_ts), silent = TRUE) 
    
    results_list[[4]] <- fit_emp_4
    
  }
  
  return(results_list[lengths(results_list) != 0])
  
}

reg_automatic_fleet <- function(regdata, startyear, endyear){
  
  reg_data_ts <- ts(regdata, start = startyear)
  
  trend <- seq(startyear:endyear)
  
  results_list <- vector(mode = "list", length = 4)
  
  if (length(na.omit(regdata$prod_value)) > 0 & length(na.omit(regdata$labor_value)) > 0 & length(na.omit(regdata$fleet_value)) > 0) {
    
    try(fit_emp_1 <- lm(
      emp_value ~ trend + prod_value + labor_value + fleet_value,
      data = reg_data_ts), silent = TRUE)
    
    results_list[[1]] <- fit_emp_1
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0 & length(na.omit(regdata$labor_value)) > 0 & length(na.omit(regdata$fleet_value)) > 0) {
   
    try(fit_emp_2 <- lm(
      emp_value ~ prod_value + labor_value + fleet_value,
      data = reg_data_ts), silent = TRUE)
     
    results_list[[2]] <- fit_emp_2
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0) {
   
    try(fit_emp_3 <- lm(
      emp_value ~ trend + prod_value,
      data = reg_data_ts), silent = TRUE)
     
    results_list[[3]] <- fit_emp_3
    
  }
  
  if (length(na.omit(regdata$prod_value)) > 0) {
   
    try(fit_emp_4 <- lm(
      emp_value ~ prod_value,
      data = reg_data_ts), silent = TRUE)
     
    results_list[[4]] <- fit_emp_4
    
  }
  
  return(results_list[lengths(results_list) != 0])
  
}

# Regression with manual model choice

reg_manual <- function(regdata, startyear, manual_model){
  
  reg_data_ts <- ts(regdata, start = startyear)
  
  fit_emp_custom <- lm(
    manual_model,
    data = reg_data_ts)
  
  return(list(fit_emp_custom))
  
}

# Summary of regressions

reg_result_summary <- function(regresult){
  
  stargazer(regresult, title = "Results of linear models", align = TRUE, type = "text") 
  
}

# Find model with best fit

reg_auto_best_fit <- function(regresult){
  
  rsquared <- c()
  
  for (i in regresult) {
    
    rsquared <- c(rsquared, summary(i)$adj.r.squared)
    
  }
  
  return(paste(which.max(rsquared)))
  
}

# Get adj-R2 for model with best fit

reg_auto_best_R2 <- function(regresult, bestfit){
  
  round(summary(regresult[[as.integer(bestfit)]])$adj.r.squared, digits = 3)
  
}

# Get predictions from model with best fit

predict_reg <- function(regresult, bestfit, regdata, startyear){
  
  predictions <- predict(regresult[[as.integer(bestfit)]], ts(regdata, start = startyear))
  
  names(predictions) <- "predictions"
  
  return(predictions)
}

# Visualize best LM model over data

reg_fit_viz <- function(regdata, bestfit, startyear, endyear, countryinput, OC2input, best_R2){
  
  print(
    ggplot(regdata, aes(x = year, group = 1)) +
      geom_point(aes(y = emp_value, col = "Original data")) +
      geom_line(aes(y = predictions, col = paste0("LM", bestfit, (" (best fit)")))) +
      labs(title = paste(countryinput, ", ", OC2input, " employment", sep = ""), subtitle = "Original data vs. predicted values from linear regression", caption = paste("Adjusted R-squared: ", best_R2)) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      scale_color_discrete(name = "Legend") +
      theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  ) 
  
}

reg_fit_viz_manual <- function(regdata, startyear, endyear, countryinput, OC2input, best_R2){
  
  print(
    ggplot(regdata, aes(x = year, group = 1)) +
      geom_point(aes(y = emp_value, col = "Original data")) +
      geom_line(aes(y = predictions, col = "User-defined LM")) +
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(countryinput, ", ", OC2input, " employment", sep = ""), subtitle = "Original data vs. predicted values from linear regression", caption = paste("Adjusted R-squared: ", best_R2)) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      scale_color_discrete(name = "Legend") +
      theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  ) 
  
}

# Save predictions as dataframe

reg_estimator <- function(regdata){
  
  return(regdata %>%
           filter(is.na(emp_value)) %>%
           select(year, predictions) %>%
           rename(value = predictions) %>%
           filter(value >= 0))
  
}

# Function generating the predictions from the best polynomial fit

trend_fit <- function(FMagg, yearsdataexclmixed, yearsall, startyear){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
    
  trend_data <- tibble(year = yearsall) %>%
    left_join(FMagg, by = "year")
  
  trend_data_agg_ts <- ts(trend_data, start = startyear)
  
  #fit first degree polynomial equation:
  fit_trend_agg_1 <- lm(trend_data$value ~ trend_data$year)
  #second degree
  fit_trend_agg_2 <- lm(trend_data$value ~ poly(trend_data$year, 2, raw=TRUE))
  #third degree
  fit_trend_agg_3 <- lm(trend_data$value ~ poly(trend_data$year, 3, raw=TRUE))
  #fourth degree
  fit_trend_agg_4 <- lm(trend_data$value ~ poly(trend_data$year, 4, raw=TRUE))
  
  trend1_agg_r2 <- round(summary(fit_trend_agg_1)$adj.r.squared, digits = 3)
  trend2_agg_r2 <- round(summary(fit_trend_agg_2)$adj.r.squared, digits = 3)
  trend3_agg_r2 <- round(summary(fit_trend_agg_3)$adj.r.squared, digits = 3)
  trend4_agg_r2 <- round(summary(fit_trend_agg_4)$adj.r.squared, digits = 3)
  
  best_fit_trend_agg <- c("1", "2", "3", "4")[which.max(c(trend1_agg_r2, trend2_agg_r2, trend3_agg_r2, trend4_agg_r2))]
  
  best_fit_trend_R2_agg <- get(paste("trend",best_fit_trend_agg, "_agg_r2", sep = ""))
  
  switch(best_fit_trend_agg,
         
         '1' = best_fit_type_agg <- "Linear trend",
         
         '2' = best_fit_type_agg <- "Quadratic trend",
         
         '3' = best_fit_type_agg <- "Cubic trend",
         
         '4' = best_fit_type_agg <- "Quartic trend",
         
  )
  
  trend_data$predictions <- predict(get(paste("fit", "trend", "agg", best_fit_trend_agg, sep = "_")), trend_data_agg_ts)
  
  trend_data$r2adj <- best_fit_trend_R2_agg
  
  trend_data$trend_type <- best_fit_type_agg
  
  return(trend_data)
  
}

# Function generating a plot representing the polynomial regression with the best fit to the data

trend_fit_viz <- function(trenddata, startyear, endyear, countryinput, OC2input){
  
  print(
    ggplot(trenddata, aes(x = year, group = 1)) +
      geom_point(aes(y = value, col = "Original data")) +
      geom_line(aes(y = predictions, col = paste(trend_type[1], "(best fit)"))) +
      scale_x_continuous(breaks = integer_breaks()) +
      coord_cartesian(ylim=c(min(trenddata$value), max(trenddata$value))) +
      labs(title = paste(countryinput, ", ", OC2input, " employment", sep = ""), subtitle = "Original data vs. predicted values from polynomial regression", caption = paste("Adjusted R-squared: ", trenddata$r2adj[1])) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      scale_color_discrete(name = "Legend") +
      theme(aspect.ratio = 3/4, axis.title.x = element_blank())
  ) 
  
}

# Function generating standardized estimates for trend

trend_estimator <- function(trenddata){
  
  trend_estimates <- trenddata %>%
    filter(is.na(value)) %>%
    select(year, predictions) %>%
    rename(value = predictions) %>%
    filter(value >= 0)
  
  trend_estimates$flag <- flag_FAOestimate
  trend_estimates$comment <- paste0("Polynomial regression estimate (", trenddata$trend_type[1], ")")
  
  return(trend_estimates)
  
}

# Function to generate forward dragged estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

fdragged_estimator <- function(FMagg, yearsdataexclmixed, yearsall){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
  
  ref_years <- pull(distinct(FMagg, year))
  missing_years <- setdiff(yearsall, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  fdragged_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (min(i)-1))){
      
      for (j in i) {
        
        fdragged_estimates$value[(fdragged_estimates$year == j)] <- FMagg$value[(FMagg$year == ref_years[which(ref_years == min(i)-1)])]
        
      }
      
    }
    
  }
  
  fdragged_estimates <- fdragged_estimates %>%
    filter(!is.na(value))
  
  if (nrow(fdragged_estimates) > 0) {
  
    fdragged_estimates$flag <- flag_FAOestimate
    fdragged_estimates$comment <- "Forward dragged estimate"
    
  }
    
  return(fdragged_estimates)
  
}


# Function to generate backward dragged estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

bdragged_estimator <- function(FMagg, yearsdataexclmixed, yearsall){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
  
  ref_years <- pull(distinct(FMagg, year))
  missing_years <- setdiff(yearsall, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  bdragged_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (max(i)+1))){
      
      for (j in i) {
        
        bdragged_estimates$value[(bdragged_estimates$year == j)] <- FMagg$value[(FMagg$year == ref_years[which(ref_years == max(i)+1)])] 
        
      }
      
    }
    
  }
  
  bdragged_estimates <- bdragged_estimates %>%
    filter(!is.na(value))
  
  if (nrow(bdragged_estimates) > 0) {
  
    bdragged_estimates$flag <- flag_FAOestimate
    bdragged_estimates$comment <- "Backward dragged estimate"
  
  }
    
  return(bdragged_estimates)
  
}


# Function to generate linearly interpolated estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

linearint_estimator <- function(FMagg, yearsdataexclmixed, yearsall){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
  
  ref_years <- pull(distinct(FMagg, year))
  missing_years <- setdiff(yearsall, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  linearint_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (min(i)-1)) & any(ref_years == (max(i)+1))){
      
      ref_year_before <- ref_years[which(ref_years == min(i)-1)]
      ref_value_before <- FMagg$value[(FMagg$year == ref_year_before)]
      
      ref_year_after <- ref_years[which(ref_years == max(i)+1)]
      ref_value_after <- FMagg$value[(FMagg$year == ref_year_after)]
      
      # for each year in between, calculate linear interpolation, then split in ratios based on subseries
      
      for (j in i) {
        
        linearint_estimates$value[(linearint_estimates$year == j)] <- (ref_value_before*(ref_year_after-j)+ref_value_after*(j-ref_year_before))/(ref_year_after-ref_year_before)
        
      }
    }
  }
  
  linearint_estimates <- linearint_estimates %>%
    filter(!is.na(value))
  
  if (nrow(linearint_estimates) > 0) {
  
    linearint_estimates$flag <- flag_FAOestimate
    linearint_estimates$comment <- "Linear interpolation estimate"
  
  }
  
  return(linearint_estimates)
  
}

# Function to generate historical average estimates

histavg_estimator <- function(FMagg, yearsdataexclmixed, yearsall, threshold){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
  
  histavg_estimates <- left_join(data.frame(yearsall) %>% rename(year = yearsall), FMagg, by = "year") %>%
    mutate(histavg = NA)
  
  ref_years <- pull(distinct(FMagg, year))
  missing_years <- setdiff(yearsall, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  for (j in ref_years) {
    
    if (all(seq(j-(threshold-1),j) %in% ref_years)) {
      
      local_avg <- mean(
        filter(histavg_estimates, year %in% seq(j-(threshold-1),j))$value)
      histavg_estimates$histavg[(histavg_estimates$year == j)] <- local_avg
      
    }
    
  }
  
  for (k in missing_years_series) {
    
    if (min(ref_years) < min(k)) {
      
      ref_year <- ref_years[which(ref_years == min(k)-1)]
      
      for (l in k) {
        
        histavg_estimates$histavg[(histavg_estimates$year == l)] <- histavg_estimates$histavg[(histavg_estimates$year == ref_year)]
        
      }
      
    }
    
  }
  
  histavg_estimates <- histavg_estimates %>%
    filter(is.na(value) & !is.na(histavg)) %>%
    select(year, histavg) %>%
    rename(value = histavg)
  
  if (nrow(histavg_estimates) > 0) {
   
    histavg_estimates$flag <- flag_FAOestimate
    histavg_estimates$comment <- "Historical average estimate"
     
  }
  
  return(histavg_estimates)
  
}

# Function to generate historical growth estimates

histgrowth_estimator <- function(FMagg, yearsdataexclmixed, yearsall, threshold){
  
  FMagg <- FMagg %>% 
    filter(year %in% yearsdataexclmixed)
  
  histgrowth_estimates <- left_join(data.frame(yearsall) %>% rename(year = yearsall), FMagg, by = "year") %>%
    mutate(histgrowth = NA)
  
  ref_years <- pull(distinct(FMagg, year))
  missing_years <- setdiff(yearsall, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  for (j in ref_years) {
    
    if (all(seq(j-(threshold-1),j) %in% ref_years)) {
      
      value_start <- histgrowth_estimates$value[(histgrowth_estimates$year == j-(threshold-1))]
      value_end <- histgrowth_estimates$value[(histgrowth_estimates$year == j)]
      local_growth <- ((value_end/value_start)^(1/threshold))-1
      histgrowth_estimates$histgrowth[(histgrowth_estimates$year == j)] <- local_growth
      
    }
    
  }
  
  for (k in missing_years_series) {
    
    if (min(ref_years) < min(k)) {
      
      ref_year <- ref_years[which(ref_years == min(k)-1)]
      
      for (l in k) {
        
        histgrowth_estimates$histgrowth[(histgrowth_estimates$year == l)] <- histgrowth_estimates$value[(histgrowth_estimates$year == ref_year)] * (1 + histgrowth_estimates$histgrowth[(histgrowth_estimates$year == ref_year)])^(l-ref_year)
        
      } 
      
    }
    
  }
  
  histgrowth_estimates <- histgrowth_estimates %>%
    filter(is.na(value) & !is.na(histgrowth)) %>%
    select(year, histgrowth) %>%
    rename(value = histgrowth)
  
  if (nrow(histgrowth_estimates) > 0) {
  
    histgrowth_estimates$flag <- flag_FAOestimate
    histgrowth_estimates$comment <- "Historical growth estimate"

  }
      
  return(histgrowth_estimates)
  
}

# Function to disaggregate estimates

estimates_disaggregation <- function(weights, mflags, estimatesagg, countryinput, OC2input, FMfiltered, missingyears, comment){
  
  estimates_disag <- inner_join(weights[!(weights$year %in% mflags$year),], estimatesagg, by = "year") %>%
    mutate(value = ratio*value.y) %>%
    select(subseries, year, value) %>%
    # rbind(t(sapply(mflags$year, function(i){
    #   c("subseries" = paste(countryinput, OC2input, "Unspecified_Status Unspecified_U", sep = "_"), "year" = i, "value" = estimatesagg$value[estimatesagg$year == i] - FMfiltered$value[FMfiltered$year == i])
    # }))) %>%
    arrange(year) %>%
    filter(year %in% missingyears)
  estimates_disag$flag <- flag_FAOestimate
  estimates_disag$value <- as.integer(round(as.numeric(estimates_disag$value)))
  estimates_disag$year <- as.integer(estimates_disag$year)
  estimates_disag$comment <- comment
  
  return(estimates_disag)
  
}

# Function aggregating all estimates in a table

estimates_table <- function(FMfiltered, reg_estimates, trend_estimates, linearint_estimates, histavg_estimates, histgrowth_estimates, bdragged_estimates, fdragged_estimates){
  
  final_estimates <- FMfiltered %>% add_column(type = "data") %>% select(subseries, year, value, type) %>%
    bind_rows(reg_estimates %>% add_column(type = "reg") %>% select(subseries, year, value, type)) %>%
    bind_rows(trend_estimates %>% add_column(type = "trend") %>% select(subseries, year, value, type)) %>%
    bind_rows(linearint_estimates %>% add_column(type = "linearint") %>% select(subseries, year, value, type)) %>%
    bind_rows(histavg_estimates %>% add_column(type = "histavg") %>% select(subseries, year, value, type)) %>%
    bind_rows(histgrowth_estimates %>% add_column(type = "histgrowth") %>% select(subseries, year, value, type)) %>%
    bind_rows(bdragged_estimates %>% add_column(type = "bdragged") %>% select(subseries, year, value, type)) %>%
    bind_rows(fdragged_estimates %>% add_column(type = "fdragged") %>% select(subseries, year, value, type)) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    arrange(year, subseries) %>%
    separate(subseries, c("geographic_area", "oc2", "oc3", "working_time", "sex"), "_")
  
}

# Function to generate visualizations of estimates

estimator_viz <- function(method, estimator, dataset, countryinput, OC2input){
  
  if (nrow(get(paste0(method, "_estimates_disag"))) > 0) {
    print(
      
      dataset %>%
        mutate(oc3_short = case_when(
          oc3 == "Marine Coastal Fishing" ~ "Coastal",
          oc3 == "Marine Deep-Sea Fishing" ~ "Deep-Sea",
          oc3 == "Marine Fishing, nei" ~ "NEI",
          TRUE ~ oc3)) %>%
        mutate(subseries = ifelse(oc2 == "Marine fishing",
                                  paste(oc3_short, working_time, sex, sep = " | "),
                                  paste(working_time, sex, sep = " | "))) %>%
        mutate(value = case_when(
          is.na(data) ~ get(method),
          !is.na(data) ~ data),
              alpha = case_when(
                is.na(data) ~ 0.35,
                !is.na(data) ~ 1)) %>%
        
      ggplot(aes(x = year, y = value, fill = subseries, alpha = alpha)) +
        geom_bar(aes(y = value, fill = subseries), stat="identity", colour="white") + 
        labs(title = paste("Visualization of official data and", estimator, "estimator"), subtitle = paste(countryinput, "|", OC2input), y = "Employment (people)", caption = "Solid bars indicate official data or alternative sources. Transparent bars indicate estimates.") +
        guides(alpha = "none") +
        scale_alpha_identity() +
        scale_fill_discrete(name = "Subseries") +
        scale_y_continuous(labels = addUnits) + 
        scale_x_continuous(breaks = integer_breaks(), minor_breaks = seq(start_year, end_year, 1)) +
        theme(aspect.ratio = 3/4, axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
    )
  }
  
}

# Initialize imputed data table

imputed_data_init <- function(data){
  
  imputed_data <- data %>%
    select(subseries, year, value, flag, comment) %>%
    mutate(timestamp = as.character(NA))
  
}

# Function to condense lists of years

findIntRuns <- function(run){
  rundiff <- c(1, diff(run))
  difflist <- split(run, cumsum(rundiff!=1))
  unname(sapply(difflist, function(x){
    if(length(x) == 1) as.character(x) else paste0(x[1], "-", x[length(x)])
  }))
}

# Aggregated imputation

aggregated_imputation <- function(finalestimates, missingyears, imputeddata, regestimatesdisag, trendestimatesdisag, linearintestimatesdisag, histavgestimatesdisag, histgrowthestimatesdisag, bdraggedestimatesdisag, fdraggedestimatesdisag, FMexisitingestimates){
  
  # Get list of available estimators per year
  
  estimators_year <- finalestimates %>%
    unite(subseries, oc3, working_time, sex, sep = " | ", remove = TRUE) %>%
    select(-c(geographic_area, oc2)) %>% 
    pivot_longer(-c(subseries, year), names_to = "type", values_to = "value") %>%
    filter(!is.na(value), type != "data") %>%
    mutate(
      estimator = case_when(
        type == "reg" ~ "Linear regression",
        type == "trend" ~ "Polynomial regression",
        type == "linearint" ~ "Linear interpolation",
        type == "histavg" ~ "Historical average",
        type == "histgrowth" ~ "Historical growth",
        type == "bdragged" ~ "Backward dragged",
        type == "fdragged" ~ "Forward dragged"
      )) %>%
    select(year, estimator) %>%
    bind_rows(FMexisitingestimates %>%
                filter(flag == flag_FAOestimate) %>%
                select(year) %>%
                mutate(estimator = "[Keep existing estimates]"))
  
  estimators_order <- c("Linear regression", "Polynomial regression", "Linear interpolation", "Historical average", "Historical growth", "Backward dragged", "Forward dragged")
  
  for (i in missingyears) {
    
    estimators <- unique(
      estimators_year %>% 
        filter(year %in% i) %>% 
        pull(estimator)
      )
    
    selected_estimator <- select.list(c(estimators[order(match(estimators, estimators_order))]), preselect = NULL, multiple = FALSE, graphics = TRUE, title=paste0("Estimator for ", paste(findIntRuns(i), sep="", collapse=", "), "?"))
    
    switch(selected_estimator,
           
           "Linear regression" = imputeddata <- imputeddata %>% 
             bind_rows(regestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Polynomial regression" = imputeddata <- imputeddata %>% 
             bind_rows(trendestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Linear interpolation" = imputeddata <- imputeddata %>% 
             bind_rows(linearintestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Historical average" = imputeddata <- imputeddata %>% 
             bind_rows(histavgestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Historical growth" = imputeddata <- imputeddata %>% 
             bind_rows(histgrowthestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Backward dragged" = imputeddata <- imputeddata %>% 
             bind_rows(bdraggedestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Forward dragged" = imputeddata <- imputeddata %>% 
             bind_rows(fdraggedestimatesdisag %>%
                         filter(year %in% i) %>%
                         mutate(comment = paste0(comment, ", aggregated imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "[Keep existing estimates]" = imputeddata <- imputeddata %>% 
             bind_rows(FMexisitingestimates %>%
                         filter(year %in% i, flag == flag_FAOestimate) %>%
                         select(subseries, year, value, flag, comment))
           
    )
    
    # data_viz(data = imputeddata %>%
    #            arrange(year, subseries) %>%
    #            separate(subseries, c("geographic_area", "OC2", "OC3", "working_time", "sex"), "_"), 
    #          countryinput = country_input, 
    #          OC2input = OC2_input, 
    #          title = "Visualization of current state of aggregated imputation")
    
  }
  
  data_viz(data = imputeddata %>%
             arrange(year, subseries) %>%
             separate(subseries, c("geographic_area", "OC2", "OC3", "working_time", "sex"), "_"),
           countryinput = country_input,
           OC2input = OC2_input,
           title = "Visualization of aggregated imputation results")
  
  return(imputeddata)
  
}

# Subseries-level imputation

subseries_imputation <- function(ss, imputeddata, FMfiltered){
  
  imputation_on <- TRUE
  
  while (imputation_on) {
    
    selected_subseries <- select.list(c(gsub("_", " | ", ss, fixed = TRUE), "Stop imputation"), preselect = NULL, multiple = FALSE, graphics = TRUE, title=paste0("Which subseries would you like estimate?"))
    
    selected_subseries <- gsub(" | ", "_", selected_subseries, fixed = TRUE)
    
    if (selected_subseries == "Stop imputation") {
     
      imputation_on <- FALSE
      
      next
       
    }
    
    selected_estimator <- select.list(c("Polynomial regression", "Linear interpolation", "Historical average", "Historical growth", "Backward dragged", "Forward dragged", "[Remove estimated data]"), preselect = NULL, multiple = FALSE, graphics = TRUE, title = "Which estimator would you like to apply?")
    
    selected_year <- select.list(as.character(setdiff(years_all, FM_filtered$year[FM_filtered$subseries == selected_subseries])), preselect = NULL, multiple = TRUE, graphics = TRUE, title = "On which missing years should the estimation be performed?")
    
    #replace_existing <- "E" %in% filter(imputeddata, year %in% selected_year, subseries == selected_subseries)$flag # Check if the selected year/subseries combination includes existing estimates to be replaced
    
    switch(selected_estimator,
           
           "Polynomial regression" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(trend_estimator(trenddata = trend_fit(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all, startyear = start_year)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Linear interpolation" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(filter(linearint_estimator(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all), !is.na(value)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Historical average" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(filter(histavg_estimator(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all, threshold = histavg_threshold), !is.na(value)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Historical growth" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(filter(histgrowth_estimator(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all, threshold = histgrowth_threshold), !is.na(value)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Backward dragged" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(filter(bdragged_estimator(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all), !is.na(value)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "Forward dragged" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate)) %>%
             bind_rows(filter(fdragged_estimator(FMagg = filter(FMfiltered, subseries == selected_subseries), yearsdataexclmixed = years_all, yearsall = years_all), !is.na(value)) %>%
                         filter(year %in% as.integer(selected_year)) %>%
                         mutate(subseries = selected_subseries) %>%
                         mutate(comment = paste0(comment, ", subseries-level imputation")) %>%
                         mutate(timestamp = paste(Sys.time()))),
           
           "[Remove estimated data]" = imputeddata <- imputeddata %>%
             filter(!(year %in% selected_year & subseries == selected_subseries & flag == flag_FAOestimate))
    )
    
    data_viz(data = imputeddata %>%
               arrange(year, subseries) %>%
               separate(subseries, c("geographic_area", "OC2", "OC3", "working_time", "sex"), "_"), 
             countryinput = country_input, 
             OC2input = OC2_input, 
             title = "Visualization of current state of subseries imputation")
    
  }
  
  imputeddata$value <- as.integer(round(imputeddata$value, 0))
  
  return(imputeddata)
  
}

# Clean imputed data and assign time stamp

imputed_data_final <- function(imputeddata){
  
  imputeddata %>%
    arrange(year, subseries) %>%
    separate(subseries, c("geographic_area", "OC2", "OC3", "working_time", "sex"), "_")
  
}

# Function for more user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

# A function factory for getting integer axis values on plots.

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}