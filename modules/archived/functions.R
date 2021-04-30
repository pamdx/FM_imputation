# Filter FM

FMfilter <- function(FMraw = FM_raw, countryinput = country_input, OC1input = OC1_input, startyear = start_year, endyear = end_year){
  
  FMraw %>%
    filter(geographic_area == countryinput) %>% # In Shiny, replace by input from dropdown menu
    filter(OC1 == OC1input) %>%
    filter(between(year, startyear, endyear)) %>%
    filter(!is.na(value)) %>%
    filter(is.na(flag) | flag == "A" | flag == "R") %>% # Make dynamic input in Shiny
    unite("subseries", geographic_area, OC1, OC2, working_time, gender, sep = "_", remove = FALSE) %>%
    select(geographic_area:comment, subseries)
  
}

# Get filtered data's subseries

getsubseries <- function(FMfiltered = FM_filtered){
  
  pull(distinct(filter(FMfiltered), subseries), subseries)
  
}

# Get dataset of current estimates

FMexisitingestimates <- function(FMraw = FM_raw, countryinput = country_input, OC1input= OC1_input, startyear = start_year, endyear = end_year){
  
  FMraw %>%
    filter(geographic_area == countryinput) %>% # In Shiny, replace by input from dropdown menu
    filter(OC1 == OC1input) %>%
    filter(between(year, startyear, endyear)) %>%
    filter(is.na(flag) | flag != "X") %>% # Make dynamic input in Shiny
    unite("subseries", geographic_area:gender, sep = "_", remove = FALSE) %>%
    select(geographic_area:comment, subseries)
  
}

# Function to generate visualizations of estimates

estimator_viz <- function(estimator, dataset){

  if (nrow(get(paste0(estimator, "_estimates_disag"))) > 0) {
    print(
      ggplot(final_estimates %>%
               mutate(data_alt = case_when(
                 is.na(data) ~ get(estimator),
                 !is.na(data) ~ data
               ))
             , aes(x = year)) +
        geom_bar(aes(y = data_alt, fill = subseries), stat="identity", colour="white", alpha = 0.4) + 
        geom_bar(aes(y = get(estimator), fill = subseries), stat="identity", colour="white") + 
        labs(title = paste("Visualization of official data and", estimator, "estimator"), subtitle = paste(country_input, "|", OC1_input), x = "Year", y = "Employment (people)", caption = "Transparent bars indicate official data or alternative sources. Solid bars indicate estimates.") +
        scale_y_continuous(labels = addUnits) + 
        theme(aspect.ratio = 3/4)
    )
  }
  
}

# Function to generate visualizations of pre- and post-processing data

data_viz <- function(data, country_input, OC1_input, title){
  
  print(
    ggplot(data, aes(x = year)) +
      geom_bar(aes(y = value, fill = subseries), stat="identity", colour="white", alpha = 0.4) + 
      geom_bar(aes(y = estimate, fill = subseries), stat="identity", colour="white") + 
      labs(title = title, subtitle = paste(country_input, "|", OC1_input), x = "Year", y = "Employment (people)", caption = "Transparent bars indicate official data or alternative sources. Solid bars indicate estimates.") +
      scale_y_continuous(labels = addUnits) + 
      theme(aspect.ratio = 3/4)
  )
  
}

# Identify aggregated Country/sector/year data where official and estimated data are mixed

mixedflags <- function(data = FM_raw, countrY = country_input, OC1 = OC1_input, startyear = start_year, endyear = end_year){
  
  FM_mixflags <- data %>%
    filter(geographic_area == countrY) %>%
    filter(OC1 == OC1) %>%
    filter(year %in% c(startyear:endyear)) %>%
    group_by(geographic_area, OC1, year) %>%
    summarise(flags = list(unique(flag)))
  
  dropflag <- c()
  
  flagtest <- lapply(FM_mixflags$flags, function(i){
    unlist(i, use.names = FALSE)
  })
  
  dropflag <- sapply(flagtest, function(i){
    append(dropflag, ifelse(NA %in% i & "B" %in% i | NA %in% i & "E" %in% i | NA %in% i & "F" %in% i | NA %in% i & "L" %in% i | NA %in% i & "P" %in% i, "1", "0"))
  })
  
  FM_mixflags["dropflag"] <- dropflag
  FM_mixflags <- FM_mixflags %>%
    unite(conc, c(geographic_area, OC1, year), sep = "_") %>%
    filter(dropflag == "1")
  mixed_flags <- as.data.frame(FM_mixflags$conc)
  mixed_flags <- mixed_flags %>%
    separate("FM_mixflags$conc", c("country","OC1", "year"), sep = "_")
  
  return(mixed_flags)
  
}

# Identify missing years

yearsdata <- function(data = FM_filtered){
  
  c(unique(data$year))
  
}

yearsdataexclmixed <- function(yearsdata = years_data, mixedflags = mixed_flags){
  
  setdiff(yearsdata, mixedflags$year)
  
}

yearsall <- function(startyear = start_year, endyear = end_year){
  
  c(startyear:endyear)
  
}

missingyears <- function(yearsall = years_all, yearsdata = years_data){
  
  setdiff(yearsall, yearsdata)
  
}

missingyearsseries <- function(missingyears = missing_years){
  
  split(missingyears, cumsum(c(1, diff(missingyears) != 1)))
  
}

missingyearsinclmixed <- function(yearsall = years_all, yearsdataexclmixed = years_data_excl_mixed){
  
  setdiff(yearsall, yearsdataexclmixed)
  
}

missingyearsseriesinclmixed <- function(missingyearsinclmixed = missing_years_incl_mixed){
  
  split(missingyearsinclmixed, cumsum(c(1, diff(missingyearsinclmixed) != 1)))
  
}

# Identify years where the continuity of subseries is interrupted

subseriesbreak <- function(yearsdata = years_data, startyear = start_year, FMfiltered = FM_filtered){
  
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

subseriesweights <- function(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed, missingyearsseriesinclmixed = missing_years_series_incl_mixed, yearsdata = years_data){
  
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

# Drag subseries and associated weights to missing years (subseries_weights_forward)

subseriesweightsforward <- function(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed, missingyearsseriesinclmixed = missing_years_series_incl_mixed){
  
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

discontinuoussubseries <- function(ss = subseries, FMfiltered = FM_filtered){
  
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

subseriestable <- function(FMfiltered = FM_filtered, ss = subseries, yearsall = years_all){
  
  FMfiltered %>% 
    select(subseries, year, value) %>%
    right_join(FMfiltered %>% expand(subseries, yearsall) %>% rename(year = yearsall)) %>%
    spread(year, value)
  
}

# Aggregate FM data for reg/trend estimates

FMagg <- function(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed){
  
  FMfiltered %>%
    filter(year %in% yearsdataexclmixed) %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    select(year, value)
  
}

# Function to aggregate production data

prodagg <- function(prodraw = prod_raw, countryinput = country_input, OC1input = OC1_input, startyear = start_year, endyear = end_year){
  
  prodraw %>%
    filter(country == countryinput) %>% # In Shiny, replace by input from dropdown menu
    filter(OC2 == OC1input) %>%
    filter(between(year, startyear, endyear)) %>%
    group_by(year) %>%
    summarise(prod_value = sum(prod_value))
}

# Function to aggregate ILO labor data

ILOlabor_agg <- function(ILOlaborraw = ILO_labor_raw, countrynames = country_names, countryinput = country_input, targetlaborclassif1 = target_labor_classif1, startyear = start_year, endyear = end_year){
  
  ILO_labor_agg <- ILOlaborraw %>%
    rename(iso3 = ref_area) %>%
    merge(countrynames) %>%
    filter(Country_en == countryinput) %>%
    filter(classif1 %in% targetlaborclassif1) %>% 
    filter(sex != "SEX_T") %>% 
    filter(between(time, startyear, endyear)) %>% 
    mutate(labor_value = obs_value*1000) %>%
    group_by(time) %>%
    summarise(labor_value = sum(labor_value)) %>%
    rename(year = time)
  
  ILO_labor_agg$year <- as.integer(ILO_labor_agg$year) 
  
  return(ILO_labor_agg)
  
}

# Generate dataset for regression

regression_data <- function(all_years = years_all, FM_data = FM_agg, ILO_labor_data = ILO_labor_agg, prod_data = prod_agg){
  
  return(data.frame(all_years) %>%
           rename(year = all_years) %>%
           left_join(FM_data %>% rename(emp_value = value), by = "year") %>%
           left_join(ILO_labor_data, by = "year") %>%
           left_join(prod_data, by = "year"))
  
}

# Plots of linear regression variables

reg_variables_viz <- function(data = reg_data){
  
  plot_emp <- ggplot(data, aes(x=year,y=emp_value, group = 1)) + 
    geom_line() + 
    scale_x_discrete(breaks = round(seq(start_year, end_year, by = 5),1)) +
    labs(title = paste(OC1_input, " employment (FAO data)"), 
         subtitle = country_input, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4)
  
  plot_prod <- ggplot(data, aes(x=year,y=prod_value, group = 1)) + 
    geom_line() + 
    scale_x_discrete(breaks = round(seq(start_year, end_year, by = 5),1)) +
    labs(title = paste(OC1_input, " production (FAO data)"), 
         subtitle = country_input, 
         y="Tonnes") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4)
  
  plot_labor <- ggplot(data, aes(x=year, y=labor_value, group = 1)) + 
    geom_line() + 
    scale_x_discrete(breaks = round(seq(start_year, end_year, by = 5),1)) +
    labs(title ="Labor force (ILO data)", 
         subtitle = country_input, 
         y="Number of people") +
    scale_y_continuous(labels = addUnits) + 
    theme(aspect.ratio = 3/4)
  
  return(grid.arrange(plot_emp, plot_prod, plot_labor, ncol=2))
  
}

# Regression with automatic model choice

reg_automatic <- function(data = reg_data, startyear = start_year, endyear = end_year){
  
  reg_data_ts <- ts(data, start = startyear)
  
  trend <- seq(startyear:endyear)
  
  try(fit_emp_1 <- lm(
    emp_value ~ trend + prod_value + labor_value,
    data = reg_data_ts), silent = TRUE)
  
  try(fit_emp_2 <- lm(
    emp_value ~ prod_value + labor_value,
    data = reg_data_ts), silent = TRUE)
  
  try(fit_emp_3 <- lm(
    emp_value ~ trend + prod_value,
    data = reg_data_ts), silent = TRUE)
  
  try(fit_emp_4 <- lm(
    emp_value ~ prod_value,
    data = reg_data_ts), silent = TRUE) 
  
  return(list(fit_emp_1, fit_emp_2, fit_emp_3, fit_emp_4))
  
}

# Regression with manual model choice

reg_manual <- function(data = reg_data, startyear = start_year, manual_model = reg_dynamic){

  reg_data_ts <- ts(data, start = startyear)
  
  fit_emp_custom <- lm(
    manual_model,
    data = reg_data_ts)
  
  return(list(fit_emp_custom))

}

# Summary of regressions

reg_result_summary <- function(reg_result){
  
  stargazer(reg_result, title = "Results of linear models", align = TRUE, type = "text") 
  
}

# Find model with best fit

reg_auto_best_fit <- function(regresult){
  
  LM1_r2 <- summary(regresult[[1]])$adj.r.squared
  LM2_r2 <- summary(regresult[[2]])$adj.r.squared
  LM3_r2 <- summary(regresult[[3]])$adj.r.squared
  LM4_r2 <- summary(regresult[[4]])$adj.r.squared
  
  return(c("1", "2", "3", "4")[which.max(c(LM1_r2, LM2_r2, LM3_r2, LM4_r2))])
  
}

# Get adj-R2 for model with best fit

reg_auto_best_R2 <- function(reg_result, best_fit = 1){
  
  round(summary(reg_result[[as.integer(best_fit)]])$adj.r.squared, digits = 3)
  
}

# Get predictions from model with best fit

predict_reg <- function(reg_result, best_fit = 1, data = reg_data, startyear = start_year){
  
  predict(reg_result[[as.integer(best_fit)]], ts(data, start = startyear))
  
}

# Visualize best LM model over data

reg_fit_viz <- function(data, best_fit = best_fit_LM, country = country_input, OC1 = OC1_input, best_R2 = best_fit_LM_R2){
  
  print(
    ggplot(data, aes(x = year, group = 1)) +
      geom_point(aes(y = emp_value, col = "Original data")) +
      geom_line(aes(y = predictions, col = paste0("LM", best_fit, (" (best fit)")))) +
      scale_x_discrete(breaks=seq(start_year, end_year, 5)) +
      labs(title = paste(country, ", ", OC1, " employment", sep = ""), subtitle = "Original data vs. predicted values from linear regression", caption = paste("Adjusted R-squared: ", best_R2)) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      theme(aspect.ratio = 3/4)
  ) 
  
}

reg_fit_viz_manual <- function(data, country = country_input, OC1 = OC1_input, best_R2 = best_fit_LM_R2){
  
  print(
    ggplot(data, aes(x = year, group = 1)) +
      geom_point(aes(y = emp_value, col = "Original data")) +
      geom_line(aes(y = predictions, col = "User-defined LM")) +
      scale_x_discrete(breaks=seq(start_year, end_year, 5)) +
      labs(title = paste(country, ", ", OC1, " employment", sep = ""), subtitle = "Original data vs. predicted values from linear regression", caption = paste("Adjusted R-squared: ", best_R2)) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      theme(aspect.ratio = 3/4)
  ) 
  
}

# Save predictions as dataframe

reg_estimator <- function(reg_data){
  
  return(reg_data %>%
           filter(is.na(emp_value)) %>%
           select(year, predictions) %>%
           rename(value = predictions) %>%
           filter(value >= 0))
  
}

# Function generating the predictions from the best polynomial fit

trend_fit <- function(data, threshold = obs_threshold_trend, all_years = years_all){
  
  if(length(pull(distinct(data, year))) >= threshold) {
    
    trend_data <- tibble(year = all_years) %>%
      left_join(data, by = "year")
    
    trend_data_agg_ts <- ts(trend_data, start = start_year)
    
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
  
}

# Function generating standardized estimates for trend

trend_estimator <- function(trend_data){
  
  trend_estimates <- trend_data %>%
    filter(is.na(value)) %>%
    select(year, predictions) %>%
    rename(value = predictions) %>%
    filter(value >= 0)
  
  trend_estimates$flag <- "E"
  trend_estimates$comment <- paste0("Polynomial trend estimate (", trend_predictions_agg$trend_type[1], ")")
  
  return(trend_estimates)
  
}

# Function generating a plot representing the polynomial trend with the best fit to the data

trend_fit_viz <- function(trend_data, start_year = start_year, end_year = end_year, country = country_input, OC1 = OC1_input){
  
  print(
    ggplot(trend_data, aes(x = year, group = 1)) +
      geom_point(aes(y = value, col = "Original data")) +
      geom_line(aes(y = predictions, col = paste(trend_type[1], "(best fit)"))) +
      scale_x_discrete(breaks=seq(start_year, end_year, 5)) +
      coord_cartesian(ylim=c(min(trend_data$value), max(trend_data$value))) +
      labs(title = paste(country, ", ", OC1, " employment", sep = ""), subtitle = "Original data vs. predicted values from polynomial trend", caption = paste("Adjusted R-squared: ", trend_data$r2adj[1])) + xlab("Year") + ylab("Employment (people)") +
      scale_y_continuous(labels = addUnits) + 
      theme(aspect.ratio = 3/4)
  ) 
  
}


# Function to generate historical average estimates

histavg_estimator <- function(data, all_years = years_all, threshold = histavg_threshold){
  
  histavg_estimates <- left_join(data.frame(all_years) %>% rename(year = all_years), data, by = "year") %>%
    mutate(histavg = NA)
  
  ref_years <- pull(distinct(data, year))
  missing_years <- setdiff(all_years, ref_years)
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
  
  histavg_estimates$flag <- "E"
  histavg_estimates$comment <- "Historical average estimate"
  
  return(histavg_estimates)
  
}

# Function to generate historical growth estimates

histgrowth_estimator <- function(data, all_years = years_all, threshold = histgrowth_threshold){
  
  histgrowth_estimates <- left_join(data.frame(all_years) %>% rename(year = all_years), data, by = "year") %>%
    mutate(histgrowth = NA)
  
  ref_years <- pull(distinct(data, year))
  missing_years <- setdiff(all_years, ref_years)
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
  
  histgrowth_estimates$flag <- "E"
  histgrowth_estimates$comment <- "Historical growth estimate"
  
  return(histgrowth_estimates)
  
}


# Function to generate forward dragged estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

fdragged_estimator <- function(data, all_years = years_all){
  
  ref_years <- pull(distinct(data, year))
  missing_years <- setdiff(all_years, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  fdragged_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (min(i)-1))){
      
      for (j in i) {
       
        fdragged_estimates$value[(fdragged_estimates$year == j)] <- data$value[(data$year == ref_years[which(ref_years == min(i)-1)])]
         
      }
      
    }
    
  }
  
  fdragged_estimates$flag <- "E"
  fdragged_estimates$comment <- "Forward dragged estimate"
  
  return(fdragged_estimates)
  
}


# Function to generate backward dragged estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

bdragged_estimator <- function(data, all_years = years_all){
  
  ref_years <- pull(distinct(data, year))
  missing_years <- setdiff(all_years, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  bdragged_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (max(i)+1))){
      
      for (j in i) {
        
        bdragged_estimates$value[(bdragged_estimates$year == j)] <- data$value[(data$year == ref_years[which(ref_years == max(i)+1)])] 
        
      }
      
    }
    
  }
  
  bdragged_estimates$flag <- "E"
  bdragged_estimates$comment <- "Backward dragged estimate"
  
  return(bdragged_estimates)
  
}


# Function to generate linearly interpolated estimates
# FOR AGGREGATED ESTIMATION, DATA MUST BE PROVIDED EXCLUDING MIXED YEARS

linearint_estimator <- function(data, all_years = years_all){
  
  ref_years <- pull(distinct(data, year))
  missing_years <- setdiff(all_years, ref_years)
  missing_years_series <- split(missing_years, cumsum(c(1, diff(missing_years) != 1)))
  
  linearint_estimates <- tibble(year = years_all, value = NA)
  
  for(i in missing_years_series){
    
    if(any(ref_years == (min(i)-1)) & any(ref_years == (max(i)+1))){
      
      ref_year_before <- ref_years[which(ref_years == min(i)-1)]
      ref_value_before <- data$value[(data$year == ref_year_before)]
      
      ref_year_after <- ref_years[which(ref_years == max(i)+1)]
      ref_value_after <- data$value[(data$year == ref_year_after)]
      
      # for each year in between, calculate linear interpolation, then split in ratios based on subseries
      
      for (j in i) {
        
        linearint_estimates$value[(linearint_estimates$year == j)] <- (ref_value_before*(ref_year_after-j)+ref_value_after*(j-ref_year_before))/(ref_year_after-ref_year_before)
        
      }
    }
  }
  
  linearint_estimates$flag <- "E"
  linearint_estimates$comment <- "Linear interpolation estimate"
    
  return(linearint_estimates)
  
}

# Function to disaggregate estimates

estimates_disaggregation <- function(weights, mflags = mixed_flags, estimates_agg, country_input = country_input, OC1_input = OC1_input, FM_filtered = FM_filtered, missing_years = missing_years_incl_mixed, comment){
  
  estimates_disag <- inner_join(weights[!(weights$year %in% mflags$year),], estimates_agg, by = "year") %>%
    mutate(value = ratio*value.y) %>%
    select(subseries, year, value) %>%
    rbind(t(sapply(mflags$year, function(i){
      c("subseries" = paste(country_input, OC1_input, "Unspecified_Status Unspecified_U", sep = "_"), "year" = i, "value" = estimates_agg$value[estimates_agg$year == i]-FM_filtered$value[FM_filtered$year == i])
    }))) %>%
    arrange(year) %>%
    filter(year %in% missing_years)
  estimates_disag$flag <- "E"
  estimates_disag$value <- as.integer(round(as.numeric(estimates_disag$value)))
  estimates_disag$year <- as.integer(estimates_disag$year)
  estimates_disag$comment <- comment
  
  return(estimates_disag)
  
}

# Function aggregating all estimates in a table

estimates_table <- function(data_filtered = FM_filtered, reg_estimates, trend_estimates, linearint_estimates, histavg_estimates, histgrowth_estimates, bdragged_estimates, fdragged_estimates){
  
  final_estimates <- data_filtered %>% add_column(type = "data") %>% select(subseries, year, value, type) %>%
    bind_rows(reg_estimates %>% add_column(type = "reg") %>% select(subseries, year, value, type)) %>%
    bind_rows(trend_estimates %>% add_column(type = "trend") %>% select(subseries, year, value, type)) %>%
    bind_rows(linearint_estimates %>% add_column(type = "linearint") %>% select(subseries, year, value, type)) %>%
    bind_rows(histavg_estimates %>% add_column(type = "histavg") %>% select(subseries, year, value, type)) %>%
    bind_rows(histgrowth_estimates %>% add_column(type = "histgrowth") %>% select(subseries, year, value, type)) %>%
    bind_rows(bdragged_estimates %>% add_column(type = "bdragged") %>% select(subseries, year, value, type)) %>%
    bind_rows(fdragged_estimates %>% add_column(type = "fdragged") %>% select(subseries, year, value, type)) %>%
    spread(type, value) %>%
    arrange(year, subseries) %>%
    separate(subseries, c("geographic_area", "oc1", "oc2", "working_time", "sex"), "_") %>%
    unite(subseries, oc2, working_time, sex, sep = " | ", remove = TRUE) %>%
    select(-c(1:2)) %>%
    select(subseries, year, data, reg, trend, linearint, histavg, histgrowth, bdragged, fdragged)
  
}

# Initialize imputed data table

imputed_data_init <- function(FMfiltered = FM_filtered){
  
  FMfiltered %>%
    select(subseries, year, value, flag, comment)
  
}

# Impute data by groups of consecutive missing years

agg_imputation_consecutive <- function(missingyearsseriesinclmixed = missing_years_series_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag){
  
  for (i in missingyearsseriesinclmixed) {
    
    selected_estimator <- menu(c("Linear regression", "Polynomial trend", "Linear interpolation", "Historical average", "Historical growth", "Backward dragged", "Forward dragged"), title=paste0("Which estimator would you like to select for the following year(s): ", toString(i), "?"))
    
    switch(selected_estimator,
           
           '1' = imputeddata <- imputeddata %>% 
             bind_rows(filter(regestimatesdisag, year %in% i)),
           
           '2' = imputeddata <- imputeddata %>% 
             bind_rows(filter(trendestimatesdisag, year %in% i)),
           
           '3' = imputeddata <- imputeddata %>% 
             bind_rows(filter(linearintestimatesdisag, year %in% i)),
           
           '4' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histavgestimatesdisag, year %in% i)),
           
           '5' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histgrowthestimatesdisag, year %in% i)),
           
           '6' = imputeddata <- imputeddata %>% 
             bind_rows(filter(bdraggedestimatesdisag, year %in% i)),
           
           '7' = imputeddata <- imputeddata %>% 
             bind_rows(filter(fdraggedestimatesdisag, year %in% i)),
           
    )
  }
  
  return(imputeddata)
  
}

# Impute data by individual missing years

agg_imputation_yby <- function(missingyearsinclmixed = missing_years_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag){
  
  for (i in missingyearsinclmixed) {
    
    selected_estimator <- menu(c("Linear regression", "Polynomial trend", "Linear interpolation", "Historical average", "Historical growth", "Backward dragged", "Forward dragged"), title=paste0("Which estimator would you like to select for the following year(s): ", toString(i), "?"))
    
    switch(selected_estimator,
           
           '1' = imputeddata <- imputeddata %>% 
             bind_rows(filter(regestimatesdisag, year == i)),
           
           '2' = imputeddata <- imputeddata %>% 
             bind_rows(filter(trendestimatesdisag, year == i)),
           
           '3' = imputeddata <- imputeddata %>% 
             bind_rows(filter(linearintestimatesdisag, year == i)),
           
           '4' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histavgestimatesdisag, year == i)),
           
           '5' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histgrowthestimatesdisag, year == i)),
           
           '6' = imputeddata <- imputeddata %>% 
             bind_rows(filter(bdraggedestimatesdisag, year == i)),
           
           '7' = imputeddata <- imputeddata %>% 
             bind_rows(filter(fdraggedestimatesdisag, year == i)),
           
    )
  }
  
  return(imputeddata)
  
}

# Subseries-level imputation

subseries_imputation <- function(ss = subseries, imputeddata = imputed_data, FMfiltered = FM_filtered){
  
  imputation_on <- TRUE
  
  while (imputation_on) {
    
    selected_subseries <- ss[menu(c(ss), title=paste0("Which subseries would you like estimate?"))]
    
    # Estimator selection
    
    selected_estimator <- menu(c("Polynomial trend", "Linear interpolation", "Historical average", "Historical growth", "Backward dragged", "Forward dragged", "Stop imputation"), title = "Which estimator would you like to apply?")
    
    switch(selected_estimator,
           
           '1' = imputeddata <- imputeddata %>% 
             bind_rows(trend_estimator(trend_fit(filter(FMfiltered, subseries == selected_subseries))) %>%
                         mutate(subseries = selected_subseries)),
           
           '2' = imputeddata <- imputeddata %>% 
             bind_rows(filter(linearint_estimator(filter(FMfiltered, subseries == selected_subseries)), !is.na(value)) %>%
                         mutate(subseries = selected_subseries)),
           
           '3' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histavg_estimator(filter(FMfiltered, subseries == selected_subseries)), !is.na(value)) %>%
                         mutate(subseries = selected_subseries)),
           
           '4' = imputeddata <- imputeddata %>% 
             bind_rows(filter(histgrowth_estimator(filter(FMfiltered, subseries == selected_subseries)), !is.na(value)) %>%
                         mutate(subseries = selected_subseries)),
           
           '5' = imputeddata <- imputeddata %>% 
             bind_rows(filter(bdragged_estimator(filter(FMfiltered, subseries == selected_subseries)), !is.na(value)) %>%
                         mutate(subseries = selected_subseries)),
           
           '6' = imputeddata <- imputeddata %>% 
             bind_rows(filter(fdragged_estimator(filter(FMfiltered, subseries == selected_subseries)), !is.na(value)) %>%
                         mutate(subseries = selected_subseries)),
           
           '7' = imputation_on <- FALSE
    )
    
  }
  
  return(imputeddata)
  
}

# Clean imputed data and assign time stamp

imputed_data_final <- function(imputeddata = imputed_data){
  
  imputeddata <- imputeddata %>%
    arrange(year, subseries) %>%
    separate(subseries, c("geographic_area", "oc1", "oc2", "working_time", "sex"), "_")
  
  imputeddata$timestamp[(!is.na(imputeddata$flag))] <- paste(Sys.time())
  
  return(imputed_data)
  
}

# Function for more user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}