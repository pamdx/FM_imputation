#################################
##### AGGREGATED ESTIMATION #####
#################################

##### LM ESTIMATION #####

if(length(years_data_excl_mixed) >= obs_threshold_linreg & OC2_input %in% c("Aquaculture", "Marine fishing", "Inland fishing")) {
  
  # Aggregate datasets
  
  ILO_labor_agg <- ILOlabor_agg(ILOlaborraw = ILO_labor_raw, countryinput = country_input, startyear = start_year, endyear = end_year)
  
  if (country_input %in% OECD_countries & OC2_input == "Marine fishing") {
   
    OECD_fleet_agg <- OECDfleet_agg(OECDfleetraw = OECD_fleet_raw, countryinput = country_input, startyear = start_year, endyear = end_year)
     
  }
  
  # Generate dataset for regression
  
  if (country_input %in% OECD_countries & OC2_input == "Marine fishing") {
    
    reg_data <- regression_data_fleet(yearsall = years_all, FMagg = FM_agg, ILOlaboragg = ILO_labor_agg, prodagg = prod_agg, OECDfleetagg = OECD_fleet_agg)
    
  } else reg_data <- regression_data(yearsall = years_all, FMagg = FM_agg, ILOlaboragg = ILO_labor_agg, prodagg = prod_agg)
  
  # Linear regression summary plots
  
  if (country_input %in% OECD_countries & OC2_input == "Marine fishing") {
    
    reg_variables_viz_fleet(regdata = reg_data, startyear = start_year, endyear = end_year, OC2input = OC2_input, countryinput = country_input)
    
  } else reg_variables_viz(regdata = reg_data, startyear = start_year, endyear = end_year, OC2input = OC2_input, countryinput = country_input)
  
  # Regression
  
  reg_result <- if (reg_type == 1) {
    
    if (country_input %in% OECD_countries & OC2_input == "Marine fishing") {
      
      reg_automatic_fleet(regdata = reg_data, startyear = start_year, endyear = end_year)
      
    } else reg_automatic(regdata = reg_data, startyear = start_year, endyear = end_year)
    
  } else if (reg_type == 2) {
    
      reg_manual(regdata = reg_data, startyear = start_year, manual_model = reg_dynamic)
      
      }
  
  # Regression results
  
  reg_result_summary(regresult = reg_result)
  
  # Determine model with best fit
  
  best_fit_LM <- if (reg_type == 1) {
    
    reg_auto_best_fit(regresult = reg_result)
    
  } else if (reg_type == 2) {
    
    1
    
  }
  
  # Get adj-R2 for model with best fit
  
  best_fit_LM_R2 <- reg_auto_best_R2(regresult = reg_result, bestfit = best_fit_LM)
  
  # Add predictions of best LM model to data
  
  reg_data$predictions <- predict_reg(regresult = reg_result, bestfit = best_fit_LM, regdata = reg_data, startyear = start_year)
  
  # Visualize fit of LM over data
  
  if (reg_type == 1) {

    reg_fit_viz(regdata = reg_data, bestfit = best_fit_LM, startyear = start_year, endyear = end_year, countryinput = country_input, OC2input = OC2_input, best_R2 = best_fit_LM_R2) 
    
  } else if (reg_type == 2) {

    reg_fit_viz_manual(regdata = reg_data, startyear = start_year, endyear = end_year, countryinput = country_input, OC2input = OC2_input, best_R2 = best_fit_LM_R2)
    
  }
  
  # Save predictions as dataframe
  
  reg_estimates_agg <- reg_estimator(regdata = reg_data)
  
  # Disaggregate estimates
  
  if (best_fit_LM_R2 >= fit_threshold_reg) {
   
    reg_estimates_disag <- estimates_disaggregation(weights = subseries_weights, mflags = mixed_flags, estimatesagg = reg_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Linear regression estimate")
     
  } else reg_estimates_disag <- tibble(subseries = character(), year = integer(), value = integer(), flag = character(), comment = character())
  
} else reg_estimates_disag <- tibble(subseries = character(), year = integer(), value = integer(), flag = character(), comment = character())

##### POLYNOMIAL TREND ESTIMATION #####

if(length(years_data_excl_mixed) >= obs_threshold_trend) {
  
  # Fit polynomial trend to data 
  
  trend_predictions_agg <- trend_fit(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall = years_all, startyear = start_year)
  
  # Show a plot of the fitted trend and the data
  
  trend_fit_viz(trenddata = trend_predictions_agg, startyear = start_year, endyear = end_year, countryinput = country_input, OC2input = OC2_input)
  
  # Generate estimates for trend
  
  trends_estimates_agg <- trend_estimator(trenddata = trend_predictions_agg)
  
  # Disaggregate estimates
  
  if (unique(trend_predictions_agg$r2adj) >= fit_threshold_trend) {
  
    trend_estimates_disag <- estimates_disaggregation(weights = subseries_weights, mflags = mixed_flags, estimatesagg = trends_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = paste0("Polynomial trend estimate (", trend_predictions_agg$trend_type[1], ")"))
      
  } else trend_estimates_disag <- tibble(subseries = character(), year = integer(), value = integer(), flag = character(), comment = character())
  
} else trend_estimates_disag <- tibble(subseries = character(), year = integer(), value = integer(), flag = character(), comment = character())

##### LBFH ESTIMATIONS #####

### Backward dragged estimates computation

  bdragged_estimates_agg <- bdragged_estimator(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall= years_all)
  
  # Disaggregate bdragged estimates
  
  bdragged_estimates_disag <- estimates_disaggregation(weights = subseries_weights, mflags = mixed_flags, estimatesagg = bdragged_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Backward dragged estimate")


### Forward dragged estimates computation

  fdragged_estimates_agg <- fdragged_estimator(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall= years_all)
  
  # Disaggregate fdragged estimates
  
  fdragged_estimates_disag <- estimates_disaggregation(weights = subseries_weights_forward, mflags = mixed_flags, estimatesagg = fdragged_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Forward dragged estimate")


### Linearly interpolated estimates computation

  linearint_estimates_agg <- linearint_estimator(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall= years_all)
  
  # Disaggregate linearint estimates
  
  linearint_estimates_disag <- estimates_disaggregation(weights = subseries_weights_interpolated, mflags = mixed_flags, estimatesagg = linearint_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Linear interpolation estimate")


### Historical average computation (aggregated, last X years)

  histavg_estimates_agg <- histavg_estimator(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall= years_all, threshold = histavg_threshold)
  
  # Disaggregate histavg estimates
  
  histavg_estimates_disag <- estimates_disaggregation(weights = subseries_weights, mflags = mixed_flags, estimatesagg = histavg_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Historical average estimate")


### Historical growth computation (aggregated, last X years)

  histgrowth_estimates_agg <- histgrowth_estimator(FMagg = FM_agg, yearsdataexclmixed = years_data_excl_mixed, yearsall= years_all, threshold = histgrowth_threshold)
  
  # Disaggregate histgrowth estimates
  
  histgrowth_estimates_disag <- estimates_disaggregation(weights = subseries_weights, mflags = mixed_flags, estimatesagg = histgrowth_estimates_agg, countryinput = country_input, OC2input = OC2_input, FMfiltered = FM_filtered, missingyears = missing_years_incl_mixed, comment = "Historical growth estimate")


##### SUMMARY OF AGGREGATED ESTIMATIONS #####

# Create estimates datasets for each subseries

final_estimates <- estimates_table(FMfiltered = FM_filtered, reg_estimates = reg_estimates_disag, trend_estimates = trend_estimates_disag, linearint_estimates = linearint_estimates_disag, histavg_estimates = histavg_estimates_disag, histgrowth_estimates = histgrowth_estimates_disag, bdragged_estimates = bdragged_estimates_disag, fdragged_estimates = fdragged_estimates_disag)

# Create rainbow visualization for each estimator type

estimator_viz(method = "reg", estimator = "linear regression", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "trend", estimator = "polynomial regression", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "linearint", estimator = "linear interpolation", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "histavg", estimator = "historical average", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "histgrowth", estimator = "historical growth", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "bdragged", estimator = "backward dragged", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)
estimator_viz(method = "fdragged", estimator = "forward dragged", dataset = final_estimates, countryinput = country_input, OC2input = OC2_input)

# Messages to user

print(paste0("There are ", length(subseries_break), " change(s) in subseries (", toString(subseries_break), ")."))
print(paste0("There are ", nrow(mixed_flags), " year(s) where official data was mixed with estimates (", toString(mixed_flags$year), ")."))
