if(length(years_data_excl_mixed) >= obs_threshold_linreg) {
  
  # Aggregate datasets
  
  prod_agg <- prodagg(prodraw = prod_raw, countryinput = country_input, OC1input = OC1_input, startyear = start_year, endyear = end_year)
  
  ILO_labor_agg <- ILOlabor_agg(ILOlaborraw = ILO_labor_raw, countrynames = country_names, countryinput = country_input, targetlaborclassif1 = target_labor_classif1, startyear = start_year, endyear = end_year)
  
  # Generate dataset for regression
  
  reg_data <- regression_data()
  
  # Linear regression summary plots
  
  reg_variables_viz(reg_data)
  
  # Regression
  
  if (reg_type == 1) {
  
    regresult <- reg_automatic()
    
  } else if (reg_type == 2) {regresult <- reg_manual()}
  
  # Regression results
  
  reg_result_summary(regresult)
  
  # Determine model with best fit
  
  if (reg_type == 1) {
  
    best_fit_LM <- reg_auto_best_fit(regresult)
    
  } else if (reg_type == 2) {
    
    best_fit_LM <- 1
    
  }
  
  # Get adj-R2 for model with best fit
  
  best_fit_LM_R2 <- reg_auto_best_R2(regresult, best_fit_LM)
  
  # Add predictions of best LM model to data
  
  reg_data$predictions <- predict_reg(regresult, best_fit_LM)
  
  # Visualize fit of LM over data
  
  if (reg_type == 1) {
  
    reg_fit_viz(reg_data)
    
  } else if (reg_type == 2) {
  
    reg_fit_viz_manual(reg_data)
      
  }
  
  # Save predictions as dataframe
  
  reg_estimates_agg <- reg_estimator(reg_data)
  
  # Disaggregate estimates
  
  reg_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = reg_estimates_agg, comment = "Linear regression estimate")

}