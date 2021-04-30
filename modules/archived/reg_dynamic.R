if(length(years_data_excl_mixed) >= obs_threshold_linreg) {
    
  # Generate dataset for regression
  
  reg_data <- regression_data()
  
  # Linear regression summary plots
  
  reg_variables_viz(reg_data)
    
  # Regression
  
  reg_result <- reg_manual()
  
  # Regression results
  
  reg_result_summary(reg_result)
  
  # Get adj-R2 for model with best fit
  
  best_fit_LM_R2 <- reg_auto_best_R2(reg_result, best_fit_LM)
  
  # Add predictions of best LM model to data
  
  reg_data$predictions <- predict_reg(reg_result, best_fit_LM)

  # Visualize fit of LM over data
  
  reg_fit_viz_manual(reg_data)
  
  # Save predictions as dataframe
  
  reg_estimates_agg <- reg_estimator(reg_data)
  
  # Disaggregate estimates
  
  reg_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = reg_estimates_agg, comment = "Linear regression estimate")

}