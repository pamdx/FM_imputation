if(length(years_data_excl_mixed) >= obs_threshold_trend) {

  # Fit polynomial trend to data 
  
  trend_predictions_agg <- trend_fit(FM_agg %>% filter(year %in% years_data_excl_mixed))
  
  # Show a plot of the fitted trend and the data
  
  trend_fit_viz(trend_predictions_agg)
  
  # Generate estimates for trend
  
  trends_estimates_agg <- trend_estimator(trend_predictions_agg)
  
  # Disaggregate estimates
  
trend_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = trends_estimates_agg, comment = paste0("Polynomial trend estimate (", trend_predictions_agg$trend_type[1], ")"))

}