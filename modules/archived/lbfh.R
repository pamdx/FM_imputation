##### Backward dragged estimates computation

bdragged_estimates_agg <- bdragged_estimator(FM_agg %>% filter(year %in% years_data_excl_mixed))

# Disaggregate bdragged estimates

bdragged_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = bdragged_estimates_agg, comment = "Backward dragged estimate")


##### Forward dragged estimates computation

fdragged_estimates_agg <- fdragged_estimator(FM_agg %>% filter(year %in% years_data_excl_mixed))

# Disaggregate fdragged estimates

fdragged_estimates_disag <- estimates_disaggregation(weights = subseries_weights_forward, estimates_agg = fdragged_estimates_agg, comment = "Forward dragged estimate")


##### Linearly interpolated estimates computation

linearint_estimates_agg <- linearint_estimator(FM_agg %>% filter(year %in% years_data_excl_mixed))

# Disaggregate linearint estimates

linearint_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = linearint_estimates_agg, comment = "Linear interpolation estimate")


##### Historical average computation (aggregated, last X years)

histavg_estimates_agg <- histavg_estimator(FM_agg %>% filter(year %in% years_data_excl_mixed))

# Disaggregate histavg estimates

histavg_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = histavg_estimates_agg, comment = "Historical average estimate")


##### Historical growth computation (aggregated, last X years)

histgrowth_estimates_agg <- histgrowth_estimator(FM_agg %>% filter(year %in% years_data_excl_mixed))

# Disaggregate histgrowth estimates

histgrowth_estimates_disag <- estimates_disaggregation(weights = subseries_weights, estimates_agg = histgrowth_estimates_agg, comment = "Historical growth estimate")
