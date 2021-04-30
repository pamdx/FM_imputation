# Create estimates datasets for each subseries

final_estimates <- estimates_table(FM_filtered, reg_estimates_disag, trend_estimates_disag, linearint_estimates_disag, histavg_estimates_disag, histgrowth_estimates_disag, bdragged_estimates_disag, fdragged_estimates_disag)

# Create rainbow visualization for each estimator type

estimator_viz("reg", final_estimates)
estimator_viz("trend", final_estimates)
estimator_viz("linearint", final_estimates)
estimator_viz("histavg", final_estimates)
estimator_viz("histgrowth", final_estimates)
estimator_viz("bdragged", final_estimates)
estimator_viz("fdragged", final_estimates)

# Messages to user

print(paste0("There are ", length(subseries_break), " change(s) in subseries (", toString(subseries_break), ")."))
print(paste0("There are ", nrow(mixed_flags), " year(s) where official data was mixed with estimates (", toString(mixed_flags$year), ")."))