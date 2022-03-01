##### IMPUTATION OF AGGREGATED ESTIMATIONS #####

# Initialize imputed data table

imputed_data <- imputed_data_init(data = FM_filtered)

# Impute data (consecutive or year-by-year)

imputed_data <- if (agg_imputation_type == 1) {
  
  aggregated_imputation(finalestimates = final_estimates, missingyears = missing_years_series_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag, FMexisitingestimates = FM_exisiting_estimates)
  
} else if (agg_imputation_type == 2) {
  
  aggregated_imputation(finalestimates = final_estimates, missingyears = missing_years_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag, FMexisitingestimates = FM_exisiting_estimates)
  
}

# Clean imputed data and assign time stamp

imputed_data <- imputed_data_final(imputeddata = imputed_data)

imputation_type <- "Aggregated imputation"