##### IMPUTATION OF AGGREGATED ESTIMATIONS #####

# Initialize imputed data table

imputed_data <- imputed_data_init(FMfiltered = FM_filtered)

# Impute data by groups of consecutive missing years

imputed_data <- agg_imputation_consecutive(missingyearsseriesinclmixed = missing_years_series_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag)

# Impute data by individual missing years
# 
# imputed_data <- agg_imputation_yby(missingyearsseriesinclmixed = missing_years_series_incl_mixed, imputeddata = imputed_data, regestimatesdisag = reg_estimates_disag, trendestimatesdisag = trend_estimates_disag, linearintestimatesdisag = linearint_estimates_disag, histavgestimatesdisag = histavg_estimates_disag, histgrowthestimatesdisag = histgrowth_estimates_disag, bdraggedestimatesdisag = bdragged_estimates_disag, fdraggedestimatesdisag = fdragged_estimates_disag)