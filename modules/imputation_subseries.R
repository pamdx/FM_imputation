# Initialize imputed data table

imputed_data <- imputed_data_init(data = FM_exisiting_estimates)

# Subseries-level imputation

imputed_data <- subseries_imputation(ss = subseries, imputeddata = imputed_data, FMfiltered = FM_filtered)

# Clean imputed data and assign time stamp

imputed_data <- imputed_data_final(imputeddata = imputed_data)

imputation_type <- "Subseries-level imputation"