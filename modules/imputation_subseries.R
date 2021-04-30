# Initialize imputed data table

imputed_data <- imputed_data_init(FMfiltered = FM_filtered)

# Subseries-level imputation

imputed_data <- subseries_imputation(ss = subseries, imputeddata = imputed_data, FMfiltered = FM_filtered)
