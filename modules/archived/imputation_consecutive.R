# Initialize imputed data table

imputed_data <- imputed_data_init()

# Impute data by groups of consecutive missing years

imputed_data <- agg_imputation_consecutive()