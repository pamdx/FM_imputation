# Calculate productivity with imputed data

productivity_imputed <- productivity_table(imputed_data %>%
                                             group_by(year) %>%
                                             summarise(emp_value = sum(value)), prod_agg)

# Create R Markdown report

rmarkdown::render(path_report_imputation, output_dir = output_directory, output_file = paste0(iconv(gsub("/", "-", country_input), to="ASCII//TRANSLIT"), "_", OC2_input, "_report.html"))

# Save imputed data as CSV

write_excel_csv(imputed_data, paste0(output_directory, gsub("/", "-", country_input), "_", OC2_input, "_imputed.csv"), na = "")
