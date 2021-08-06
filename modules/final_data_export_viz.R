# Calculate productivity with imputed data

productivity_imputed <- productivity_table(imputed_data %>%
                                             group_by(year) %>%
                                             summarise(emp_value = sum(value)), prod_agg)

# Create R Markdown report

rmarkdown::render("report.Rmd", output_file = paste0("./outputs/", gsub("/", "-", country_input), "_", OC2_input, "_report.html"))

# Save imputed data as CSV

write_excel_csv(imputed_data, paste0("./outputs/", gsub("/", "-", country_input), "_", OC2_input, "_imputed.csv"), na = "")