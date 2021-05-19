# Clean imputed data and assign time stamp

imputed_data <- imputed_data_final(imputeddata = imputed_data)

# Generate chart of imputed data

data_viz(data = imputed_data, countryinput = country_input, OC1input = OC1_input, title = "Visualization of final imputed data")

ggsave(paste0("./outputs/", country_input, "_", OC1_input, "_imputed.png"), dpi = 300)

# Save imputed data as CSV

write_excel_csv(imputed_data, paste0("./outputs/", country_input, "_", OC1_input, "_imputed.csv"), na = "")

# Create comparison table between original data and processed data

ctable = compare_df(imputed_data %>%
                      select(geographic_area:comment) %>%
                      arrange(geographic_area, year, OC1, OC2, working_time, sex), 
                    FM_exisiting_estimates %>% 
                      select(geographic_area, OC1, OC2, working_time, sex, year, value, flag, comment) %>%
                      arrange(geographic_area, year, OC1, OC2, working_time, sex), 
                    c("geographic_area", "OC1", "OC2", "working_time", "sex", "year"))

# Output HTML table

create_output_table(
  ctable,
  output_type = "html",
  file_name = paste0("./outputs/", country_input, "_", OC1_input, "_comparison.html"),
  limit = 1000)
