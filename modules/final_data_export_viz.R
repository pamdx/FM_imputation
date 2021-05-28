# Save current estimates chart

data_viz(data = FM_exisiting_estimates, countryinput = country_input, OC2input = OC2_input, title = "Visualization of current estimates")

ggsave(paste0("./outputs/", country_input, "_", OC2_input, "_existing.png"), dpi = 300)

# Generate chart of imputed data

data_viz(data = imputed_data, countryinput = country_input, OC2input = OC2_input, title = "Visualization of final imputed data")

ggsave(paste0("./outputs/", country_input, "_", OC2_input, "_imputed.png"), dpi = 300)

# Save imputed data as CSV

write_excel_csv(imputed_data, paste0("./outputs/", country_input, "_", OC2_input, "_imputed.csv"), na = "")

# Create comparison table between original data and processed data

ctable = compare_df(imputed_data %>%
                      select(geographic_area:comment) %>%
                      arrange(geographic_area, year, OC2, OC3, working_time, sex), 
                    FM_exisiting_estimates %>% 
                      select(geographic_area, OC2, OC3, working_time, sex, year, value, flag, comment) %>%
                      arrange(geographic_area, year, OC2, OC3, working_time, sex), 
                    c("geographic_area", "OC2", "OC3", "working_time", "sex", "year"))

# Output HTML table

create_output_table(
  ctable,
  output_type = "html",
  file_name = paste0("./outputs/", country_input, "_", OC2_input, "_comparison.html"),
  limit = 1000)
