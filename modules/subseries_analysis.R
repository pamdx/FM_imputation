# Basic FM filter

FM_filtered <- FMfilter(FMraw = FM_raw, countryinput = country_input, OC2input = OC2_input, startyear = start_year, endyear = end_year)

subseries <- getsubseries(FMfiltered = FM_filtered)

# Visualization of current estimates

FM_exisiting_estimates <- FMexisitingestimates(FMraw = FM_raw, countryinput = country_input, OC2input = OC2_input, startyear = start_year, endyear = end_year)

data_viz(data = FM_exisiting_estimates, countryinput = country_input, OC2input = OC2_input, title = "Visualization of existing data and estimates")

# Identify aggregated Country/sector/year data where official and estimated data are mixed

mixed_flags <- mixedflags(FMraw = FM_raw, countryinput = country_input, OC2input = OC2_input, startyear = start_year, endyear = end_year)

# Identify missing years

years_data <- yearsdata(FMfiltered = FM_filtered)
years_data_excl_mixed <- yearsdataexclmixed(yearsdata = years_data, mixedflags = mixed_flags)
years_all <- yearsall(startyear = start_year, endyear = end_year)
missing_years <- missingyears(yearsall = years_all, yearsdata = years_data)
missing_years_series <- missingyearsseries(missingyears = missing_years)
missing_years_incl_mixed <- missingyearsinclmixed(yearsall = years_all, yearsdataexclmixed = years_data_excl_mixed)
missing_years_series_incl_mixed <- missingyearsseriesinclmixed(missingyearsinclmixed = missing_years_incl_mixed)

if (length(missing_years) == 0) {
  
  print("There is no year with missing data for the selected period. No imputation is needed.")

  }

# Identify years where the continuity of subseries is interrupted

subseries_break <- subseriesbreak(yearsdata = years_data, startyear = start_year, FMfiltered = FM_filtered)

# Summarize weight of each subseries for each year

subseries_weights <- subseriesweights(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed, missingyearsseriesinclmixed = missing_years_series_incl_mixed, yearsdata = years_data)
subseries_weights_interpolated <- subseriesweightsinterpolated(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed, missingyearsseriesinclmixed = missing_years_series_incl_mixed, yearsdata = years_data)
subseries_weights_forward <- subseriesweightsforward(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed, missingyearsseriesinclmixed = missing_years_series_incl_mixed)

# Inspect discontinuities in subseries

discontinuous_subseries <- discontinuoussubseries(ss = subseries, FMfiltered = FM_filtered)

# Generate wide table showing data by subseries

subseries_table <- subseriestable(FMfiltered = FM_filtered, ss = subseries, yearsall = years_all)

# Aggregate FM and PROD data for reg/trend estimates, productivity charts

FM_agg <- FMagg(FMfiltered = FM_filtered, yearsdataexclmixed = years_data_excl_mixed)

prod_agg <- prodagg(prodraw = prod_raw, countryinput = country_input, OC2input = OC2_input, startyear = start_year, endyear = end_year)

# Calculate productivity with existing estimates

productivity_existing <- productivity_table(FM_exisiting_estimates %>%
                                              group_by(year) %>%
                                              summarise(emp_value = sum(value)), prod_agg)