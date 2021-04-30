### First stage variables

subseries_names <- pull(distinct(filter(FM_filtered), subseries), subseries)
subseries_number <- length(subseries_names)
subseries_coverage <- list()

for (i in subseries_names) {
  
  years_covered <- FM_filtered %>%
    filter(subseries == i) %>%
    pull(unique(year))
  
  subseries_coverage <- c(subseries_coverage, list(years_covered))
  
}

subseries_coverage_unique <- unique(lapply(subseries_coverage, sort))

names(subseries_coverage) <- subseries_names

common_coverage <- Reduce(intersect, subseries_coverage)

years_all_subseries <- unlist(subseries_coverage_unique)

year_coverage_table <- FM_filtered %>%
  group_by(subseries, year) %>%
  summarise(value = sum(value)) %>%
  spread(key = year, value = value)

### Decision tree

if (subseries_number == 0) {
  
  first_stage_pattern <- 1
  
} else if (subseries_number == 1) {
  
  first_stage_pattern <- 2
  
} else if (subseries_number > 1) {
  
  if (setequal(common_coverage, unlist(subseries_coverage[1]))) {
    
    first_stage_pattern <- 3
    
  } else if (any(duplicated(years_all_subseries))) {
    
    first_stage_pattern <- 4
    
  } else first_stage_pattern <- 6
  
}

if (first_stage_pattern == 2 | first_stage_pattern == 3) {
  
  if (OC1_input != "Subsistence" | OC1_input != "Unspecified" | OC1_input != "Processing") {
    
    if (best_fit_LM_R2 >= fit_threshold) {
      
      recommended_estimator <- "LM"
      
    } else if (best_fit_trend_R2 >= fit_threshold) {
      
      recommended_estimator <- "Trend"
      
    } else recommended_estimator <- "LBFH"
    
  }
  
} else if (first_stage_pattern == 4) {
  
  if (best_fit_trend_R2_agg >= fit_threshold) {
    
    recommended_estimator <- "Trend"
    
  } else recommended_estimator <- "LBFH"
  
}
