# Load packages

library(shiny)
library(DT)

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(compareDF)
library(Rilostat)
library(stargazer)
library(gridExtra)

theme_set(theme_bw())

# Load non-reactive data and functions

source("./modules/data_import_shiny.R")
source("./modules/functions_shiny.R")

# Define UI ----
ui <- navbarPage("EMPUTATOR (beta)",
        
        # First tab
        tabPanel("Data import",
        
        sidebarLayout(
          sidebarPanel(
            helpText("Please select the country, sector and period of interest."),
            
            selectInput("country", label = h4("Choose a country"), choices = c(unique(FM_raw$geographic_area))),
            
            selectInput("OC1", label = h4("Choose a sector"), choices = c(unique(FM_raw$OC1))),
            
            sliderInput("year_range", label = h4("Define a starting and ending year"), min = 1950, 
                        max = as.integer(format(Sys.Date(), "%Y")), value = c(1995, as.integer(format(Sys.Date(), "%Y"))-2), sep = ""), 
            
            width = 3),
          
          mainPanel(
            tabsetPanel(
              tabPanel("Chart", plotOutput("viz_exisiting_estimates")), 
              tabPanel("Table", dataTableOutput("subseries_table"))
              )
            )
          )
        ),
      
        # Second tab
        tabPanel("Estimations",
                 
         sidebarLayout(
           sidebarPanel(
             
             actionButton("compute_estimates", label = "Compute estimates", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             
             hr(),
             
             helpText("You can modify the parameters of the estimators below."),
             
             sliderInput("reg_threshold", label = h4("Official data threshold for regression estimator"), min = 0, max = 100, value = 70, post  = " %"),
             
             radioButtons("reg_type", label = h4("Regression model"), choices = list("Automatic" = 1, "Manual" = 2), selected = 1),
             
             checkboxGroupInput("ind_variables", label = h4("Independent variables for manual regression"), choices = list("Trend" = 1, "Production volume" = 2, "Labour force" = 3), selected = c(1, 2, 3)),
             
             sliderInput("trend_threshold", label = h4("Official data threshold for trend estimator"), min = 0, 
                         max = 100, value = 70, post  = " %"),
             
             numericInput("histaverage_threshold", label = h4("Minimum number of years for historical average"), 
                        value = 5, min = 2, max = 25, step = 1), 
             
             numericInput("histgrowth_threshold", label = h4("Minimum number of years for historical growth"), 
                          value = 5, min = 2, max = 25, step = 1), 
             
             width = 3),
             
           mainPanel(
             tabsetPanel(
               tabPanel("Linear regression",  
                        fluidRow(
                          column(12, plotOutput("reg_summary"), hr()),
                          column(12, plotOutput("reg_variables"), hr()),
                          column(12, uiOutput("reg_results"), hr()),
                          column(12, plotOutput("reg_fit"))
                        )
               ),
               tabPanel("Trend", 
                        fluidRow(
                          column(12, plotOutput("trend_summary"), hr()),
                          column(12, plotOutput("trend_fit"))
                          )
               ),
               tabPanel("Linear interpolation", plotOutput("linearint_summary")),
               tabPanel("Historical average", plotOutput("histavg_summary")),
               tabPanel("Historical growth", plotOutput("histgrowth_summary")),
               tabPanel("Backward dragged", plotOutput("bdragged_summary")),
               tabPanel("Forward dragged", plotOutput("fdragged_summary"))
             )
           )
         )       
         
        ),
        
        # Third tab
        tabPanel("Imputation",
        
         sidebarLayout(
           sidebarPanel(
             helpText("Please select the estimator of your choice for each year with missing data."),
             
             uiOutput("estimator_choices"),
             
             hr(),
             
             helpText("Once you are satisfied with your choices, click the button below."),
             
             actionButton("export_imputed", label = "Export imputed data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             
             width = 3),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Plot", plotOutput("")), 
               tabPanel("Table", tableOutput("")), 
               tabPanel("Summary of changes", htmlOutput(""))
             )
           )
         )       
                 
        )
      
)

# Define server logic ----
server <- function(input, output) {
  
  #####
  # Data import
  
  # Basic FM filter
  
  FM_filtered <- reactive({FMfilter(FMraw = FM_raw, countryinput = input$country, OC1input = input$OC1, startyear = min(input$year_range), endyear = max(input$year_range))})
  
  subseries <- reactive({getsubseries(FMfiltered = FM_filtered())})
  
  # Visualization of current estimates
  
  FM_exisiting_estimates <- reactive({FMexisitingestimates(FMraw = FM_raw, countryinput = input$country, OC1input = input$OC1, startyear = min(input$year_range), endyear = max(input$year_range))})
  
  output$viz_exisiting_estimates <- renderPlot({data_viz(data = FM_exisiting_estimates(), countryinput = input$country, OC1input = input$OC1, title = "Visualization of current estimates")
  })

  # Subseries analysis
  #####
  
  # Identify aggregated Country/sector/year data where official and estimated data are mixed
  
  mixed_flags <- reactive({mixedflags(FMraw = FM_raw, countryinput = input$country, OC1input = input$OC1, startyear = min(input$year_range), endyear = max(input$year_range))})
  
  # Identify missing years
  
  years_data <- reactive({yearsdata(FMfiltered = FM_filtered())})
  years_data_excl_mixed <- reactive({yearsdataexclmixed(yearsdata = years_data(), mixedflags = mixed_flags())})
  years_all <- reactive({yearsall(startyear = min(input$year_range), endyear = max(input$year_range))})
  missing_years <- reactive({missingyears(yearsall = years_all(), yearsdata = years_data())})
  missing_years_series <- reactive({missingyearsseries(missingyears = missing_years())})
  missing_years_incl_mixed <- reactive({missingyearsinclmixed(yearsall = years_all(), yearsdataexclmixed = years_data_excl_mixed())})
  missing_years_series_incl_mixed <- reactive({missingyearsseriesinclmixed(missingyearsinclmixed = missing_years_incl_mixed())})
  
  output$estimator_choices <- renderUI({
    
    lapply(missing_years(), function(i) {
      selectInput(inputId = paste0("estimator_", i), label = h4(paste0("Estimator for year ", i)), choices = list("Select...", "Linear regression" = 1, "Polynomial trend" = 2, "Linear interpolation" = 3, "Historical average" = 4, "Historical growth" = 5, "Backward dragging" = 6, "Forward dragging" = 7)) })
    
  })
  
  # Identify years where the continuity of subseries is interrupted
  
  subseries_break <- reactive({subseriesbreak(yearsdata = years_data(), startyear = min(input$year_range), FMfiltered = FM_filtered())})
  
  # Summarize weight of each subseries for each year
  
  subseries_weights <- reactive({subseriesweights(FMfiltered = FM_filtered(), yearsdataexclmixed = years_data_excl_mixed(), missingyearsseriesinclmixed = missing_years_series_incl_mixed(), yearsdata = years_data())})
  subseries_weights_forward <- reactive({subseriesweightsforward(FMfiltered = FM_filtered(), yearsdataexclmixed = years_data_excl_mixed(), missingyearsseriesinclmixed = missing_years_series_incl_mixed())})
  
  # Aggregate FM data for reg/trend estimates
  
  FM_agg <- reactive({FMagg(FMfiltered = FM_filtered(), yearsdataexclmixed = years_data_excl_mixed())})
  
  # Inspect discontinuities in subseries
  
  discontinuous_subseries <- reactive({discontinuoussubseries(ss = subseries(), FMfiltered = FM_filtered())})
  
  # Generate wide table showing data by subseries
  
  output$subseries_table <- renderDataTable({subseriestable(FMfiltered = FM_filtered(), ss = subseries(), yearsall = years_all())}) 
  
  #####
  # Aggregated estimations
  
  #### LM estimations
  
    #if(length(years_data_excl_mixed()) >= input$reg_threshold) {
      
      # Aggregate datasets
      
      prod_agg <- reactive({
        prodagg(prodraw = prod_raw, countryinput = input$country, OC1input = input$OC1, startyear = min(input$year_range), endyear = max(input$year_range))
      }) 
      
      ILO_labor_agg <- reactive({
        ILOlabor_agg(ILOlaborraw = ILO_labor_raw, countrynames = country_names, countryinput = input$country, targetlaborclassif1 = target_labor_classif1, startyear = min(input$year_range), endyear = max(input$year_range))
      }) 
      
      # Generate dataset for regression
      
      reg_data <- reactive({
        regression_data(yearsall = years_all(), FMagg = FM_agg(), ILOlaboragg = ILO_labor_agg(), prodagg = prod_agg())
      }) 
      
      # Linear regression summary plots
      
      output$reg_variables <- renderPlot({reg_variables_viz(regdata = reg_data(), startyear = min(input$year_range), endyear = max(input$year_range), OC1input = input$OC1, countryinput = input$country)})
      
      # Regression

      reg_result <- reactive({
        
        if (input$reg_type == 1) {

          reg_automatic(regdata = reg_data(), startyear = min(input$year_range), endyear = max(input$year_range))
  
        } else if (input$reg_type == 2) {

          reg_manual(regdata = reg_data(), startyear = min(input$year_range), manual_model = reg_dynamic)

        }
        
      })

      # Regression results

      output$reg_results <- renderUI(HTML({reg_result_summary(regresult = reg_result())}))

      # Determine model with best fit

      best_fit_LM <- reactive({
        
        if (input$reg_type == 1) {
  
          reg_auto_best_fit(regresult = reg_result())
  
        } else if (input$reg_type == 2) {

          1

        }
        
      })

      # Get adj-R2 for model with best fit

      best_fit_LM_R2 <- reactive({reg_auto_best_R2(regresult = reg_result(), bestfit = best_fit_LM())})

      # Add predictions of best LM model to data

      predictions <- reactive({predict_reg(regresult = reg_result(), bestfit = best_fit_LM(), regdata = reg_data(), startyear = min(input$year_range))})

      reg_predictions <- reactive(cbind(reg_data(), predictions()))
      
      # Visualize fit of LM over data

      output$reg_fit <- renderPlot({
        
        if (input$reg_type == 1) {
  
          reg_fit_viz(regdata = reg_predictions(), bestfit = best_fit_LM(), startyear = min(input$year_range), endyear = max(input$year_range), countryinput = input$country, OC1input = input$OC1, best_R2 = best_fit_LM_R2())
  
        } else if (input$reg_type == 2) {

          reg_fit_viz_manual(regdata = reg_predictions(), startyear = min(input$year_range), endyear = max(input$year_range), countryinput = input$country, OC1input = input$OC1, best_R2 = best_fit_LM_R2())

        }
        
      })

      # Save predictions as dataframe

      reg_estimates_agg <- reactive({reg_estimator(regdata = reg_predictions())})

      # Disaggregate estimates

      reg_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = reg_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Linear regression estimate")})

    #}
      
  #### Trend estimations
      
      # Fit polynomial trend to data 
      
      trend_predictions_agg <- reactive({trend_fit(FMagg = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), yearsall = years_all(), startyear = min(input$year_range))})
      
      # Show a plot of the fitted trend and the data
      
      output$trend_fit <- renderPlot({trend_fit_viz(trenddata = trend_predictions_agg(), startyear = min(input$year_range), endyear = max(input$year_range), countryinput = input$country, OC1input = input$OC1)})
      
      # Generate estimates for trend
      
      trends_estimates_agg <- reactive({trend_estimator(trenddata = trend_predictions_agg())})
      
      # Disaggregate estimates
      
      trend_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = trends_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = paste0("Polynomial trend estimate (", trend_predictions_agg()$trend_type[1], ")"))})
      
  #### Backward dragged estimates computation
      
      bdragged_estimates_agg <- reactive({bdragged_estimator(FMagg = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), yearsall = years_all())})
      
      # Disaggregate bdragged estimates
      
      bdragged_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = bdragged_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Backward dragged estimate")})
      
      
  #### Forward dragged estimates computation
      
      fdragged_estimates_agg <- reactive({fdragged_estimator(data = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), all_years = years_all())})
      
      # Disaggregate fdragged estimates
      
      fdragged_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights_forward(), mflags = mixed_flags(), estimatesagg = fdragged_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Forward dragged estimate")})
      
      
  #### Linearly interpolated estimates computation
      
      linearint_estimates_agg <- reactive({linearint_estimator(data = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), all_years = years_all())})
      
      # Disaggregate linearint estimates
      
      linearint_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = linearint_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Linear interpolation estimate")})
      
      
  #### Historical average computation (aggregated, last X years)
      
      histavg_estimates_agg <- reactive({histavg_estimator(data = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), all_years = years_all(), threshold = input$histaverage_threshold)})
      
      # Disaggregate histavg estimates
      
      histavg_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = histavg_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Historical average estimate")})
      
      
  #### Historical growth computation (aggregated, last X years)
      
      histgrowth_estimates_agg <- reactive({histgrowth_estimator(data = FM_agg(), yearsdataexclmixed = years_data_excl_mixed(), all_years = years_all(), threshold = input$histgrowth_threshold)})
      
      # Disaggregate histgrowth estimates
      
      histgrowth_estimates_disag <- reactive({estimates_disaggregation(weights = subseries_weights(), mflags = mixed_flags(), estimatesagg = histgrowth_estimates_agg(), countryinput = input$country, OC1input = input$OC1, FMfiltered = FM_filtered(), missingyears = missing_years_incl_mixed(), comment = "Historical growth estimate")})
      
  #### Summary of aggregated estimations
      
      # Create estimates datasets for each subseries
      
      final_estimates <- reactive({estimates_table(FMfiltered = FM_filtered(), reg_estimates = reg_estimates_disag(), trend_estimates = trend_estimates_disag(), linearint_estimates = linearint_estimates_disag(), histavg_estimates = histavg_estimates_disag(), histgrowth_estimates = histgrowth_estimates_disag(), bdragged_estimates = bdragged_estimates_disag(), fdragged_estimates = fdragged_estimates_disag())})
      
      # Create rainbow visualization for each estimator type
      
      output$reg_summary  <- renderPlot({estimator_viz(estimator = "reg", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$trend_summary  <- renderPlot({estimator_viz(estimator = "trend", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$linearint_summary  <- renderPlot({estimator_viz(estimator = "linearint", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$histavg_summary  <- renderPlot({estimator_viz(estimator = "histavg", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$histgrowth_summary  <- renderPlot({estimator_viz(estimator = "histgrowth", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$bdragged_summary  <- renderPlot({estimator_viz(estimator = "bdragged", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
      output$fdragged_summary  <- renderPlot({estimator_viz(estimator = "fdragged", dataset = final_estimates(), countryinput = input$country, OC1input = input$OC1)})
}

# Run the app ----
shinyApp(ui = ui, server = server)