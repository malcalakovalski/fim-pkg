#######################
# Define Server Logic #
#######################
library(shinyjs)
library(rsconnect)
library(shinycssloaders)
library(plotly)
library(readxl)
library(shiny)
library(writexl)
library(readxl)
library(glue)
library(zoo)
library(lubridate)
library(tsibble)
library(tidyr)

source('shiny_functions.R')



#------- Load the FIM Data ---------# 
# Read in the forecast sheet data 
data <- readxl::read_xlsx('cache/forecast.xlsx')

# Read in the Hutchins Center FIM Output (we use this to create the final chart, which compares the user's results with ours)
load('cache/hutchins_fim.rda') 

# Read in the National Accounts data and historical overrides 
load('cache/usna.rda')
load('cache/historical_overrides.rda')

# Set the Current Quarter 
current_quarter <- yearquarter(Sys.Date()) %>% yearquarter()
current_quarter <- current_quarter - 1

# Source the Contributions R Script, which defines the functions that are used to calculate the FIM. 
source("shiny_contributions.R")



server <- function(input, output, session) {
  
  
  
  # Download handler for the Excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fim_data_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data, file)  
    }
  )
  
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)  
    read_xlsx(input$file$datapath)
  })
  
  # Reactive expression to clean the uploaded data
  forecast_user <- reactive({
    req(uploaded_data())
    
    uploaded_data() %>%
      select(-name) %>%  # Remove the 'name' column from the data
      pivot_longer(-variable, names_to = 'date') %>%  # Reshape the data
      pivot_wider(names_from = 'variable', values_from = 'value') %>%
      mutate(date = yearquarter(date)) %>%  # Convert date to year-quarter format
      tsibble::as_tsibble(index = date)
  })
  
  # Create projections dataset
  projections <- reactive({
    req(forecast_user())
    ui_forecast <- data.frame(forecast_user())
    
    # Join the NIPAs (contained in the cache folder) with the user in
    coalesce_join(usna, ui_forecast, by = 'date') %>%  # Ensure 'usna' is defined
      mutate(across(where(is.numeric), ~ coalesce(.x, 0)))%>% # Coalesce NA's to 0 for numeric values
      
      # Replace missing values with 0 
      mutate( # Coalesce NA's to 0 for all numeric values 
        across(where(is.numeric),
               ~ coalesce(.x, 0))) %>%
      
      #Define FIM health variables 
      mutate(
        federal_health_outlays = medicare + medicaid_grants,
        state_health_outlays = medicaid - medicaid_grants
      ) %>% 
      
      # apply historical_overrides for ARP 
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   federal_other_direct_aid_arp = historical_overrides$federal_other_direct_aid_arp_override,
                   federal_other_vulnerable_arp = historical_overrides$federal_other_vulnerable_arp_override,
                   federal_social_benefits = historical_overrides$federal_social_benefits_override,
                   federal_aid_to_small_businesses_arp = historical_overrides$federal_aid_to_small_businesses_arp_override) %>% 
      mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
                   federal_corporate_taxes = tail(historical_overrides$federal_corporate_taxes_override, n = 1),
                   state_corporate_taxes = tail(historical_overrides$state_corporate_taxes_override, n = 1)) %>% 
      mutate_where(date == yearquarter("2021 Q1"),
                   federal_social_benefits = federal_social_benefits + 203) %>% 
      mutate_where(date == yearquarter('2021 Q4'),
                   federal_ui = 11, 
                   state_ui = ui - federal_ui) %>%
      #apply historical_overrides for Supply Side IRA
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   supply_side_ira = historical_overrides$supply_side_ira_override) %>%
      #apply historical_overrides for Federal Student Loans
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   federal_student_loans = historical_overrides$federal_student_loans_override)
  })
  
  #####################################
  # CALCULATE THE FIM USING USER DATA #
  #####################################
  
  # Federal Purchases Contribution
  federal_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_purchases,
      mpc_matrix = NULL, 
      dg = data$federal_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp
    )
    
  })
  
  # Consumption Grants Contribution 
  consumption_grants_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$consumption_grants,
      mpc_matrix = NULL, 
      dg = data$consumption_grants_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp 
    )
  })
  
  # Investment Grants Contribution 
  investment_grants_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$investment_grants, 
      mpc_matrix = NULL, 
      dg = data$investment_grants_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # State Purchases Contribution 
  state_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_purchases, 
      mpc_matrix = NULL, 
      dg = data$state_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp 
    )
  })
  
  #Federal Non-Corporate Taxes 
  federal_non_corporate_taxes_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_non_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc/federal_non_corporate_taxes.rds"), 
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # State Non-Corporate Taxes Contribution
  state_non_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_non_corporate_taxes,
      mpc_matrix =  readRDS("cache/mpc/state_non_corporate_taxes.rds"),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # Federal Corporate Taxes Contribution
  federal_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc/federal_corporate_taxes.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Supply Side IRA Contribution 
  supply_side_ira_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$supply_side_ira, 
      mpc_matrix = NULL, 
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp 
    )
  })
  
  # State Corporate Taxes Contribution 
  state_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc/state_corporate_taxes.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp 
    )
  })
  
  # Federal Social Benefits
  federal_social_benefits_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_social_benefits, 
      mpc_matrix =readRDS("cache/mpc/federal_social_benefits.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp 
    )
  })
  
  # State social benefits 
  state_social_benefits_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_social_benefits, 
      mpc_matrix = readRDS("cache/mpc/state_social_benefits.rds"),
      rpgg = data$real_potential_gdp_growth, 
      dg = data$consumption_deflator_growth, 
      gdp = data$gdp
    )
  }) 
  
  # Rebate Checks 
  rebate_checks_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$rebate_checks,
      mpc_matrix = readRDS("cache/mpc/rebate_checks.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Rebate Checks ARP 
  rebate_checks_arp_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$rebate_checks_arp,
      mpc_matrix = readRDS("cache/mpc/rebate_checks_arp.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal UI
  federal_ui_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_ui,
      mpc_matrix = readRDS("cache/mpc/federal_ui.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # State UI Contribution 
  state_ui_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_ui,
      mpc_matrix = readRDS("cache/mpc/state_ui.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Subsidies Contribution 
  federal_subsidies_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_subsidies, 
      mpc_matrix = readRDS("cache/mpc/federal_subsidies.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # Federal Aid to Small Businesses ARP Contribution 
  federal_aid_to_small_businesses_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_aid_to_small_businesses_arp,
      mpc_matrix = readRDS("cache/mpc/federal_aid_to_small_businesses_arp.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Other Direct Aid ARP Contribution
  federal_other_direct_aid_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_other_direct_aid_arp, 
      mpc_matrix = readRDS("cache/mpc/federal_other_direct_aid_arp.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # Federal Other Vulnerable ARP 
  federal_other_vulnerable_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_other_vulnerable_arp,
      mpc_matrix = readRDS("cache/mpc/federal_other_vulnerable_arp.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Student Loans 
  federal_student_loans_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_student_loans,
      mpc_matrix =  readRDS("cache/mpc/federal_student_loans.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # State Subsidies Contribution 
  state_subsidies_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_subsidies,
      mpc_matrix =  readRDS("cache/mpc/state_subsidies.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Health Outlays Contribution 
  federal_health_outlays_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_health_outlays,
      mpc_matrix =  readRDS("cache/mpc/federal_health_outlays.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # State Health Outlays 
  state_health_outlays_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_health_outlays, 
      mpc_matrix = readRDS("cache/mpc/state_health_outlays.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Calculate Federal Contribution 
  federal_contribution <- reactive({
    req(federal_purchases_contribution(), 
        consumption_grants_contribution(), 
        investment_grants_contribution())
    
    sum <- federal_purchases_contribution() + consumption_grants_contribution() + investment_grants_contribution()
  }) 
  
  # Calculate State Contribution 
  state_contribution <- reactive({
    req(state_purchases_contribution(), 
        consumption_grants_contribution(), 
        investment_grants_contribution())
    
    sum <- state_purchases_contribution() - consumption_grants_contribution() - investment_grants_contribution()
  })
  
  # Calculate Taxes Contribution
  taxes_contribution <- reactive({
    req(
      federal_non_corporate_taxes_contribution(), 
      state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution(), 
      supply_side_ira_contribution(), 
      state_corporate_taxes_contribution)
    
    sum <- federal_non_corporate_taxes_contribution() + 
      state_non_corporate_taxes_contribution() +
      federal_corporate_taxes_contribution() + 
      supply_side_ira_contribution() + 
      state_corporate_taxes_contribution()
  })
  
  # Calculate Transfers Contribution 
  transfers_contribution <- reactive({
    req(federal_social_benefits_contribution(), state_social_benefits_contribution(), rebate_checks_contribution(), 
        rebate_checks_arp_contribution(), federal_ui_contribution(), state_ui_contribution(), federal_subsidies_contribution(), 
        federal_aid_to_small_businesses_arp_contribution(), federal_other_vulnerable_arp_contribution(), federal_student_loans_contribution(), 
        state_subsidies_contribution(), federal_health_outlays_contribution(), state_health_outlays_contribution()) 
    
    sum <-   federal_social_benefits_contribution() + 
      state_social_benefits_contribution() + 
      rebate_checks_contribution() + 
      rebate_checks_arp_contribution() + 
      federal_ui_contribution() + 
      state_ui_contribution() + 
      federal_subsidies_contribution() + 
      federal_aid_to_small_businesses_arp_contribution() + 
      federal_other_direct_aid_arp_contribution() + 
      federal_other_vulnerable_arp_contribution() + 
      federal_student_loans_contribution() + 
      state_subsidies_contribution() + 
      federal_health_outlays_contribution() + 
      state_health_outlays_contribution()
  })
  
  # Calculate fim 
  fim <- reactive({ 
    req(transfers_contribution(), taxes_contribution(), federal_contribution(), state_contribution())
    
    sum <- transfers_contribution() + taxes_contribution() + federal_contribution() + state_contribution()
    
  })
  
  # Create Contributions Data Frame 
  contributions <- reactive({
    req(
      federal_purchases_contribution(), consumption_grants_contribution(), 
      investment_grants_contribution(), state_purchases_contribution(), 
      federal_non_corporate_taxes_contribution(), state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution(), supply_side_ira_contribution(), 
      state_corporate_taxes_contribution(), federal_social_benefits_contribution(), 
      state_social_benefits_contribution(), rebate_checks_contribution(), 
      rebate_checks_arp_contribution(), federal_ui_contribution(), 
      state_ui_contribution(), federal_subsidies_contribution(), 
      federal_aid_to_small_businesses_arp_contribution(), 
      federal_other_direct_aid_arp_contribution(), 
      federal_other_vulnerable_arp_contribution(), federal_student_loans_contribution(), 
      state_subsidies_contribution(), federal_health_outlays_contribution(), 
      state_health_outlays_contribution(),
      fim()
    )
    
    data.frame(
      date = as.character(projections()$date), 
      federal_purchases_contribution = federal_purchases_contribution(),
      consumption_grants_contribution = consumption_grants_contribution(), 
      investment_grants_contribution = investment_grants_contribution(), 
      state_purchases_contribution = state_purchases_contribution(), 
      federal_non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution(), 
      state_non_corporate_taxes_contribution = state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution = federal_corporate_taxes_contribution(), 
      supply_side_ira_contribution = supply_side_ira_contribution(), 
      state_corporate_taxes_contribution = state_corporate_taxes_contribution(), 
      federal_social_benefits_contribution = federal_social_benefits_contribution(), 
      state_social_benefits_contribution = state_social_benefits_contribution(), 
      rebate_checks_contribution = rebate_checks_contribution(), 
      rebate_checks_arp_contribution = rebate_checks_arp_contribution(), 
      federal_ui_contribution = federal_ui_contribution(), 
      state_ui_contribution = state_ui_contribution(), 
      federal_subsidies_contribution = federal_subsidies_contribution(), 
      federal_aid_to_small_businesses_arp_contribution = federal_aid_to_small_businesses_arp_contribution(), 
      federal_other_direct_aid_arp_contribution = federal_other_direct_aid_arp_contribution(), 
      federal_other_vulnerable_arp_contribution = federal_other_vulnerable_arp_contribution(), 
      federal_student_loans_contribution = federal_student_loans_contribution(), 
      state_subsidies_contribution = state_subsidies_contribution(), 
      federal_health_outlays_contribution = federal_health_outlays_contribution(), 
      state_health_outlays_contribution = state_health_outlays_contribution(),
      fim()
    ) %>% 
      filter(date > yearquarter("1999 Q4"))
  })
  
  # Get Date
  date <- reactive({
    projections()$date
  })
  
  # Create Plot Data
  fiscal_impact_measure <- reactive({
    req(fim(), date())
    
    data <- data.frame(
      date(), 
      fim(), 
      hutchins_fim$fiscal_impact_measure
    ) %>% 
      rename(
        user_fim = fim..,
        hutchins_fim = hutchins_fim.fiscal_impact_measure, 
        date = date..
      )  
  })
  
  # Define results loaded reactive function
  resultsLoaded <- reactiveVal(FALSE) # define a reactive called resultsLoaded
  observeEvent(input$file, {
    # Mark the results as loaded
    resultsLoaded(TRUE)
  })
  
  # Create FIM Plot in the Main Panel 
  output$fimPlot <- renderPlotly({
    
    if(!resultsLoaded()){
      
    # Define FIM Plot to display initially 
    data <- hutchins_fim %>% 
      filter(date > yearquarter("1999 Q4")) %>%
      filter(date < current_quarter + 9) %>% 
      mutate(date = as.character(date)) 
    
    plot1 <- plot_ly() %>% 
      add_trace(data, x = ~data$date, y = ~data$fiscal_impact_measure, type = "bar",
                name = "Hutchins Center FIM", marker = list(color = "#e4649c"),
                hovertemplate = 'Fiscal Impact: %{y:.2f}%<extra></extra>') %>% 
      add_trace(data, x = ~data$date, y = ~data$fiscal_impact_4q_ma, type = "scatter",
                mode = 'lines+markers',
                name = "4 Quarter Moving Average", marker = list(color = "black"), line = list(color = "black"),
                hovertemplate = 'Four Quarter Moving Average: %{y:.2f}%<extra></extra>') %>% 
      layout(
        # X Axis 
        xaxis = list(
          title = "",
          showspikes = TRUE, 
          spikemode = "across", 
          spikecolor = "black",
          spikethickness = 1,
          spikedash = "solid",
          
          tickmode = 'linear',
          tick0 = "2000 Q1", 
          dtick = 4

          ),
        
        # Y Axis 
        yaxis = list(
          title = "",
          ticksuffix = "%"
        ), 
        
        # Format Hover Line 
        hovermode = "x unified", # displays a single label for all data points that share the same x coordinate
        
        
        # Format Data Label 
        hoverlabel = list(
          bordercolor = 'transparent', # makes the border of the hover label transparent 
          font = list(size = 12)  # Change size of the hover label text
        )
        
      )
    } else {
  
  # Create results plot with user defined inputs 
    
    data <- fiscal_impact_measure() %>% 
      filter(date < current_quarter + 9) %>% 
      filter(date >= yearquarter("2015 Q1")) %>% 
      mutate(date = as.character(date))
    
    plot2 <- plot_ly() %>% 
      add_trace(data, x = ~data$date, y = ~data$user_fim, type = "bar", 
                name = "Your FIM", marker = list(color = "#003A70"),
                hovertemplate = 'Your FIM: %{y:.2f}%<extra></extra>') %>% 
      add_trace(data, x = ~data$date, y = ~data$hutchins_fim, type = "bar", 
                name = "Hutchins FIM", marker = list(color = "#FF9E1B"),
                hovertemplate = 'Hutchins FIM: %{y:.2f}%<extra></extra>') %>% 
      layout(
        
        # X Axis 
        xaxis = list(
          title = "",
          showspikes = TRUE, 
          spikemode = "across", 
          spikecolor = "black",
          spikethickness = 1,
          spikedash = "solid"
          
          ), 
        
        # Y Axis 
        yaxis = list(
          title = "",
          ticksuffix = "%"
          ), 
        
        # Format Hover Line 
        hovermode = "x unified",
        
        # Format Data Label 
        hoverlabel = list(
            bordercolor = 'transparent'
          )
        
        )
  }
        
  })
  
  # Create Table Data
  table_data <- reactive({
    req(date(), fiscal_impact_measure(), transfers_contribution(), taxes_contribution(),
        federal_contribution(), state_contribution())
    
    data <- data.frame(
      fiscal_impact_measure(), 
      federal_contribution(), 
      state_contribution(),
      transfers_contribution(), 
      taxes_contribution()) %>% 
      filter(date <= current_quarter + 8) %>% 
      filter(date >= current_quarter) %>% 
      mutate(date = as.character(date)) %>% 
      select(-hutchins_fim)
    
  })
  
  
  # Create Table 
  output$dataTable <- renderTable({
    req(table_data()) 
    
    data <- table_data()
    colnames(data) <- c("Date", "Your FIM", "Federal Purchases Contribution",
                        "State Purchases Contribution", 
                        "Transfers Contribution",
                        "Taxes Contribution")
    
    data
    
  })
  
  # Define plot title 
  output$results_plotTitle <- renderUI({
    if (resultsLoaded() == TRUE) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Your Fiscal Impact Measure")
    } else if (resultsLoaded() == FALSE) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Hutchins Center Fiscal Impact Measure")
    } else {
      print("")
    }
  })
  
  # Define plot help text 
  output$chart_helpText <- renderUI({
    if(!resultsLoaded()) {
      print("The chart below displays the official Hutchins Center FIM - the contribution
            of federal, state, and local fiscal policy to GDP growth. Use the panel on the
            left hand side of the screen to input your own data and this graph will be regenerated 
            based on your inputs.")
    }
  })
  
  # Define table help text
  # Defines help text describing the contents of the summary table that displays only when the results have loaded
  output$results_helpText <- renderUI({
    if (resultsLoaded()) {
      # Show the help text only if results are loaded
      print("The table below summarizes your results. 
        It indicates the contribution to the FIM from taxes, transfers, and purchases.")
    } else {
      NULL
    }
  })
  
  # Table Title
  # Defines a table  title that displays only when the results have loaded 
  output$results_Title <- renderUI ({
    if (resultsLoaded()) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Results Summary")
    } else {
      NULL
    }
  })
  
  # Reactive Allowing User to Download Contributions 
  
  output$downloadContributions <- downloadHandler(
    filename = function() {
      paste("fim_contributions_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(contributions(), file)  
    }
  )
  
  # Disable the contributions download button initially
  shinyjs::disable("downloadContributions")
  
  # Observe file upload and enable the button if a file is uploaded
  observeEvent(input$file, {
    shinyjs::enable("downloadContributions")
  })
  
}
