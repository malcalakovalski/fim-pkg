
# Load the necessary packages 
library(shiny)
library(readxl)
library(writexl)
library(lubridate)
library(zoo)
library(shinyjs)
library(rsconnect)
library(shinycssloaders)


# Connect to Hutchins Shiny.io account 
# This is how we share the FIM App as a clickable link. 
rsconnect::setAccountInfo(name='hutchins',
                          token='1A972D7D4A560925E0382145E6EF8A1E',
                          secret='ycz0u60XaZqGvSk/akHvL/Q9PAOryj4pyMykKk/a')

#------- Load the FIM Data ---------# 

# Read in the forecast sheet data 
data <- readxl::read_xlsx('shiny/cache/forecast.xlsx')

# Read in the Hutchins Center FIM Output (we use this to create the final chart, which compares the user's results with ours)
load('shiny/cache/hutchins_fim.rda') 

# Read in the National Accounts data and historical overrides 
load('shiny/cache/usna.rda')
load('shiny/cache/historical_overrides.rda')

# Set the Current Quarter 
current_quarter <- yearquarter(Sys.Date()) %>% yearquarter()
current_quarter <- current_quarter - 1

# Source the Contributions R Script, which defines the functions that are used to calculate the FIM. 
source("shiny/shiny_contributions.R")

# Define Server Logic 
server <- function(input, output) {
  
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_non_corporate_taxes.rds"), 
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
      mpc_matrix =  readRDS("shiny/cache/mpc/state_non_corporate_taxes.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_corporate_taxes.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/state_corporate_taxes.rds"),
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
      mpc_matrix =readRDS("shiny/cache/mpc/federal_social_benefits.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/state_social_benefits.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/rebate_checks.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/rebate_checks_arp.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_ui.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/state_ui.rds"), 
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_subsidies.rds"), 
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_aid_to_small_businesses_arp.rds"),
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_other_direct_aid_arp.rds"), 
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
      mpc_matrix = readRDS("shiny/cache/mpc/federal_other_vulnerable_arp.rds"), 
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
      mpc_matrix =  readRDS("shiny/cache/mpc/federal_student_loans.rds"), 
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
      mpc_matrix =  readRDS("shiny/cache/mpc/state_subsidies.rds"), 
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
      mpc_matrix =  readRDS("shiny/cache/mpc/federal_health_outlays.rds"), 
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
      mpc_matrix = readRDS("shiny/cache/mpc/state_health_outlays.rds"), 
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
    )
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
  
  # Create Plot 
  output$barPlot <- renderPlot({
    req(fiscal_impact_measure())
    
    plot_data_long <- fiscal_impact_measure() %>% 
      filter(date < current_quarter + 8) %>% 
      filter(date >= yearquarter("2015 Q1")) %>% 
      pivot_longer(cols = c(user_fim, hutchins_fim), 
                   names_to = "Variable", 
                   values_to = "Value") 
    
    ggplot(plot_data_long, aes(x = date, y = Value, fill = Variable)) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      labs(title = "Your Fiscal Impact Measure", 
           x = "Date") +
      scale_fill_manual(values = c("#003A70", "#FF9E1B"), 
                        labels = c("Hutchins Center FIM", "Your FIM")) +
      scale_x_yearquarter(breaks = waiver(),
                          date_breaks = '3 months',
                          date_labels = "Q%q") +
      facet_grid( ~ year(date),
                  space = "free_x",
                  scale = "free_x",
                  switch = "x")  +
      scale_y_continuous(labels = function(x) paste0(x, "%")) + 
      theme(
        # Format Legend 
        legend.position = "top",
        legend.text = element_text(size = 14), 
        legend.title = element_blank(),
        
        # Format Axis Text and Labels
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 11, color = "black", face = "bold"), 
        axis.text.y = element_text(size = 14, color = "black", face = "bold"),
        strip.text.x = element_text(size = 14, color = "black"),
        
        
        # Background Colors 
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
        
        
        )
  
    
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
  
  # Define results loaded reactive function
  resultsLoaded <- reactiveVal(FALSE) # define a reactive called resultsLoaded
  observeEvent(input$file, {
    # Mark the results as loaded
    resultsLoaded(TRUE)
  })
  
  # Define plot title 
  output$results_plotTitle <- renderUI({
    if (resultsLoaded()) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Your Fiscal Impact Measure")
    } else {
        NULL
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

# Define UI 
ui <- fluidPage(
  
  # App title
  titlePanel("Hutchins Center FIM Interactive"),
  helpText("The Hutchins Center FIM translates changes in taxes and spending at federal, state, 
           and local levels into changes in aggregate demand, illustrating the effect of fiscal policy on real GDP growth.
           We estimate the future path of the FIM based on forecasts for major tax and spending categories produced by 
           the Congressional Budget Office, as well as our own assumptions about future fiscal policy. This interactive app 
           allows you to input your own values for each of our primary variables and then calculates the FIM for our eight-quarter forecast period 
           based on your inputs. "),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      helpText('Click the button below to download our forecasts and MPCs for each of the primary FIM inputs. 
               You can overwrite our numbers with your own. It is important that you edit only the values. 
               Do not change the file structure or variable names.'),
      
      # Button to download the data
      downloadButton("downloadData", "Download"),
      
      # Display text directing users to re-upload their forecasts 
      helpText("Re-upload the Excel file with your own forecasts. Your results will be displayed to the right."),
      
      # Generate file input for uploading excel data
      fileInput("file", label = NULL, accept = c(".xlsx")),  # Ensure .xlsx is specified
      
      # Display text directing user to download contributions
      helpText("Download an Excel file breaking down 
                the contributions of each of your inputs to the total FIM. 
               You must upload your data in order to access this file."), 
      
      useShinyjs(),
      # Button to download contributions 
      downloadButton("downloadContributions", "Download Contributions"), 
      
      
      # Set width
      width = 3
    ),
    
    # Main panel with spinner and plot output
    mainPanel(
      uiOutput("results_plotTitle"),
      withSpinner(plotOutput("barPlot", width = "1100px", height = "800px"), type = 1, color = "gray"),
      
      uiOutput("results_Title"),
      uiOutput("results_helpText"),
      tableOutput("dataTable")
    )
  )
)


# Run the application
shinyApp(ui = ui, server = server)


