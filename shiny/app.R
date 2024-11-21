#####################
# FIM-teractive App #
#####################

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

# Source the UI and Server
source("shiny/server.R")
source("shiny/ui.R")


# Run the application
shinyApp(ui = ui, server = server)



