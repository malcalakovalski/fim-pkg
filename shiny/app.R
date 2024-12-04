#####################
# FIM-teractive App #
#####################

library(shiny)

# Connect to Hutchins Shiny.io account 
# This is how we share the FIM App as a clickable link. 
rsconnect::setAccountInfo(name='hutchins',
                          token='1A972D7D4A560925E0382145E6EF8A1E',
                          secret='ycz0u60XaZqGvSk/akHvL/Q9PAOryj4pyMykKk/a')


# Source the UI and Server
source("shiny/server.R")
source("shiny/ui.R")


# Run the application
shinyApp(ui = ui, server = server)

rsconnect::deployApp("shiny/", appName = "fim_interactive")
