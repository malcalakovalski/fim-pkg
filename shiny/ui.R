#############
# Define UI #
#############

# Load the necessary packages 
library(shinyjs)
library(rsconnect)
library(shinycssloaders)
library(plotly)


ui <- fluidPage(
  
  # Add reference to CSS file 
  includeCSS("www/aesthetics.css"),
  
  # App title
  titlePanel("Hutchins Center FIM Interactive", windowTitle = "Hutchins Center FIM Interactive"),
  helpText(style = "font-size: 14px; color: #676666; text-align: left;
           margin-top: 10px; margin-bottom: 15px; margin-left: 30px; margin-right: 30px", 
           "The Hutchins Center Fiscal Impact Measure (FIM) translates changes in taxes and spending at federal, state, 
           and local levels into changes in aggregate demand, illustrating the effect of fiscal policy on real GDP growth.
           We estimate the future path of the FIM based on forecasts for major tax and spending categories produced by 
           the Congressional Budget Office, as well as our own assumptions about future fiscal policy. This interactive app 
           allows you to input your own values for each of our primary variables and then calculates the FIM for our eight-quarter forecast period 
           based on your inputs. "),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      helpText(style = "font-size: 14x; color: #676666; text-align: left;
               margin-top: 0px; margin-bottom: 10px; margin-left: 5px; margin-right: 5px", 
               'Click the button below to download our forecasts and MPCs for each of the primary FIM inputs. 
               You can overwrite our numbers with your own. It is important that you edit only the values. 
               Do not change the file structure or variable names.'),
      
      # Button to download the data
      downloadButton("downloadData", "Download", style = "font-size: 14px"),
      
      # Display text directing users to re-upload their forecasts 
      helpText(style = "font-size: 14px; color: #676666; text-align: left;
               margin-top: 20px; margin-bottom: 10px; margin-left: 5px; margin-right: 5px", 
               "Re-upload the Excel file with your own forecasts. Your results will be displayed to the right."),
      
      # Generate file input for uploading excel data
      fileInput("file", label = NULL, accept = c(".xlsx")), # Ensure .xlsx is specified
      
      # Display text directing user to download contributions
      helpText(style = "font-size: 14px; color: #676666; text-align: left;
               margin-top: 20px; margin-bottom: 10px; margin-left: 5px; margin-right: 5px", 
               "Download an Excel file breaking down 
                the contributions of each of your inputs to the total FIM. 
               You must upload your data in order to access this file."), 
      
      useShinyjs(),
      # Button to download contributions 
      downloadButton("downloadContributions", "Download Contributions", style = "font-size: 14px"), 
      
      
      # Set width
      width = 3
    ),
    
    # Main panel with spinner and plot output
    mainPanel(
      uiOutput("results_plotTitle"),
      withSpinner(plotlyOutput("fimPlot", width = "100%", height = "auto"), 
                  type = 1, color = "gray"),
      
      uiOutput("results_Title"),
      uiOutput("results_helpText"),
      tableOutput("dataTable")
    )
  )
)
