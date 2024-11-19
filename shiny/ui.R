
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
