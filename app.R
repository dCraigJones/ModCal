library(shiny)
library(shinythemes)

source("./R/utils.R")

### GUI Naming Scheme
#
# i.. = Indivdual
# o.. = Overview
# c.. = Comments?
#
# .s. = Sidebar
# .m. = Main
#
# ..s = SelectInput
# ..c = Checkbox
# ..r = radio buttons
# ..t = text input
# ..b = button
# ..p = plot

ui <- navbarPage("ModCal v0.1"
                 , theme = shinytheme("cosmo")
                 
                 , tabPanel("Overview")   
                 
                 , tabPanel("Individual"
                            
                            # Sidebar with a slider input for number of bins 
                            , sidebarLayout(
                              sidebarPanel(
                                  selectInput("iss_calibration_run", "Calibration Run: ", calibration_events)
                                , hr()
                                , selectInput("iss_basin", "Selection Basin: ", c("All"))
                                , selectizeInput("iss_pumpstation", "Pumpstation", c("a", "b", "c"))
                                , hr()
                                , checkboxInput("isc_approved", "Approve", FALSE)
                                , checkboxInput("isc_investigate", "Investigate", FALSE)
                                , hr()
                                , radioButtons("isr_view", "Filter", filter_options)
                                #, textInput("ist_comment", "Comment")
                                #, submitButton("Submit")
                              ) # sidebarPanel
                              
                              , mainPanel(
                                tabsetPanel(
                                  tabPanel("Timeseries", plotOutput("imp_timeseries"))
                                  , tabPanel("Control")
                                  , tabPanel("Source")
                                ) # tabsetPanel
                                
                              ) # mainPanel
                              
                            ) # sidebarLayout
                            
                 ) # tabPanel - Individual
                 
                 , navbarMenu("Select Plant..."
                              , tabPanel("Buckman")
                              , tabPanel("Cedar Bay")
                 ) # navbarMenu
                 
) # navbarPage


server <- function(input, output) { 
  output$imp_timeseries <- renderPlot({
    plot(1,1,type="n")
  }) # output$imp_timeseries
  
}

# Run the application 
shinyApp(ui = ui, server = server)

