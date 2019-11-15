#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(shiny)
library(shinythemes)
library(DT)

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

ui <- navbarPage("ModCal v0.1", id="main"
                 , theme = shinytheme("cosmo")
                 
                 , tabPanel("Overview", value="tab_overview"
                    , DTOutput("omt_overview")  
                 )   
                 
                 , tabPanel("Individual", value="tab_indiv"
                            
                            # Sidebar with a slider input for number of bins 
                            , sidebarLayout(
                              sidebarPanel(
                                  selectInput("iss_calibration_run", "Calibration Run: ", calibration_events)
                                , hr()
                                #, selectInput("iss_basin", "Selection Basin: ", c("All"))
                                , selectizeInput("iss_pumpstation", "Pumpstation", sw$address[order(sw$address)])
                                , hr()
                                #, checkboxInput("isc_investigate", "Investigate", FALSE)
                                , selectInput("iss_comment", "Comment: ", comment_options)
                                , checkboxInput("isc_approved", "Approve", FALSE)
                                , hr()
                                , radioButtons("isr_view", "Filter", filter_options)
                                #, textInput("ist_comment", "Comment")
                                #, submitButton("Submit")
                                , textOutput("qa")
                              ) # sidebarPanel
                              
                              , mainPanel(
                                tabsetPanel(
                                  tabPanel("Timeseries", plotOutput("imp_timeseries"))
                                  , tabPanel("Control", plotOutput("imp_control"))
                                  , tabPanel("Source", plotOutput("imp_source"))
                                ) # tabsetPanel
                                
                              ) # mainPanel
                              
                            ) # sidebarLayout
                            
                 ) # tabPanel - Individual
                 
                 , navbarMenu("Select Plant..."
                              , tabPanel("Buckman")
                              , tabPanel("Cedar Bay")
                 ) # navbarMenu
                 
) # navbarPage


server <- function(input, output, session) { 
  
  index <- reactive( match( input$iss_pumpstation, sw$address ) )
  
  approval <- reactive({ input$isc_approved })
  
  observeEvent( approval() , {
    
    sw_tmp$approved[ index() ] <<- approval()
    save(sw_tmp, file="./data/sw_tmp.RData")
    
  })
  
  comment <- reactive({ input$iss_comment })
  
  observeEvent( comment(), {
    
    sw_tmp$comment[ index() ] <<- comment()
    save(sw_tmp, file="./data/sw_tmp.RData")
    
  })
  
  observeEvent( index(), {
    
    updateSelectInput( session, "iss_comment", selected=sw_tmp$comment[ index() ] )
    updateCheckboxInput( session, "isc_approved", value=sw_tmp$approved[ index() ])
    
  })
  
  selected <- reactive({ input$omt_overview_rows_selected })
  
  observeEvent( selected(), {
    
    rowVal <- selected()
    rowValLast <- rowVal[length(rowVal)]
    
    updateTabsetPanel( session, "main", "tab_indiv" )
    updateSelectizeInput( session, "iss_pumpstation", selected=error$address[ rowValLast ] )
    
  })
  
  output$imp_timeseries <- renderPlot({
    tmp %>% 
      dplyr::filter(cmms==sw$cmms[index()]) %>%
      ggplot(aes(x=hour, y=mu)) + 
      geom_line(col="grey50") + 
      geom_errorbar(aes(ymin=q1, ymax=q3), col="grey50") +
      geom_line(aes(y=hrt), lwd=1) +
      geom_point(aes(y=hrt), size=3) 
  }) # output$imp_timeseries
  
  output$imp_control <- renderPlot({
    tmp %>% 
      dplyr::filter(cmms==sw$cmms[index()]) %>%
      ggplot(aes(x=hour, y=z)) +
      geom_line(lwd=1) + 
      geom_point(size=3) + 
      geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
      #geom_hline(yintercept=-3:5, col=rgb(1,0,0,0.25), lwd=0.5) +
      geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
      geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
      geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
      ylab("z")
  }) # output$imp_control
  
  output$imp_source <- renderPlot({
    hrt %>% 
      dplyr::filter(cmms==sw$cmms[index()]) %>% 
      ggplot(aes(x=datetime, y=runtime)) + 
      geom_line()
    
    
  }) # output$imp_source
  
  output$omt_overview <- renderDT({ error })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

