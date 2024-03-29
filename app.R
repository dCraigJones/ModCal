#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(shiny)
library(shinythemes)
#library(shinyFiles)
library(DT)
library(htmltools)

source("./R/utils.R")
source("./R/def.R")
#source("./R/modules.R")

ui <- navbarPage(paste("ModCal v0.1 (", file_info$plant_basin, ")"), id="main"
                 , theme = shinytheme("cosmo")

                 , tabPanel("Overview", value="tab_overview"
                    , DTOutput("omt_overview")  
                 )   
                 
                 , tabPanel("Individual", value="tab_indiv"
                          
                            , sidebarLayout(
                              sidebarPanel(
                                  #selectInput("iss_calibration_run", "Calibration Run: ", calibration_events)
#                                , br()
                                  #HTML(paste("<H2>", file_info$plant_basin, "</H2>"))
                                #, hr()
                                  selectizeInput("iss_pumpstation", "Pumpstation", tbl_info$address[order(tbl_info$address)])
                                , actionButton("iss_action", "Copy to Clipboard")
                                , hr()
                                , selectInput("iss_comment", "Comment: ", comment_options)
                                , checkboxInput("isc_approved", "Approve", FALSE)
                                , textInput("ist_action", "Action:")
                                , hr()
                                , HTML(paste("<i>Simulation Date:", file_info$date_created, "</i>"))
                                , textOutput("qa")
                              ) # sidebarPanel
                              
                              , mainPanel(
                                
                                tabsetPanel(
                                    tabPanel("Timeseries", plotOutput("imp_timeseries"))
                                  , tabPanel("Control", plotOutput("imp_control"))
                                  , tabPanel("Dbl-Mass", plotOutput("imp_dblmass"))
                                  , tabPanel("Source", plotOutput("imp_source"))
                                ) # tabsetPanel
                                
                                , DTOutput("imp_summary")
                                
                              ) # mainPanel
                              
                            ) # sidebarLayout
                            
                 ) # tabPanel - Individual
                 
                 , tabPanel("Review",
                      DTOutput("omt_review")
                            
                 ) # tabPanel - Review
                 
                 , navbarMenu("File..."
                              , tabPanel("New", 
                                  fileInput("new_file", "Select Simulation Result File...",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  )
                                  , textInput("new_date", "Enter Simulation Date/Time", as.character(format(Sys.time(), "%F %I:%M %p")))
                                  , selectInput("new_basin", "Select Wastewater Plant Basin", wastewater_plants)
                                  , actionButton("new_ok", "GO!")
                                  , textOutput("new_msg")
                              ) #Tab Panel - NEW
                              , tabPanel("Open",
                                 fileInput("open_file", "Open ModCal File...",
                                           accept = c(
                                             ".hrt")
                                 )
                                         
                              ) #Tab Panel - OPEN
                              
                              , tabPanel("Save", 
                                      textInput("save_filename", "Enter Proposed Filename...", suggested_filename)
                                    , actionButton("save_ok", "Save")
                                    , hr()
                                    , span("file will by saved in the \"save\" folder of the project directory")
                                    , textOutput("save_msg")
                              )
                              
                              , tabPanel("Export" , 
                                   textInput("export_filename", "Enter Proposed Filename...")
                                   , actionButton("export_ok", "Export")
                                   , hr()
                                   , span("file will by saved in the xyz folder")
                                   
                              ) #Tab Panel - EXPORT
                 )
               

) # navbarPage


server <- function(input, output, session) { 
  
filename <- reactive ({ input$export })
  
output$export_qa <- renderText(filename()$datapath)
  
# Reactive Variables/Events -----------------------------------------------

  index <- reactive( match( input$iss_pumpstation, tbl_info$address ) )
  
  observeEvent( input$iss_action ,  writeClipboard(tbl_info$address[index()]))

  #set_approved()
  approval <- reactive({ input$isc_approved })

  observeEvent( approval() , {

    tbl_info$approved[ index() ] <<- approval()
    save(tbl_info, file="./data/info.RData")

  })
  
  comment <- reactive({ input$iss_comment })
  
  observeEvent( comment(), {
    
    tbl_info$comment[ index() ] <<- comment()
    save(tbl_info, file="./data/info.RData")
    
  })
  
  action <- reactive({input$ist_action})
  
  observeEvent( action(), {
    
    tbl_info$action[ index() ] <<- action()
    save(tbl_info, file="./data/info.RData")
    
  })
  
  
  
  observeEvent( index(), {
    
    updateSelectInput( session, "iss_comment", selected=tbl_info$comment[ index() ] )
    updateCheckboxInput( session, "isc_approved", value=tbl_info$approved[ index() ])
    updateTextInput( session, "ist_action", value=tbl_info$action[ index() ])
    #output$qa <- renderText( paste0(hrt %>% filter(cmms==tbl_info$cmms[ index() ]) %>% ungroup() %>% distinct(address)) )
    
    
  })
  
  
  
  selected <- reactive({ input$omt_overview_rows_selected })
  
  observeEvent( selected(), {
    
    rowVal <- selected()
    rowValLast <- rowVal[length(rowVal)]
    
    updateTabsetPanel( session, "main", "tab_indiv" )
    updateSelectizeInput( session, "iss_pumpstation", selected=tbl_info$address[ rowValLast ] )
    
  })
  

# Output Plots/Tables -----------------------------------------------------

  output$imp_timeseries <- renderPlot({
    tbl_ts %>% 
      dplyr::filter(cmms==tbl_info$cmms[index()]) %>%
      ggplot(aes(x=hour, y=q2)) + 
      geom_line(col="grey50", alpha=0.5, lwd=1) + 
      geom_errorbar(aes(ymin=q1, ymax=q3), col="grey50", alpha=0.35, lwd=1) +
      geom_line(aes(y=hrt), lwd=1, col="black") +
      geom_point(aes(y=hrt), size=3, col="black", fill="white", pch=21) +
      labs(x="hour of day", y="runtime (minutes/hour)")
  }) # output$imp_timeseries
  
  output$imp_control <- renderPlot({
    tbl_ts %>% 
      dplyr::filter(cmms==tbl_info$cmms[index()]) %>%
      ggplot(aes(x=hour, y=z)) +
      geom_line(lwd=1, col="black") +
      geom_point(size=3, col="black", fill="white", pch=21) +
      geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
      geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
      geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
      geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
      xlab("hour of day") +
      ylab("z-score")
  }) # output$imp_control
  
  output$imp_dblmass <- renderPlot({
    tbl_ts %>% 
      filter(cmms==tbl_info$cmms[index()]) %>% 
      ungroup() %>%
      mutate(cm=cumsum(hrt)
             , c1=cumsum(q1)
             , c2=cumsum(q2)
             , c3=cumsum(q3)
      ) %>% 
      ggplot(aes(x=cm, ymin=c1, ymax=c3)) +
      geom_linerange(lwd=2, col="grey50") +
      geom_point(aes(y=c2), pch=21, size=3, fill="white") +
      geom_abline(slope=1, lwd=1, col="grey25") +
      geom_smooth(aes(x=cm, y=c2), method=lm, fill=NA, lwd=0.75, lty=2) + 
      xlab("model (cumulative runtime)") +
      ylab("field (cumulative runtime)") +
      xlim(0,NA) + ylim(0,NA)
  })
  
  output$imp_source <- renderPlot({
    hrt %>% 
      dplyr::filter(cmms==tbl_info$cmms[index()]) %>% 
      ggplot(aes(x=datetime, y=runtime)) + 
      geom_line()
    
    
  }) # output$imp_source
  
  output$imp_summary <- DT::renderDT(tbl_info %>% 
     dplyr::filter(cmms==tbl_info$cmms[index()]) %>%
     ungroup() %>%   
     select(cmms, RT, MPE, NSE, mu_z, sd_z, beta, RMS)
     , selection="none"
     , rownames=FALSE
     , options=list(dom='t')
     , container=summary_sketch
  )
  
  output$omt_overview <- DT::renderDataTable(tbl_info[,c(1:4,9,6:8,5)]
     , selection="single"
     , rownames=FALSE
     , filter="bottom"
     , container=sketch
  )
  
  #x = tbl_info %>% select(cmms, address, approved, comment, action)
  
  output$omt_review = renderDT(
       tbl_info %>% 
         select(cmms, address, approved, comment, action)
     , selection = 'none'
     , editable = TRUE
     , rownames=FALSE
     , filter="bottom"
  )
  
  proxy = dataTableProxy('omt_review')
  
  observeEvent(input$omt_review_cell_edit, {
    x = tbl_info #%>% select(cmms, address, approved, comment, action)
    
    info = input$omt_review_cell_edit
    i = info$row
    j = info$col
    v = info$value
    
    isolate(
      if (j == 7 ) {
        
        x[i, j] <<- v #DT::coerceValue(v, x[i, j])
        tbl_info <<- x
        save(tbl_info, file="./data/info.RData")
    })

    replaceData(proxy, x, resetPaging = FALSE)  # important
  })

  observeEvent( input$new_ok, {
    inFile <- input$new_file

    if(is.null(inFile))
       {output$new_msg <- renderText( "Select model output csv file!" )}
    else {
        load_new_file(inFile$datapath)

        file_info <<- data.frame(date_created=input$new_date, plant_basin=input$new_basin)

        save(file_info, file="./data/file_info.RData")

        updateTabsetPanel( session, "main", "tab_overview" )
    }
  })
  
  observeEvent( input$open_file, {
    inFile <- input$open_file

        load(inFile$datapath, envir=.GlobalEnv)
        
        save(tbl_ts, file="./data/ts.RData")
        save(tbl_info, file="./data/info.RData")
        save(file_info, file="./data/file_info.RData")
        
        updateTabsetPanel( session, "main", "tab_overview" )
    
  })
  
  
  observeEvent( input$save_ok, {
    save(tbl_ts, tbl_info, file_info, 
         file=paste0("./save/", input$save_filename, ".hrt")
         )
    
    output$save_msg <- renderText( paste0("ModCal files saved to ../save/", input$save_filename, ".hrt") )
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)


# README - Naming Documentation -------------------------------------------

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