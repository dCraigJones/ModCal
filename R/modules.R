

set_approved <- function() {

  approval <- reactive({ input$isc_approved })
  
  observeEvent( approval() , {
    
    sw$approved[ index() ] <<- approval()
    save(sw, file="./data/sw.RData")
    
  })
  
}