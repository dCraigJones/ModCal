#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
        selectizeInput("selected_pumpstation", "Pumpstation Address ", error$address[order(error$address)])
        
        , textOutput("TextOut")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"), 
        plotOutput("controlChart"),
        tableOutput("tblOut")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  i <- reactive(match(input$selected_pumpstation, error$address))
  #tmp <- reactive(tmp %>% dplyr::filter(cmms==error$cmms[i()]))
  
  output$tblOut <- renderTable({
    tmp %>% 
      dplyr::filter(cmms==error$cmms[i()])
    
  })
  
  output$TextOut <- renderText({
    i()
    
  })
   
   output$distPlot <- renderPlot({
     tmp %>% 
       dplyr::filter(cmms==error$cmms[i()]) %>%
       ggplot(aes(x=hour, y=mu)) + 
       geom_line(col="grey50") + 
       geom_errorbar(aes(ymin=q1, ymax=q3), col="grey50") +
       geom_line(aes(y=hrt), lwd=1) +
       geom_point(aes(y=hrt), size=3) 
   })
   
   output$controlChart <- renderPlot({
     tmp %>% 
       dplyr::filter(cmms==error$cmms[i()]) %>%
       ggplot(aes(x=hour, y=z)) +
       geom_line(lwd=1) + 
       geom_point(size=3) + 
       geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
       #geom_hline(yintercept=-3:5, col=rgb(1,0,0,0.25), lwd=0.5) +
       geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
       geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
       geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
       ylab("z")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

