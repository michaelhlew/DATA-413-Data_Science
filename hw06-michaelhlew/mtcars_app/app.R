#Michael Lewis, Shiny App for mtcars
library(shiny)
library(tidyverse)
data("mtcars")

ui <- fluidPage(

    # Application title
    titlePanel("Shiny App For EDA of mtcars Data"),
    
    #variable select
    varSelectInput("Xvar", "X Variable", data = mtcars),
    
    radioButtons("pt_plot", "Choose a plot type:", c("Density", "Histogram", "Frequency Polygon")),
    plotOutput("plot1")

)

server <- function(input, output) {

output$plot1 <- renderPlot({
  if (input$pt_plot == "Density") {
    ggplot(mtcars, aes(x = !!(input$Xvar))) +
      geom_density(outline.type = "full") +
    theme_bw()
  } 
  
  else if (input$pt_plot == "Histogram") {
    ggplot(mtcars, aes(x = !!(input$Xvar))) + 
      geom_histogram() +
      theme_bw()
  } 
  
  else {
    ggplot(mtcars, aes(x = !!(input$Xvar))) + 
      geom_freqpoly() + 
      theme_bw()
  }
})

}

shinyApp(ui = ui, server = server)
