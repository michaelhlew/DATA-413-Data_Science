#Michael Lewis, Shiny App for mpg
library(shiny)
library(tidyverse)
data("mpg")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Shiny App For EDA of the MPG dataset"),
  #variable select
  varSelectInput("Xvar", "X Variable", data = mpg),
  varSelectInput("Yvar", "Y Variable", data = mpg),
  varSelectInput("categorical", "Color Variable (Categorical)", data = mpg),
  plotOutput("plot"),

)


  server <- function(input, output) {
    output$plot <- renderPlot({
      ggplot(mpg, aes(x = !!(input$Xvar), y = !!input$Yvar, color = !!(input$categorical))) +
        geom_point()
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
