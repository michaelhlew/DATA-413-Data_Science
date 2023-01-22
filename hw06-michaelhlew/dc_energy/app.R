#Michael Lewis, Shiny App for DC energy

library(shiny)
library(tidyverse)
library(bslib)
library(broom)

#Data import 
read_rds("./data/energy_year.rds") -> data
#read_rds("dc_energy/data/energy_year.rds") -> data

#Factor variables 
data %>% 
  mutate(Report_Year = as_factor(Report_Year),
         Type_SS = as_factor(Type_SS),
         Type_EPA = as_factor(Type_EPA),
         Metered_Energy = as_factor(Metered_Energy),
         Metered_Water = as_factor(Metered_Water)) -> data

#New variable: Era
data %>% 
  mutate(Era = case_when(
                  (Built < 1900 ~ "Pre-1900"), 
                  (Built < 1951 ~ "Early-Mid 20th"),
                  (Built < 2000 ~ "Late 20th"),
                  (Built < 2011 ~ "Aughts"),
                  (Built >= 2011 ~ "Teens and later"))) -> data






ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "simplex"),
  titlePanel("Analyzing Building Energy Performance Data", windowTitle = "Analyzing Building Energy Performance Data"),
  tabsetPanel(type = "tabs",
              tabPanel("Introduction",
                       
                       mainPanel(
                         h4("This app supports exploratory data analysis of Building Energy Performance in Washington, DC"),
                         p("The data used in this app are a subset of a dataset used by the city of Washington, DC Department of Energy & Environment (DOEE). It is data on public and private building energy performance disclosed under the Building Energy Performance Standards (BEPS) Program."),
                         br(),
                         
                         p("The data set for the app was downloaded in February 2022 from:"),
                         a(href="https://opendata.dc.gov/datasets/DCGIS::building-energy-performance/about", "DC Open Data on Building Energy Performance"),
                         br(),
                         br(),
                         p("The data set was filtered to remove data on non-compliant reports and columns not of interest. The data was further reduced by eliminating records where there appeared to be
large discrepancies between the square footage of the building recorded for tax purposes and as reported by the owners in their reports. These discrepancies created extreme values
that could hinder the analysis. Finally, the variables were renamed to be more human readable. The meta-data definitions can be found at:"),

a(href="https://www.arcgis.com/sharing/rest/content/items/10f4f09fc5684d9988ae83ae4cca8b70/info/metadata/metadata.xml?format=default&output=html", "Building Energy Performance Metadata"),
br(),
br(),
p("This data is meant to be in accordance with the US Government Energy Star Program. For help in interpreting the variables suggest:"),
a(href="https://www.energystar.gov/buildings/benchmark/understand_metrics/source_site_difference", "EPA Energy Star Program")
                       )
              ),
tabPanel("Univariate",
sidebarLayout(
sidebarPanel(
  
  varSelectInput("var1", "Variable?", data = data),
  checkboxGroupInput("years", "What Report Years?", choices = unique(data$Report_Year)),
  checkboxInput("log1", "Log Transform?"),
  checkboxInput("factorflip", "Flip Coordinates on Factors?", value = FALSE),
  sliderInput("bins", "Number of Bins?", value = 40, min = 1,max = 100),
  numericInput("mu", "Null Value", value = 0, step = .5),
  tableOutput("table1")
           ),

mainPanel(plotOutput("plotuni"))
)
),
tabPanel("Bivariate",
sidebarLayout(
sidebarPanel(
  
  varSelectInput("var2X", "X Variable?", data = data),
  checkboxInput("log2X", "Log Transform?"),
  varSelectInput("var2Y", "Y Variable?", data = data),
  checkboxInput("log2Y", "Log Transform?"),
  checkboxInput("OLS", "Fit OLS?"),
  checkboxGroupInput("years2", "What Report Years?", choices = unique(data$Report_Year))
           ),

           mainPanel(plotOutput("plotbi")
)
)
), 

tabPanel("SpreadSheet",
checkboxInput("numfact", "See only numeric or factor?"),
dataTableOutput("sheet")
)
  ))




# Output
server <- function(input, output) {
  
  output$plotuni <- renderPlot({
    req(input$years)
    data %>% 
      filter(Report_Year == !!(input$years)) %>% 
      drop_na(input$var1)-> data #filter data on years
    
    p1 <- ggplot(data, aes(x = !!input$var1)) + facet_wrap(~ Report_Year) #base plot
    
    if(is.numeric(data[[input$var1]])){
      
      
      if(input$log1) {
        p1 <- p1 + geom_histogram(bins = input$bins) +
          scale_x_log10() +
          labs(x = paste("Log(",input$var1,")")) } 
      
      else { p1 <- p1 + geom_histogram(bins = input$bins) } }
    if ((input$factorflip) & is.factor(input$var1))
    { p1 + geom_bar() + coord_flip() }
    
    
    p1 
    
    
  }) 
  
  
  
  #T-test
  output$table1 <- renderTable({
    if(is.numeric(data[[input$var1]])){
      if(input$log1){ #t-test for if the data is logged
        validate(need(!data[[input$var1]] <= 0),"T-test unavailable: variable has one or more values less than or equal to zero.")
        data %>%
          select(input$var1) %>% 
          log() %>%
          t.test(alternative = "two.sided", mu = input$mu , conf.level = 0.95) %>% 
          broom::tidy() %>% select("P-value" = p.value, "Estimate" = estimate,
                                   "95 % Lower" = conf.low, "95 % Upper" = conf.high)
      }
      
      else{
        data %>%
          select(input$var1) %>% 
          t.test(alternative = "two.sided", mu = input$mu , conf.level = 0.95) %>% 
          broom::tidy() %>% select("P-value" = p.value, "Estimate" = estimate, "95 % Lower" = conf.low, "95 % Upper" = conf.high)
      }
    }
    else{
      print("Variable is not numeric")
    }
  })
  
  #Bivariate
  output$plotbi <- renderPlot({
    req(input$years2)
    data %>% 
      filter(Report_Year == !!(input$years2)) %>% 
      mutate(Report_Year = as.factor(Report_Year))-> data #filter data on years
    
    pl2 <- ggplot(data, aes(x = !!input$var2X, y = !!input$var2Y, color = Report_Year)) 
    
    #Base 
    
    #If both numeric
    if(is.numeric(data[[input$var2X]]) & is.numeric(data[[input$var2Y]]) ){
      
      #If OLS, yes
      if(input$OLS){
        #OLS with Log on both
        if(input$log2X & input$log2Y){
          pl2 + geom_point() + scale_x_log10() + scale_y_log10() + geom_smooth(method = lm, se = FALSE) +
            labs(x = paste("Log(",input$var2X,")")) + labs(y = paste("Log(",input$var2Y,")")) -> pl2
          
        }
        #Just logged on X
        else if(input$log2X){
          pl2 <- pl2 + 
            geom_point() + scale_x_log10() + geom_smooth(method = lm, se = FALSE) + labs(x = paste("Log(",input$var2X,")")) 
        }
        #Just logged on Y
        else if(input$log2Y){
          pl2 <- pl2 + geom_point() + scale_y_log10() + geom_smooth(method = lm, se = FALSE) + labs(y = paste("Log(",input$var2Y,")"))
        }
        #Not logged
        else{ pl2 <- pl2 + geom_point() +geom_smooth(method = lm, se = FALSE) }
      } #End of the OLS
      #No OLS:
      else {
        
        #No OLS, logged X & Y
        if(input$log2X & input$log2Y){
          pl2 <- pl2 + geom_point() + scale_x_log10() + scale_y_log10() + labs(x = paste("Log(",input$var2X,")")) + labs(y = paste("Log(",input$var2Y,")"))
        }
        #No OLS, logged on X
        else if(input$log2X){ pl2 <- pl2 + geom_point() + scale_x_log10() + labs(x = paste("Log(",input$var2X,")"))
        }
        #No OLS, logged on Y
        else if(input$log2Y){ pl2 <- pl2 + geom_point() + scale_y_log10() + labs(y = paste("Log(",input$var2Y,")"))
        
        }
        #No OLS, No Logs
        else{ pl2 <- pl2 + geom_point()
        }
      }# OLS done
    }
    
    #Y not numeric
    
    else if(is.numeric(data[[input$var2X]]) & !is.numeric(data[[input$var2Y]])){
    
      if(input$log2X & input$log2Y){
        validate(need(is.double(data[[input$var2X]]) & is.double(data[[input$var2Y]]),
                      "Variables are non-numeric"))
      }
      
      else if(input$log2Y){validate(need(is.double(data[[input$var2Y]]),
                                         "Dependent variable is non-numeric")) 
      } else if(input$log2X){
        pl2 <- pl2 + geom_boxplot() + scale_x_log10() + labs(x = paste("Log(",input$var2X,")"))
      }
      
      else{pl2 <- pl2 + geom_boxplot()  
      }
      
    }
    #Not numeric X, numeric Y
    else if(!is.numeric(data[[input$var2X]]) & is.numeric(data[[input$var2Y]])){
      if(input$log2X & input$log2Y){
        pl2 <- pl2 +
          validate(
            need(is.double(data[[input$var2X]]) & is.double(data[[input$var2Y]]),
                 "Non-numeric variable")
          ) 
      }
      
      else if(input$log2X){
        validate(need(is.double(data[[input$var2X]]),"Non-numeric variable")
        ) 
      }
      else if(input$log2Y){
        pl2 <- pl2 + geom_boxplot() + scale_y_log10() + labs(y = paste("Log(",input$var2Y,")"))
        
      }
      else{
        pl2 <- pl2 + geom_boxplot()
      }
      
    }else{
      if(input$log2X & input$log2Y){validate(need(is.double(data[[input$var2X]]&data[[input$var2Y]]),
      "Non-numeric variable")
        )  
      }else if(input$log2X){
        validate(need(is.double(data[[input$var2X]]),"Non-numeric variable")
        )
      }else if(input$log2Y){
        validate(need(is.double(data[[input$var2Y]]),"Non-numeric variable")
        )
      }else{pl2 <- pl2 + geom_jitter()
      }
    }
    pl2
  })
  
  
  
  output$sheet <- renderDataTable({
    if (input$numfact) {keep(data, ~ is.numeric(.) | is.factor(.))}
    else {data}
    
  })
  
  
  
}


shinyApp(ui = ui, server = server)
