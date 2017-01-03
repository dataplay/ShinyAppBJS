library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
source("global.R")

# Define UI for application that create time-series charts
# on crime victimation rates from Bureau of Justice Statistics

# Define server logic required to draw a line chart

server <- shinyServer(function(input, output) {

  datasetInput <- reactive({switch(input$crime, 
                       "Serious Violent Victimization" = bjscrimedata$Violent.Victimization,
                       "Rape/Sexual Assault" = bjscrimedata$Rape.Sexual.Assault,
                       "Robbery Aggravated Assault" = bjscrimedata$Robbery,
                       "Simple Assault" = bjscrimedata$Simple.Assault,
                       "Personal Theft/Larceny" = bjscrimedata$Personal.Theft.Larceny)
  })
  
  merged_data <- reactive({data.frame(bjscrimedata$Year, datasetInput())
  })
  
  output$text <- renderText({
    paste(input$crime,": Victimization per 1,000 persons age 12 or older, 1993-2015")
  })
  
  output$summary <- renderPrint({
    dataset <- merged_data()
    paste(input$crime, "Summary Statistics") 
    summary(dataset$datasetInput)
  })
  
  output$lineChart <- renderPlotly({
    dataset <- merged_data()
   p <- plot_ly(dataset, x = ~bjscrimedata.Year, y = ~datasetInput() , type = 'scatter', mode = 'lines') 
   p <- layout(p, xaxis = list(           
                 title = "Year",
                 range = c(1992, 2017),
                 titlefont = list(size=14),
                 zeroline = TRUE
               ),
               yaxis = list(           
                 title = "Victimization Rates",
                 titlefont = list(size=14),
                 ticks="outside",
                 ticklen=3,
                 zeroline = TRUE, 
                 showline=TRUE
               ), height=400, width=600)
  })
  
})  

shinyUI(sidebarLayout(
  # Application title
  titlePanel("Crime Victimization Rates from Bureau of Justice Statistics"),
  
  ui <- fluidPage(
    sidebarPanel(
      # Application title
      titlePanel(h4("Crime Victimization Rates from Bureau of Justice Statistics")),
      #Select Input
      selectInput("crime", "Select a Category of Crime", choices = c("Serious Violent Victimization", 
      "Rape/Sexual Assault", "Robbery Aggravated Assault","Simple Assault","Personal Theft/Larceny"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("text")),
      plotlyOutput("lineChart"),
      h3(" "),
      h5("Summary Statistics"),
      verbatimTextOutput("summary")
    )
  )
))

  
shinyApp(ui = ui, server = server)