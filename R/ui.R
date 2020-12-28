library(plotly)
library(shinyjqui)

ui <- fluidPage(
  headerPanel('TV Series Ratings'),
  
  sidebarPanel(
    width=3,
    textInput('seriesName', 'TV Series', value = "", placeholder = "The Office"),
    numericInput('nseason', 'No of Seasons', 1, min = 1, max = 50),
    helpText("Number of seasons should not exceed the number of seasons the show has."),
    submitButton("Submit")
  ),
  
  mainPanel(
    jqui_resizable(plotlyOutput("plot1"))
  )
)

