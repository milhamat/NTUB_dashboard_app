library(shiny)

ui <- fluidPage(
  mainPanel(
    fluidRow(
      align = "center",
      "How to center this?"
    ), width = 12
  )
)

server <- function(input, output) {}

shinyApp(ui, server) 