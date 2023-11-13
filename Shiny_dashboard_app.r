library(shiny)

# The UI
ui <- fluidPage(
  #"Hello, world!"
  titlePanel("國立台北商業大學 校務永續發展中心"),
  selectInput("datasetA", 
              label = "Dataset", 
              choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

# the server
server <- function(input, output, session) {
  output$summary <- renderPrint({
    datasetA <- get(input$datasetA, "package:datasets")
    summary(datasetA)
  })
  
  output$table <- renderTable({
    datasetA <- get(input$datasetA, "package:datasets")
    datasetA
  })
}

# the 
shinyApp(ui, server)
