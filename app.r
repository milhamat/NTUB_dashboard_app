library(shiny)

# The UI
ui <- fluidPage(
  
  fluidRow(
    titlePanel("國立台北商業大學 校務永續發展中心"),
    img(src="campus_logo.png", height=75, width=180)
  ),
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
