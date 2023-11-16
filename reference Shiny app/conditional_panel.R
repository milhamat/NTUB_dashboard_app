## Only run this example in interactive R sessions
if (interactive()) {
  
  ## UI (USER INTERFACE)
  ui <- fluidPage(
    sidebarPanel(
      selectInput("plotType", "Plot Type",
                  c(Scatter = "scatter", Histogram = "hist")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plotType == 'hist'",
        selectInput("breaks", "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
        ),
        # Only show this panel if Custom is selected
        conditionalPanel(
          condition = "input.breaks == 'custom'",
          sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
        )
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
  
  ## SERVER <- BACKEND
  server <- function(input, output) {
    x <- rnorm(100)
    y <- rnorm(100)
    
    #reactive({
      #print(input$breaks)
      #print(input$breakCOunt)
    #})
    
    
    output$plot <- renderPlot({
      
      print(input$breaks)
      print(input$breakCOunt)
      
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        
        hist(x, breaks = breaks)
      }
    })
  }
  
  ## THE EXECUTER
  shinyApp(ui, server)
}
## source
##https://search.r-project.org/CRAN/refmans/shiny/html/conditionalPanel.html