library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("ANOVA",
             uiOutput("varNames"),
             verbatimTextOutput("anovaTable")
    )
  )
)

# @
server <- function(input, output) {
  output$varNames <- renderUI({
    selectizeInput("varNames2",
                   "Variables: ",
                   choices = names(iris[, -5])
    )
  })
  
  output$anovaTable <- renderPrint({
    aov.model <- aov(iris[[input$varNames2]] ~ iris$Species, data = iris) # Stuck here
    #print(aov.model)
    br()
    br()
    print(summary(aov.model))
    cat("Coefficients"); cat("\n")
    print(aov.model$coefficients)
  }) 
}

shinyApp(ui, server)