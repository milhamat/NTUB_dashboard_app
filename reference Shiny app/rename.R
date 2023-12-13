library(shiny)
library(DT)
library(data.table)
library(dplyr)

ui <- fluidPage(
  titlePanel(""),
  fluidRow(
    checkboxInput(inputId = "logarithm",
                  label = "Log(variable)"),
    dataTableOutput("my_df"),
    textInput("new_name", 
              label = "New_name"),
    actionButton("new_name2", "Validate")
  )
)


server <- function(input, output) {
  
  data <- head(mtcars[, 1:3])
  
  
  reactive_data <- eventReactive(input$new_name2, {
    colnames(data) <- c("mpg", "cyl", input$new_name)
    data
  }) 
  
  output$my_df <- renderDataTable({
    data <- reactive_data()
    if(input$logarithm){
      log_name <- paste0('logarithm(', input$new_name, ')')
      data %>%
        mutate(!!log_name := log(data[, input$new_name]))
      
    }
    else {
      data
    }
  })
}

shinyApp(ui = ui, server = server)