library(shiny)
library(DT)

df <- iris

ui <- fluidPage(
  DT::dataTableOutput('data'),
)

server <- function(input, output) {
  
  rv <- reactiveVal(df)
  
  output$data <- DT::renderDataTable ({
    DT::datatable(rv(), editable = TRUE)
  })
  
  observeEvent(input$data_cell_edit, {
    info <- input$data_cell_edit
    newdf <- rv()
    newdf[info$row, info$col] <- info$value
    rv(newdf)
    df <<- rv()
  })
  
}

shinyApp(ui = ui, server = server)