library(shiny)

ui <- shinyUI(
  fluidPage(
    tags$style(HTML("
        .tabbable > .nav > li > a                  {background-color: purple;  color:white}"
                    )),
    tabsetPanel(
      tabPanel(
        title = "Hello",
        textInput(inputId = "text", label = "Input")
      ),
      tabPanel(
        title = "World"
      )
    )
  )
)
server <- function(input, output) {}
shinyApp(ui=ui,server=server)