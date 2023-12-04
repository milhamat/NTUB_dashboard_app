ui <- shinyUI(
  fluidPage(
    tags$style(".nav-tabs {
  background-color: #800080;
    }

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: transparent;
border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
}"),
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

server <- shinyServer(function(input, output, session){
  
  
})

shinyApp(ui=ui, server=server)