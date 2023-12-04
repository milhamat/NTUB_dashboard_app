library(shiny)

ui <- shinyUI(
  bootstrapPage(
    navbarPage(title = NULL, id = "navbar",
               tabPanel("water"),
               tabPanel("earth")
    ),
    
    tags$style(HTML("
        .navbar-default .navbar-brand {color:white;}
        .navbar-default .navbar-brand:hover {color:white;}
        .navbar { background-color:purple;}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:black; background-color:white;}
        .navbar-default .navbar-nav > li > a:hover {color:white;background-color:#680973;text-decoration}
                  "))
  )
)

server <- function(input, output, session){}

shinyApp(ui, server)