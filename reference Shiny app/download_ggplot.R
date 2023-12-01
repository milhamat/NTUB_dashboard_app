library(shiny)
library(shinydashboard)

ui <- tabItem(tabName = "models2",
              fluidPage(
                fluidRow(
                  infoBoxOutput("overview")
                ),
                fluidRow(
                  actionButton("result1","Generate Result"),
                  downloadButton('downloadPlot','Download Plot'),
                  plotOutput("plot3")
                )
              ))

server <- function(input,output,session){
  output$overview <- renderValueBox({
    valueBox(
      paste("91"),"Overview",icon=icon("hourglass"),
      color="green"
    )
  })
  data <- reactiveValues()
  
  observeEvent(input$result1,{
    data$plot <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))+ 
      geom_point(shape=1)})
  
  output$plot3  <- renderPlot({  data$plot })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("input$plot3",'.png',sep='')},
    content = function(file){
      ggsave(file,plot=data$plot)
    }
  )
}

shinyApp(ui, server)