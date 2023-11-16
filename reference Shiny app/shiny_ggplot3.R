library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(    
  titlePanel("chart"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Category","Category:",choices=colnames(mtcars))
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plooot")  
    )
  )
)

server <- function(input, output) {
  output$plooot<-renderPlot({
    req(input$Category)
    icq <- sym(input$Category)
    mtcars %>%
      group_by(!!!icq, vs) %>%
      summarise(disp=sum(disp)) %>%
      ggplot(aes_string(input$Category, "disp", fill="vs")) +
      geom_bar(stat="identity", position="dodge")
  })
}

shinyApp(ui=ui,server=server)