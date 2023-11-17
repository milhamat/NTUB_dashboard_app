library(shiny)
library(ggplot2)
library(ggfortify)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput('num',label='Insert Number of clusters',value = 3,min = 2,max = 10,step = 1)
    ),
    mainPanel(
      fluidRow(
        column(width = 6,
               plotOutput("data")
        ),
        column(width = 6,
               plotOutput("boxplot"))
      )
    )
  )
  
  
)
server <- function(input, output, session) {
  
  clust_data <- reactive({
    kmeans(mtcars,input$num)
  })
  output$data<-renderPlot({autoplot(clust_data(),data=mtcars,label=TRUE,label.size=3)})
  
  output$boxplot <- renderPlot({
    mtcars_with_clusters <- cbind(mtcars, clust_data()$cluster)
    colnames(mtcars_with_clusters) <- c(colnames(mtcars_with_clusters[-ncol(mtcars_with_clusters)]),
                                        "cluster")
    boxplot(mpg ~ cluster, data = mtcars_with_clusters)
  })
}

shinyApp(ui, server)