library(shiny)
library(ggplot2)

# global code runs once
i <- 100
x <- runif(i)
y <- runif(i)

# Define UI for application
ui <- fluidPage(
  titlePanel("Random Dataset"),
  sidebarLayout(
    sidebarPanel(
      h4("Input Data"),
      sliderInput("pbins", "Multiplier:", min = 1,max = 5, value = 2)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      h4("Distribution"),
      plotOutput("distPlot"),
      h4("Table"),
      tableOutput("table")
    )
  )
)

# Define server logic required
server <- function(input, output) {
  
  # z is a function of y and the slider input (1 to 5)
  z <- reactive ({(y*input$pbins)})
  
  output$distPlot <- renderPlot({
    
    df <- data.frame(x = x, y = y, condition = "nominal")
    df2 <- data.frame(x = x, z = z(), condition = "varied")
    
    ggplot()+
      geom_line(data = df, aes(x = x, y = y), colour = "blue") +
      geom_line(data = df2, aes(x = x, y = z), colour = "red")
    
  })
  
  output$table <- renderTable({
    m_z <- mean(z())
    sd_z <- sd(z())
    
    results <- data.frame(0, 0)
    results[1,1] = format(m_z, digits = 3)
    results[1,2] = format(sd_z, digits = 4)
    colnames(results) <- c("Mean z", "SD z")
    rownames(results) <- c("Stats")
    results
  })
}

shinyApp(ui = ui, server = server)