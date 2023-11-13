library(shiny)
library(ggplot2)

# The UI
ui <- fluidPage(
  fluidRow(titlePanel("國立台北商業大學 校務永續發展中心")),
  #fluidRow(img(src="campus_logo.png", height=70, width=200)),
  br(),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                # Panel Raw Data
                tabPanel("Raw Data",
                         # Sidebar panel for inputs ----
                         br(),
                         sidebarPanel(
                           # Input Select a file ----
                           fileInput("file1", "Choose CSV File"),
                           multiple = FALSE,
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                           
                           # Input: Checkbox if file has header ----
                           checkboxInput("header", "Header", TRUE),
                           
                           # Input: Select separator ----
                           radioButtons("sep", "Separator",
                                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
                         DT::dataTableOutput("raw_data")),
                
                ## Panel Visual GGplot
                tabPanel("Visualize in ggplot",
                         plotOutput("plot"),
                         ## Side Bar Panel
                         sidebarPanel(
                           ## Select Input
                           selectInput(
                             inputId = "select_plot",
                             label = "plot",
                             choices = c("-", 
                                         "histogram" = "geom_histogram", 
                                         "density plot" = "geom_density",
                                         "box plot" = "geom_boxplot", 
                                         "violin plot" = "geom_violin", 
                                         "scatter plot" = "geom_point")),
                           actionButton("goButton", "Update")
                           )),
                
                ## Panel Data Preporcessing
                tabPanel("Data Preprocessing",
                         )
                
    )
  )
  
  #selectInput("datasetA", 
  #label = "Dataset", 
  #choices = ls("package:datasets")),
  #verbatimTextOutput("summary"),
  #tableOutput("table")
)

# the server
server <- function(input, output, session) {
  
  # first tab
  output$raw_data <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  output$plot1 <- renderPlot({
    ggplot()
    +input$Select_plot(data = df, mapping = aes(x = input$aes_x, y = input$aes_y))
  })
  
  sel = ''
  # Button Actions
  observeEvent( input$goButton, {
    #print("Cliked")
    print(input$select_plot)
    sel = input$select_plot
    
    output$plot <- renderPlot({
      select = sel
      
      if (select == "geom_histogram"){
        print("Hist Pick")
        hist(df[1])
      }
      
    })
    
  })
 
  
  
  #output$summary <- renderPrint({
    #datasetA <- get(input$datasetA, "package:datasets")
    #summary(datasetA)
  #})
  
  #output$table <- renderTable({
    #datasetA <- get(input$datasetA, "package:datasets")
    #datasetA
  #})
}

# the 
shinyApp(ui, server)
