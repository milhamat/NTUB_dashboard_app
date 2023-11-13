library(shiny)
library(datasets)
library(ggplot2)

ui <- shinyUI(fluidPage(
  titlePanel("國立台北商業大學 校務永續發展中心"),
  img(src="campus_logo.png", height=70, width=200),
  tabsetPanel(
    tabPanel("Raw Data", # Upload File
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 ## Table Out
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Visualize in ggplot", # First Type
             pageWithSidebar(
               headerPanel('My First Plot'),
               sidebarPanel(
                 selectInput(
                   inputId = "select_plot",
                   label = "plot",
                   choices = c("-", 
                               "histogram" = "geom_histogram", 
                               "density plot" = "geom_density",
                               "box plot" = "geom_boxplot", 
                               "violin plot" = "geom_violin", 
                               "scatter plot" = "geom_point")),
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 ## Plot Out
                 plotOutput('MyPlot')
               )
             )
    )
    
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    ## For updating the x and y axis feature 
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  # Select Input
  #reactive({
    #if (input$select_plot=="geom_histogram"){
      #updateSelectInput(session, "ycol",
                        #choices = "-")
    #}else{
      #updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                        #choices = names(df), selected = names(df))
      #updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                        #choices = names(df), selected = names(df)[2])
    #}
  #})
  
  
  output$MyPlot <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    # x    <- data()[, c(input$xcol, input$ycol)]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    # x    <- data()[, input$xcol]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    x <- data()[, c(input$xcol, input$ycol)]
    
    if (input$select_plot=="geom_point"){
      plot(x)
    }
    if (input$select_plot=="geom_histogram"){
      x <- data()[, c(input$xcol)]
      hist(x)
    }
    if (input$select_plot=="geom_boxplot"){
      x <- data()[, c(input$xcol)]
      boxplot(x)
    }
    if (input$select_plot=="geom_density"){
      x <- data()[, c(input$xcol)]
      density(x)
    }
    if (input$select_plot=="geom_violin"){
      
    }
    
  })
})

shinyApp(ui, server)