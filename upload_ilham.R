library(shiny)
library(datasets)
library(ggplot2)

ui <- shinyUI(fluidPage(
  ## Title
  titlePanel("國立台北商業大學 校務永續發展中心"),
  ## Image
  #img(src="campus_logo.png", height=70, width=200),
  br(),
  ## Tabs
  tabsetPanel(
    ## Tabs panel
    tabPanel("Raw Data", # Upload File
             ## Title panel
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 ## added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 ## Checking box
                 checkboxInput('header', 'Header', TRUE),
                 ## Radio Buttons
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 ## Radio Buttons
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               ),
               ## Main Panel
               mainPanel(
                 ## Table Out
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Visualize in ggplot", ## First Type
             pageWithSidebar(
               headerPanel('Plots'),
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
                 
                 ## "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 p("Note: the plots only take numeric data!"),
                 ## Plot Out
                 plotOutput('MyPlot'),
                 
               )
             )
    )
    
  )
)
)

server <- shinyServer(function(input, output, session) {
  ## added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    ## For updating the x and y axis feature 
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[1])
                      #choices = names(df), selected = names(df)[2])
    #print(df)
    
    return(df)
  })
  
  ## Showing Raw Data Table
  output$contents <- renderTable({
    data()
  })
  
  ## For Plotting
  output$MyPlot <- renderPlot({
    dat <- data()[, c(input$xcol, input$ycol)]
    
    if (input$select_plot=="geom_histogram"){
      dat %>%
        ggplot(aes_string(x=input$xcol))+geom_histogram(colour='darkblue')
    } else if (input$select_plot=="geom_density"){
      dat %>%
        ggplot(aes_string(x=input$xcol))+geom_density(colour='darkblue')
    } else if (input$select_plot=="geom_boxplot"){
      dat %>%
        ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_boxplot(colour='darkblue')
    } else if (input$select_plot=="geom_violin"){
      dat %>%
        ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_violin(colour='darkblue')
    } else if (input$select_plot=="geom_point"){
      dat %>%
        ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_point(colour='darkblue')
    }
  },height = 400,width = 600
  )
})

shinyApp(ui, server)