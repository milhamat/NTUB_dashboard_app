library(shiny)
library(datasets)
library(ggplot2)

my_mtcars <- mtcars
my_mtcars$cyl[1:10]=NA
my_mtcars$wt[1:10]=NA

ui <- fluidPage(
  fileInput('file1', 'Choose CSV File',
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv')),
  actionButton('upload_button','Upload'),
  actionButton('convert_button','Convert'),
  verbatimTextOutput("datainfo"),
  tableOutput('table')
)

server <- shinyServer(function(input,output){
  
  data <- reactiveVal()
  
  # file$datapath -> gives the path of the file
  observeEvent(input$file1, ignoreNULL=T, ignoreInit=T, {
    req(input$file1)
    inFile <- input$file1 
    dat <- read.csv(inFile$datapath)
    data(dat)
    print(names(dat))
  })
  
  observeEvent(input$convert_button, {
    claim <- data() # read the data from the reactiveVal
    #claim[is.na(claim)] <- 0 # replace with zero
    claim <- na.omit(claim)
    data(claim) # write back the modified data to the reactiveVal
  })
  
  output$datainfo <- renderPrint({
    if(is.null(data())){return ()}
    str(data())
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
})

shinyApp(ui,server)