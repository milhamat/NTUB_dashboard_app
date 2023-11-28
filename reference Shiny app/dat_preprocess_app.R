my_mtcars <- mtcars
my_mtcars$cyl[1:10]=NA
my_mtcars$wt[1:10]=NA

ui <- fluidPage(
  actionButton('upload_button','Upload'),
  actionButton('convert_button','Convert'),
  tableOutput('table')
)

server <- shinyServer(function(input,output){
  
  data <- reactiveVal()
  
  # file$datapath -> gives the path of the file
  observeEvent(input$upload_button, ignoreNULL=T, ignoreInit=T, {
    data(my_mtcars)
  })
  
  observeEvent(input$convert_button, {
    claim <- data() # read the data from the reactiveVal
    #claim[is.na(claim)] <- 0 # replace with zero
    claim <- na.omit(claim)
    data(claim) # write back the modified data to the reactiveVal
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
})

shinyApp(ui,server)