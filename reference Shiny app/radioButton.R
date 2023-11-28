ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV file to upload", accept = ".csv"),
      radioButtons(
        "dman_preview", "Display:",
        c("preview", "str", "summary"),
        selected = "preview",
        inline = TRUE
      ),
    ),                                   
    
    mainPanel(
      uiOutput("myoutput")
    )
  )
)

server <- function(input, output){
  df1 <- reactive({
    # req(input$file1)
    # df <- read.csv(input$file1$datapath,
    #                header = input$header,
    #                sep = input$sep,
    #                quote = input$quote)
    # df
    cars ## temporary dataframe for testing purpose
  })
  
  output$contents <- renderTable({
    
    # if(input$disp == "head") {
    #   return(head(df1()))
    # }
    # else {
    #   return(df1())
    # }
    
    df1()
  })
  
  output$summary <- renderPrint({
    summary(df1())
  })
  
  output$str <- renderPrint({ str(df1()) })
  
  output$myoutput <- renderUI({
    switch(input$dman_preview, 
           "preview" = tableOutput("contents"),
           "str" = verbatimTextOutput("summary"),
           "summary" = verbatimTextOutput("str")
    )
  })
}

shinyApp(ui = ui, server = server)