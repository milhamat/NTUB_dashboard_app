library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      uiOutput("df_merge_column_selection"), #<#>#
      textInput("new_label_postmerge","New name:"),
      actionButton("combine_columns", "Combine columns"),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      tableOutput("adjust")
      
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  df_kept_statements <- reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
  })
  
  
  
  df_merge_statements <- reactive({
    df_kept_statements()
  })
  
  
  output$df_merge_column_selection <- renderUI({
    varSelectInput("variable_merge", 
                   "Statements to merge:", 
                   df_kept_statements(), 
                   multiple = TRUE) 
  })
  
  observeEvent(input$variable_merge,{
    print(input$variable_merge)
  })
  
  
  outputmergetemp <- reactive({   
    req(input$combine_columns)
    temp <- df_merge_statements() %>%  
      rowwise() %>%
      #mutate(!!as_name(input$new_label_postmerge) := sum(!!!input$variable_merge))
      #mutate(temp = sum(!!!input$variable_merge))
      poit <- input$new_label_postmerge
      mutate(!!as_name(poit) := sum(!!!input$variable_merge)) #%>% 
    #rename( = )
    #mutate(!!input$new_label_postmerge = sum(!!!input$variable_merge)) #%>% 
    
  })
  
  
  output$adjust <- renderTable({ 
    req(input$combine_columns)
    outputmergetemp() })
  
}
# Run the app ----
shinyApp(ui, server)