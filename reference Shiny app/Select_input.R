library(shiny)
library(ggplot2)

CSS <- "
.selectize-dropdown [data-selectable].option-disabled {
  color: #aaa;
  cursor: default;
}"

type <- as.character(c('summer','summer','summer','summer','winter','winter','winter','winter'))
country <- as.character(c('A','A','B','B','A','A','B','B'))
year <- c(2011,2012,2013,2014,2011,2012,2013,2014)
col1 <- c(33,7,NA,NA,5,11,NA,NA)
col2 <- c(10,3,NA,NA,8,15,NA,NA)
col3 <- c(NA,NA,10,15,NA,NA,20,25)
col4 <- c(NA,NA,8,5,NA,NA,22,16)

TD <- data.frame(type,country,year,col1,col2,col3,col4,stringsAsFactors=FALSE)


ui <- fluidPage(
  tags$head(
    tags$script(src = "selectize-disable-options.js"),
    tags$style(HTML(CSS))
  ),
  titlePanel("Test App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Choose a type", choices = c("All",unique(TD$type))),
      selectInput("country","Choose an country", choices = c("All",unique(TD$country))),
      selectizeInput("yaxis", "Choose a y variable", choices = colnames(TD[,3:7])),
      selectInput("xaxis", "Choose a x variable", choices = colnames(TD[,3:7])),
      actionButton("goButton", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotOutput("plot1"))
      )
    )
  )
)


server <- function(input, output, session){
  
  data1 <- reactive({
    if(input$type == "All"){
      TD
    }else{
      TD[TD$type == input$type,]
    }
  })
  
  data2 <- reactive({
    if(input$country == "All"){
      TD
    }else{
      TD[TD$country == input$country,]
    }
  })
  
  observe({
    if(input$type != "All"){
      selected_country <- isolate(input$country)
      countries <- unique(data1()$country)
      updateSelectInput(
        session, "country", 
        choices = c("All", countries),
        selected = ifelse(selected_country %in% countries, selected_country, "All")
      )
    }else if(input$country != 'All'){
      selected_type <- isolate(input$type)
      types <- unique(data2()$type)
      updateSelectInput(
        session, "type", 
        choices = c('All', types),
        selected = ifelse(selected_type %in% types, selected_type, "All")
      )
    }else if(input$type == "All" && input$country == "All"){
      updateSelectInput(
        session, "country", 
        choices = c('All', unique(TD$country))
      )
      updateSelectInput(
        session, "type", 
        choices = c('All', unique(TD$type))
      )
    }
  })
  
  data3 <- reactive({
    if(input$country == "All"){
      data1()
    }else if(input$type == "All"){
      data2()
    }else if(input$country == "All" && input$type == "All"){
      TD
    }else{
      TD[which(TD$country== input$country & TD$type == input$type),]
    }
  })
  
  observeEvent(data3(), {
    emptyColumns <- sapply(data3()[,3:7], function(x){
      all(is.na(x))
    })
    choices <- colnames(TD[,3:7])
    choices[emptyColumns] <- paste(choices[emptyColumns], "(no data)")
    updateSelectizeInput(
      session, "yaxis", choices = choices,
      options = list(
        plugins = list(
          disable_options = list(
            disableOptions = as.list(choices[emptyColumns])
          )
        )
      )
    )
  })
  
  data4 <- eventReactive(input$goButton, {
    data3()
  })
  
  x_var<- eventReactive(input$goButton, {
    input$xaxis
  })
  y_var <- eventReactive(input$goButton,{
    input$yaxis
  })
  
  output$plot1 <- renderPlot({
    x <- x_var()
    y <- y_var()
    p <- ggplot(data4(), aes_string(x=x, y=y)) + geom_line() + geom_point()
    p + labs(x = x, y = y) + theme(plot.title = element_text(hjust = 0.5, size=20))
  })
  
}

shinyApp(ui,server)