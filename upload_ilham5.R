library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)
library(caret)
###################################################################################################
ui <- shinyUI(fluidPage(
  ## Title
  titlePanel("國立台北商業大學 校務永續發展中心"),
  ## Image
  img(src="campus_logo.png", height=80, width=300),
  br(),
  ###################################################################################################
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
                 DT::dataTableOutput("table"),
                 #tableOutput('contents')
                 verbatimTextOutput("summary")
               )
             )
    ),
    ###################################################################################################
    ## Tabs
    tabPanel("Data Preprocessing",
             titlePanel("Preprocess"),
               sidebarLayout(
                 sidebarPanel(
                   tags$strong("Impute Missing Value"),
                   tags$br(),
                   actionButton("avg", "Average/Most Frequent",
                                #style='padding:5px; font-size:100%',
                                class="btn-block"),
                   actionButton("rplc", "Replace with Zero",
                                class="btn-block"),
                   actionButton("rmv", "Remove row with missing values",
                                class="btn-block"),
                   tags$br(),
                   tags$strong("Normalize"),
                   tags$br(),
                   actionButton("normlz", "Normalize to interval [0, 1]",
                                class="btn-block"),
                   actionButton("std", "Standardize",
                                class="btn-block"),
                   tags$br(),
                   tags$strong("Data Reset"),
                   tags$br(),
                   actionButton("resetData", "Reset Dataset",
                                class="btn-block"),
                 ),
                 mainPanel(
                   textOutput("miss"),
                   verbatimTextOutput("datainfo")
        )
      )
    ),
    ###################################################################################################
    tabPanel("Visualize in ggplot", ## First Type
             pageWithSidebar(
               headerPanel('Plots'),
               sidebarPanel(
                 selectInput(
                   inputId = "n_var",
                   label = "How many variable",
                   choices = c(
                               "One Variable" = "one_var", 
                               "Two Variable" = "two_var",
                               "Three Variable" = "three_var"
                    )
                ),
                ##########################################
                ## one Variable Pick
                conditionalPanel(
                  condition = "input.n_var == 'one_var'",
                  selectInput(
                    inputId = "one_var_pick",
                    label = "Plot Kinds",
                    choices = c(
                                "Countinuous" = "conti",
                                "Discrate" = "disct"
                    )
                  )
                ),
                ## two Variable Pick
                conditionalPanel(
                  condition = "input.n_var == 'two_var'",
                  selectInput(
                    inputId = "two_var_pick",
                    label = "Plot Kinds",
                    choices = c(
                                "Both Continuous" = "both_conti", 
                                "One Discrete, one continuous" = "one_dist_one_conti",
                                "Both Discrete" = "both_dist",
                                "Continuous Bivariate distribution" = "conti_bivar_dist",
                                "Continuous Function" = "conti_func"
                    )
                  )
                ),
                ## three Variable Pick
                conditionalPanel(
                  condition = "input.n_var == 'three_var'",
                  selectInput(
                    inputId = "three_var_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Contour" = "geom_contour", 
                                "Contour Filled" = "geom_contour_filled",
                                "Raster" = "geom_raster",
                                "Tile" = "geom_tile"
                    )
                  )
                ),
                ######################
                ### ONE VAR
                ## conti Pick
                conditionalPanel(
                  condition = "input.one_var_pick == 'conti' && input.n_var != 'two_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "conti_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Area Plot" = "geom_area_one",
                                "Density Plot" = "geom_density",
                                "Dot Plot" = "geom_dotplot",
                                "Freqpoly" = "geom_freqpoly",
                                "Histogram Plot" = "geom_histogram",
                                "QQ Plot" = "geom_qq"
                    )
                  )
                ),
                ## disct Pick
                conditionalPanel(
                  condition = "input.one_var_pick == 'disct' && input.n_var != 'two_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "disct_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Bar Plot" = "geom_bar"
                    )
                  )
                ),
                ######################
                ### TWO VAR
                ## both conti
                conditionalPanel(
                  condition = "input.two_var_pick == 'both_conti' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "both_conti_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Point" = "geom_point",
                                "Quantile" = "geom_quantile",
                                "Rug" = "geom_rug",
                                "Smooth" = "smooth"
                    )
                  )
                ),
                ## one dist one conti
                conditionalPanel(
                  condition = "input.two_var_pick == 'one_dist_one_conti' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "one_dist_one_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Col" = "geom_col",
                                "Boxplot" = "geom_boxplot",
                                "Violin" = "geom_violin"
                    )
                  )
                ),
                ## both dist
                conditionalPanel(
                  condition = "input.two_var_pick == 'both_dist' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "both_dist_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Count" = "geom_count",
                                "Jitter" = "geom_jitter"
                    )
                  )
                ),
                ## conti bivar dist
                conditionalPanel(
                  condition = "input.two_var_pick == 'conti_bivar_dist' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "conti_bivar_dist_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Bin2d" = "geom_bin2d",
                                "Desity 2d" = "geom_desity_2d"
                    )
                  )
                ),
                ## conti func
                conditionalPanel(
                  condition = "input.two_var_pick == 'conti_func' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "conti_func_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Area" = "geom_area_two",
                                "Line" = "geom_line",
                                "Step" = "geom_step"
                    )
                  )
                ),
                ######################
                 ## "Empty inputs" - they will be updated after the data is uploaded
                 ## X variable
                 selectInput('xcol', 
                             'X Variable', 
                             ""),
                 ## Y variable
                 conditionalPanel(
                  condition = "input.n_var != 'one_var'",
                    selectInput('ycol', 
                                'Y Variable', 
                                "", 
                                selected = ""
                    )
                  ),
                 ## Z variable
                 conditionalPanel(
                  condition = "input.n_var == 'three_var'",
                    selectInput('zcol', 
                                'Z Variable', 
                                "", 
                                selected = ""
                    )
                  ),
                 ## Ymin
                 conditionalPanel(
                  condition = "input.n_var == 'two_var' && input.two_var_pick == 'vis_err'",
                    selectInput('ymin', 
                                'Ymin', 
                                "", 
                                selected = ""
                    )
                  ),
                 ## Ymax
                 conditionalPanel(
                  condition = "input.n_var == 'two_var' && input.two_var_pick == 'vis_err'",
                    selectInput('ymax', 
                                'Ymax', 
                                "", 
                                selected = ""
                    )
                  ),
                tags$br(),
                tags$p(strong("Download Plot as png")),
                downloadButton("savePlot", "Save Plot")
               ),
               ################
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
###################################################################################################
server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  ## added "session" because updateSelectInput requires it
  
  dat <- reactiveVal()
  OriginalData <- reactiveVal()
  
  ## Initialize the dataset
  datainlist <- reactive({
    list(input$file1, input$header, input$sep, input$quote)
  })

  observeEvent(datainlist(), ignoreNULL=T, ignoreInit=T, {
    
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                    quote = input$quote)
    dat(df)
    OriginalData(df)
    # For updating the x and y axis feature 
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'zcol', label = 'Z Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ymin', label = 'Ymin',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ymax', label = 'Ymax',
                      choices = names(df), selected = names(df)[1])
    #choices = names(df), selected = names(df)[2])
  })
  
  ## Showing Raw Data Summary
  output$summary <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    summary(dat())
  })
  
  ## 
  output$datainfo <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    str(dat())
  })
  
  ## Showing Raw Data Table
  output$table <- DT::renderDataTable({
    datt <- dat()
    tryCatch({
      datt
    })
    return(datt)
  })
  ### Impute Missing Value
  observeEvent(input$rmv, {
    dataUpdate <- dat()
    dataUpdate <- na.omit(dataUpdate)
    dat(dataUpdate)
    print(typeof(dataUpdate))
  })
  
  observeEvent(input$rplc, {
    dataUpdate <- dat()
    dataUpdate[is.na(dataUpdate)] <- 0
    dat(dataUpdate)
  })
  
  observeEvent(input$avg, {
    dataUpdate <- dat()
    for (i in 1:ncol(dataUpdate)){
      dataUpdate[is.na(dataUpdate[,i]), i] <- mean(dataUpdate[, i], na.rm=T)
    }
    dat(dataUpdate)
  })
  ### Normalize
  observeEvent(input$std, {
    dataUpdate <- dat()
    rmchar <- dataUpdate[, !sapply(dataUpdate, is.character)]
    scl <- scale(rmchar)
    scl <- data.frame(scl)
    colnm <- colnames(scl)
    dataUpdate[colnm] <- scl[colnm]
    dat(dataUpdate)
  })

  observeEvent(input$normlz, {
    dataUpdate <- dat()
    rmchar <- dataUpdate[, !sapply(dataUpdate, is.character)]
    process <- preProcess(as.data.frame(rmchar), method=c("range"))
    norm_scale <- predict(process, as.data.frame(rmchar))
    colnm <- colnames(norm_scale)
    dataUpdate[colnm] <- norm_scale[colnm]
    dat(dataUpdate)
  })
  
  observeEvent(input$resetData, {
    # for resetting the dataset
    dat(OriginalData())
  })
  
  output$miss <- renderText({
    valcnt <- sum(is.na(dat()))
    paste0("total missing value: ", valcnt)
  })
  
  ## For Plotting
  output$MyPlot <- renderPlot({
    ### Important for updating the layout
    ### we need to reinitialize the dataset
    datt <- dat()[, c(input$xcol, input$ycol, input$zcol)]
    ### ONE VARIABLE
    ## conti
    conti <- input$conti_pick
    disct <- input$disct_pick 
    if (input$n_var=="one_var"&&input$one_var_pick=="conti"){
      if (conti=="geom_histogram"){ # geom_histogram
        ggplot(datt, aes_string(x=input$xcol))+geom_histogram(colour='darkblue')
      } else if (conti=="geom_density"){ # geom_density
        ggplot(datt, aes_string(x=input$xcol))+geom_density(colour='darkblue')
      } else if (conti=="geom_area_one") { # geom_area_one
        ggplot(datt, aes_string(x=input$xcol))+geom_area(stat="bin")
      } else if (conti=="geom_dotplot") { # geom_dotplot
         ggplot(datt, aes_string(x=input$xcol))+geom_dotplot()
      } else if (conti=="geom_freqpoly") { # geom_freqpoly
         ggplot(datt, aes_string(x=input$xcol))+geom_freqpoly()
      } else if (conti=="geom_qq") { # geom_qq
         ggplot(datt, aes_string(sample=input$xcol))+stat_qq()
      } 
    } else if (input$n_var=="one_var"&&input$one_var_pick=="disct") {
      if (disct=="geom_bar"){ ## disct # geom_bar
        ggplot(datt, aes_string(x=input$xcol))+geom_bar()
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="both_conti"){
      if (input$both_conti_pick=="geom_point"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_point(colour='darkblue')
      } else if (input$both_conti_pick=="geom_quantile") {
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_quantile()
      } else if (input$both_conti_pick=="geom_rug"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_rug(sides = "bl")
      } else if (input$both_conti_pick=="smooth"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_smooth()
      } 
    } else if (input$n_var=="two_var"&&input$two_var_pick=="one_dist_one_conti"){
      if (input$one_dist_one_pick=="geom_boxplot"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_boxplot()
      } else if (input$one_dist_one_pick=="geom_violin"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_violin()
      } else if (input$one_dist_one_pick=="geom_col"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_col()
      } 
    } else if (input$n_var=="two_var"&&input$two_var_pick=="both_dist"){
      if (input$both_dist_pick=="geom_count"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_count()
      } else if (input$both_dist_pick=="geom_jitter"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_jitter()
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="conti_bivar_dist"){
      if (input$conti_bivar_dist_pick=="geom_bin2d"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_bin_2d()
      } else if (input$conti_bivar_dist_pick=="geom_desity_2d"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_density_2d()
      } 
    } else if (input$n_var=="two_var"&&input$two_var_pick=="conti_func"){
      if (input$conti_func_pick=="geom_area_two"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_area()
      } else if (input$conti_func_pick=="geom_line"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_line()
      } else if (input$conti_func_pick=="geom_step"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol))+geom_step()
      }
    }  else if (input$n_var=="three_var"){
      if (input$three_var_pick=="geom_contour"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_contour()
      } else if (input$three_var_pick=="geom_contour_filled"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_contour_filled()
      } else if (input$three_var_pick=="geom_raster"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_raster()
      } else if (input$three_var_pick=="geom_tile"){
        ggplot(datt, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_tile()
      }
    }
  } ,height = 400, width = 600
  )
  
  output$savePlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in") 
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )
  
})
###################################################################################################
shinyApp(ui, server)