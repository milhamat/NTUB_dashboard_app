library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)
library(ISLR)
library(caret)
library(tidyverse)
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
    tabPanel("Data Information", # Upload File
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
                 h4(strong("Observation")),
                 DT::dataTableOutput("table"),
                 #tableOutput('contents')
                 h4(strong("Summary")),
                 verbatimTextOutput("summary")
               )
             )
    ),
    ###################################################################################################
    tabPanel("Data Mining",
             h3("Filter"),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(4, selectInput("fltrCol", "Filter By:", c(""))),
                          column(4, selectInput("fltrBolean", "Boolean", c("==", "!=", ">", "<"))),
                          column(4, selectInput("fltrVal", "Value", c(""))
                          # column(4, uiOutput("COL_VALUE"))
                          #column(4, textInput(inputId = "inpFilter", label = "Value"))
                          ))
               ),
               mainPanel(
                 DT::dataTableOutput("Filter")
               )),
             hr(),
             h3("Select"),
             sidebarLayout(
               sidebarPanel(
                selectInput(inputId = "selCol",
                             label = "Select Cloumn",
                             c(""),
                             ),
                #  varSelectInput(inputId = "selCol",
                #              label = "Select Cloumn",
                #              c(""),
                #              multiple = TRUE
                #              ),
                             ),
                 mainPanel(
                   DT::dataTableOutput("Select")
                             )),
             hr(),
             h3("Arrange"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Argn",
                             label = "Arrange By",
                             choices = ""),
                             ),
                 mainPanel(
                   DT::dataTableOutput("Arrange")
                             )),
             hr(),
             #3
             h3("Mutate"),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(5, textInput(inputId = "mutName", label = "Name")),
                          #column(4, textInput("mutpick", label=" ", value="=")),
                          column(2,  p("=", style="text-align: center;")), #align="center",
                          column(5, textInput(inputId = "mutForml", label = "Formula"))
                          ),
                 actionButton("mutApply", "Apply Mutate",
                                         class="btn-block"),
                 ),
               mainPanel(
                 DT::dataTableOutput("Mutate")
               )),
             hr(),
             #3
             h3("Summarise"),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(5, selectInput(inputId = "sumVal", label = "Value", c(""))),
                          column(2,  p("=", style="text-align: center;")),
                          column(5, selectInput("sumFunc", "Filter By:",  c("Maen"="smrMean",
                                                                            "Median"="smrMedian",
                                                                            "Variance"="smrVar",
                                                                            "Numbers"="smrNum",
                                                                            "SD"="smrSD",
                                                                            "IQR"="smrIqr",
                                                                            "Min"="smrMin",
                                                                            "Max"="smrMax",
                                                                            "All"="smrAll")))
                )),
               mainPanel(
                 #DT::dataTableOutput("Summarise")
                 verbatimTextOutput("Summarise")
               )),
             hr(),
             #5
             h3("Group-by"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Grpby", #textInput
                             label = "Name :",
                             c("")),
                             ),
                             mainPanel(
                               DT::dataTableOutput("Groupby")
             )),
             hr(),
             #3
             h3("Rename"),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(5, selectInput(inputId = "olName", label = "Old Value", c(""))),
                          column(2,  p("=", style="text-align: center;")),
                          column(5, textInput(inputId = "nwName", label = "New Value"))
                          ),
                 actionButton("rnmApply", "Apply Change",
                              class="btn-block"),
                 ),
               mainPanel(
                 DT::dataTableOutput("Rename")
               )),
    ),
    ###################################################################################################
    ## Tabs
    tabPanel("Data Preprocessing",
             titlePanel("Preprocess"),
               sidebarLayout(
                 sidebarPanel(
                   p(strong("Note:"),textOutput("miss")),
                   #textOutput("miss"),
                   ######################ALL#########################
                   tags$strong("Impute Missing Value (whole)"),
                   tags$br(),
                   actionButton("avg", "Average/Most Frequent",
                                #style='padding:5px; font-size:100%',
                                class="btn-block"),
                   actionButton("rplc", "Replace with Zero",
                                class="btn-block"),
                   actionButton("rmv", "Remove row with missing values",
                                class="btn-block"),
                   tags$br(),
                   tags$strong("Normalize (whole)"),
                   tags$br(),
                   actionButton("normlz", "Normalize to interval [0, 1]",
                                class="btn-block"),
                   actionButton("std", "Standardize",
                                class="btn-block"),
                   ####################Partial#####################
                   tags$br(),
                   tags$strong("Impute Missing Value (partial)"),
                   tags$br(),
                   selectInput(inputId = "imptMissVal",
                               label = "select column:",
                               choices = ""),
                   actionButton("avg_p", "Average/Most Frequent",
                                #style='padding:5px; font-size:100%',
                                class="btn-block"),
                   actionButton("rplc_p", "Replace with Zero",
                                class="btn-block"),

                   tags$br(),
                   tags$strong("Normalize (partial)"),
                   tags$br(),
                   selectInput(inputId = "norm_p",
                               label = "select column:",
                               choices = ""),
                   actionButton("normlz_p", "Normalize to interval [0, 1]",
                                class="btn-block"),
                   actionButton("std_p", "Standardize",
                                class="btn-block"),
                   ################## LAST BUTTON ###################
                   tags$br(),
                   tags$strong("Data Reset (all)"),
                   tags$br(),
                   actionButton("resetData", "Reset Dataset",
                                class="btn-block"),
                 ),
                 mainPanel(
                   #textOutput("miss"),
                   h4(strong("Observation")),
                   verbatimTextOutput("datainfo")
        )
      )
    ),
    ###################################################################################################
    ## Tabs
    tabPanel("Statistical Test",
             #titlePanel("ANOVA"),
             h3("ANOVA"),
             sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "anovaX",
                            label = "Dependent Variable (X) :",
                            c("")),
                selectInput(inputId = "anovaY",
                            label = "Independent Variable (Y) :",
                            c("")),
              ),
              mainPanel(
                verbatimTextOutput("ANOVArslt")
            )
        ), #<----- here
              h3("T-test"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "ttestX",
                              label = "(X) :",
                              c("")),
                  selectInput(inputId = "ttestY",
                              label = "(Y) :",
                              c("")),
                ),
                mainPanel(
                  verbatimTextOutput("ttestRslt")
                )
        ), #<----- here
                h3("U-test"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "utestX",
                                label = "(X) :",
                                c("")),
                    selectInput(inputId = "utestY",
                                label = "(Y) :",
                                c("")),
                  ),
                  mainPanel(
                    verbatimTextOutput("utestRslt")
                  )
        ), #<----- here
                h3("Paired test"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "PtestX",
                                label = "(X) :",
                                c("")),
                    selectInput(inputId = "PtestY",
                                label = "(Y) :",
                                c("")),
                  ),
                  mainPanel(
                    verbatimTextOutput("ptRslt")
                  )
        ), #<----- here
                h3("Chi Squared tests"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "cstestX",
                                label = "(X) :",
                                c("")),
                    selectInput(inputId = "cstestY",
                                label = "(Y) :",
                                c("")),
                  ),
                  mainPanel(
                    verbatimTextOutput("cstestRslt")
                  )
        ), #<----- here
              h3("Goodness of Fit test"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "gftestX",
                              label = "(X) :",
                              c("")),
                  selectInput(inputId = "gftestY",
                              label = "(Y) :",
                              c("")),
                ),
                mainPanel(
                  verbatimTextOutput("gftRslt")
                )
        ), #<----- here
    ),
    # 1.T-test
    # 2.U-test
    # 3.Paired tests
    # 4.Chi Squared tests
    # 5.Goodness of Fit test
    ###################################################################################################
    tabPanel("Data Visualization", ## First Type
             pageWithSidebar(
               headerPanel('Plots'),
               sidebarPanel(
                 #p("Test"),
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
                #tags$p(strong("Download Plot as png"))#,
                # downloadButton("savePlot", "Save Plot")
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
    noChar <- df[, !sapply(df, is.character)]
    updateSelectInput(session, inputId = 'norm_p', label = 'select column:',
                      choices = names(noChar), selected = names(noChar)[1])
    
    listMiss <- names(which(colSums(is.na(df))>0))
    updateSelectInput(session, inputId = 'imptMissVal', label = 'select column:',
                      choices = listMiss, selected = listMiss[1])

    ####### Data Mining ####
    updateSelectInput(session, inputId = 'fltrCol', label = 'Filter By:',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'selCol', label = 'Select Cloumn',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'Argn', label = 'Arrange By',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'sumVal', label = 'Value',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'Grpby', label = 'Name :',
                      choices = names(df), selected = names(df)[1])                
    updateSelectInput(session, inputId = 'olName', label = 'Old Value',
                      choices = names(df), selected = names(df)[1])
    ## STATISTICAL TEST
    updateSelectInput(session, inputId = 'anovaY', label = 'Dependent Variable (X) :',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'anovaX', label = 'Independent Variable (Y) :',
                      choices = names(noChar), selected = names(noChar)[1])

    updateSelectInput(session, inputId = 'ttestY', label = '(Y) :',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'ttestX', label = '(X) :',
                      choices = names(noChar), selected = names(noChar)[1])

    updateSelectInput(session, inputId = 'utestY', label = '(Y) :',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'utestX', label = '(X) :',
                      choices = names(noChar), selected = names(noChar)[1])

    updateSelectInput(session, inputId = 'PtestY', label = '(Y) :',
                      choices = names(noChar), selected = names(noChar)[1])
    updateSelectInput(session, inputId = 'PtestX', label = '(X) :',
                      choices = names(noChar), selected = names(noChar)[1])

   updateSelectInput(session, inputId = 'cstestY', label = '(Y) :',
                      choices = names(noChar), selected = names(noChar)[1])
   updateSelectInput(session, inputId = 'cstestX', label = '(X) :',
                      choices = names(noChar), selected = names(noChar)[1])

   updateSelectInput(session, inputId = 'gftestY', label = '(Y) :',
                      choices = names(noChar), selected = names(noChar)[1])
   updateSelectInput(session, inputId = 'gftestX', label = '(X) :',
                      choices = names(noChar), selected = names(noChar)[1])

  })
  
  mtydat <- reactive({
    return(dat())
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

  ### Statistical Test ###
  output$ANOVArslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
      datt <- dat()
      aov.model <- aov(datt[[input$anovaY]] ~ datt[,input$anovaX], data = datt) # Stuck here
      #print(aov.model)
      br()
      print(summary(aov.model))
      br()
      # cat("Coefficients"); cat("\n")
      print(aov.model$coefficients)
  })

  output$ttestRslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    datt <- dat()
    x <- input$ttestX
    y <- input$ttestY
    t.test(datt[,x], datt[,y])
  })

  output$utestRslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    datt <- dat()
    x <- input$utestX
    y <- input$utestY
    wilcox.test(datt[,x], datt[,y])
  })

  output$ptRslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    datt <- dat()
    x <- input$PtestX
    y <- input$PtestY
    t.test(datt[,x], datt[,y], paired = TRUE, var.equal = TRUE) 
  })

  output$cstestRslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    datt <- dat()
    data <- datt[, !sapply(datt, is.character)]
    
    x <- input$cstestX
    y <- input$cstestY
    csTwo <- chisq.test(datt[,x], datt[,y])
    print(csTwo)
    print("ALL Columns")
    csAll <- chisq.test(data)
    csAll
  
    # cs[,x]
  })

  output$gftRslt <- renderPrint({
    if(is.null(dat())){
      return ()
      }
    datt <- dat()
    data <- datt[, !sapply(datt, is.character)]
    
    x <- input$gftestX
    y <- input$gftestY
    gfitTwo <- chisq.test(datt[,x], datt[,y], p = ratio, rescale.p = TRUE)
    print(gfitTwo)
    print("ALL Columns")
    gfitAll <- chisq.test(data, p = ratio, rescale.p = TRUE)
    gfitAll
    # gfit[,x]
  })
  ########################
  ## DATA MINING #####

  ### FILTER ###
  observeEvent(input$fltrCol, {
    if(is.null(dat())){
      return ()
    }
    datf <- dat()
    x <- datf %>% select(!!sym(input$fltrCol))
    updateSelectInput(session, inputId = 'fltrVal', label = 'Value',
                      choices = x, selected = x[1])
  })
  
  output$Filter <- DT::renderDataTable({
    datt <- dat()
    dtPoint <- input$fltrCol
    tryCatch({
      if (input$fltrBolean=="=="){
        datt <- datt[datt[,dtPoint] == input$fltrVal,]
      } else if (input$fltrBolean=="!="){
        datt <- datt[datt[,dtPoint] != input$fltrVal,]
      } else if (input$fltrBolean==">"){
        datt <- datt[datt[,dtPoint] > input$fltrVal,]
      } else if (input$fltrBolean=="<"){
        datt <- datt[datt[,dtPoint] < input$fltrVal,]
      }
    })
    return(datt)
  })
  ### FILTER ###
  ### SELECT ###
  output$Select <- DT::renderDataTable({
      datt <- dat()
      dtPoint <- input$selCol
      
      DT::datatable(datt[, dtPoint, drop = FALSE])
    })
  ### SELECT ###
  ### ARRANGE ###
  output$Arrange <- DT::renderDataTable({
    if(is.null(dat())){
      return ()
    }
    datt <- dat()
    dtPoint <- input$Argn
    DT::datatable(datt %>% arrange(datt[,dtPoint]))
  })
  ### ARRANGE ###
  ####MUTATE ####
  observeEvent(input$mutApply,{ #HERE
    datt <- dat()
    colnam <- input$mutName
    forml <- input$mutForml
    
    DATT <- "datt"
    COLNAM <- "colnam"
    FORML <- "forml"
    
    output$Mutate <- DT::renderDataTable({ 
      datUpdate <- eval(parse(text = paste0(DATT, "%>% mutate(", eval(parse(text = COLNAM)), "=", eval(parse(text = FORML)),")")))
      DT::datatable(datUpdate)
    })
  })
  ####MUTATE ####
  ### SUMMARISE ###
  output$Summarise <- renderPrint({
    if(is.null(dat())){
      return ()
    }
    datt <- dat()
    datt <- na.omit(datt)
    #tryCatch({
    if (input$sumFunc=='smrMean'){
      datt %>% summarise_at(input$sumVal, mean)
    } else if (input$sumFunc=='smrMedian') {
      datt %>% summarise_at(input$sumVal, median)
    } else if (input$sumFunc=='smrVar') {
      datt %>% summarise_at(input$sumVal, var)
    } else if (input$sumFunc=='smrNum') {
      datt %>% summarise_at(input$sumVal, length)
    } else if (input$sumFunc=='smrSD') {
      datt %>% summarise_at(input$sumVal, sd)
    } else if (input$sumFunc=='smrIqr') {
      datt %>% summarise_at(input$sumVal, IQR)
    } else if (input$sumFunc=='smrMin') {
      datt %>% summarise_at(input$sumVal, min)
    } else if (input$sumFunc=='smrMax') {
      datt %>% summarise_at(input$sumVal, max)
    } else if (input$sumFunc=='smrAll') {
      dtpoint <- input$sumVal
      summary(datt[,dtpoint])
    }
    #})
    # return(datt)
  })
  ### SUMMARISE ###
  ####GROUP BY ####
  output$Groupby <- DT::renderDataTable({
    if(is.null(dat())){
      return ()
    }
    datt <- dat()
    dtPoint <- input$Grpby
    
    datt %>% group_by(datt[,dtPoint])
  })
  ####GROUP BY ####
  #### RENAME ####
  observeEvent(input$rnmApply, { #HERE
    datt <- dat()
    ol <- input$olName
    nw <- input$nwName
    
    DATT <- "datt"
    OL <- "ol"
    NW <- "nw"
    
    
    #datUpd <- rename(datt, nw = ol)
    # print(datUpd)

    output$Rename <- DT::renderDataTable({ 
      datUpd <- eval(parse(text = paste0(DATT, "%>% rename(", eval(parse(text = NW) ), "=", eval(parse(text = OL)),")")))
      DT::datatable(datUpd)
    })
  })
  #### RENAME ####

  #################### 

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
  
  observeEvent(input$avg_p, {
    dataUpdate <- dat()
    df1 <- na.omit(dataUpdate)
    ft <- input$imptMissVal
    avg <- mean(df1[,ft])
    dataUpdate[,ft][is.na(dataUpdate[,ft])] <- avg
    
    dat(dataUpdate)
  })
   observeEvent(input$rplc_p, {
    dataUpdate <- dat()
    ft <- input$imptMissVal
    dataUpdate[,ft][is.na(dataUpdate[,ft])] <- 0
    
    dat(dataUpdate)
  })
 
   observeEvent(input$normlz_p, {
     dataUpdate <- dat()
     ft <- input$norm_p
     dataPoint <- dataUpdate[,ft]
     
     process <- preProcess(as.data.frame(dataPoint), method=c("range"))
     norm_scale <- predict(process, as.data.frame(dataPoint))
     
     dataUpdate[,ft] <- norm_scale
     
     dat(dataUpdate)
  })
   observeEvent(input$std_p, {
     dataUpdate <- dat()
     ft <- input$norm_p
     dataUpdate[,ft] <- scale(dataUpdate[,ft])
     
     dat(dataUpdate)
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
  } #,height = 400, width = 600
  )
  
  # output$savePlot <- downloadHandler(
  #   filename = "plot.png",
  #   content = function(file) {
  #     device <- function(..., width, height) {
  #       grDevices::png(..., 
  #                      width = width, 
  #                      height = height,
  #                      res = 300, 
  #                      units = "in"
  #                      ) 
  #     }
  #     ggsave(file, plot = dat$plot, device = device)
  #   }
  # )
})

###################################################################################################
shinyApp(ui, server)