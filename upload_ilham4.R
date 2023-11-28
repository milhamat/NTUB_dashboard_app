library(shiny)
library(datasets)
library(ggplot2)
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
                 verbatimTextOutput("summary"),
                 ## Table Out
                 DT::dataTableOutput("table")
                 #tableOutput('contents')
               )
             )
    ),
    ###################################################################################################
    ## Tabs
    tabPanel("Data Preprocessing",
             titlePanel("Preprocess"),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons('impute', 'Impute Missing Value',
                                c("Average/Most Frequent"="avg",
                                  "Replace with Random Value"="rplc",
                                  "Remove row with missing values"="rmv"
                                ),
                                '"'),
                   radioButtons('norm', 'Normalize',
                                c("Standardize"="std",
                                  "Normalize to interval [-1, 1]"="normin"
                                ),
                                '"'),
                   
                 ),
                 mainPanel(
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
                                "Continuous Function" = "conti_func",
                                "Visualizing Error" = "vis_err",
                                "Maps" = "maps"
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
                                "Label" = "geom_label",
                                "Point" = "geom_point",
                                "Quantile" = "geom_quantile",
                                "Rug" = "geom_rug",
                                "Smooth" = "smooth",
                                "Text" = "geom_text"
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
                                "Dotplot" = "goem_dotplot",
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
                                "Desity 2d" = "geom_desity_2d",
                                "Hex" = "geom_hex"
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
                ## Vis Error
                conditionalPanel(
                  condition = "input.two_var_pick == 'vis_err' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "vis_err_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Cross Bar" = "geom_crossbar",
                                "Error Bar" = "geom_errorbar",
                                "Line Range" = "geom_linerange",
                                "Point Range" = "geom_pointrange"
                    )
                  )
                ),
                ## maps
                conditionalPanel(
                  condition = "input.two_var_pick == 'maps' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "maps_pick",
                    label = "Plot Options",
                    choices = c("-" = "-",
                                "Maps" = "geom_map"
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
  ## added "session" because updateSelectInput requires it
  
  dataUpdate <- reactiveVal()
  
  ## Initialize the dataset
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
    updateSelectInput(session, inputId = 'zcol', label = 'Z Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ymin', label = 'Ymin',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ymax', label = 'Ymax',
                      choices = names(df), selected = names(df)[1])
    #choices = names(df), selected = names(df)[2])
    #print(df)
    
    return(df)
  })
  
  #dataUpdate <- observe({
  #  dat <- data()
  #  return(dat)
  #})
  
  ## Showing Raw Data Summary
  output$summary <- renderPrint({
    if(is.null(data())){
      return ()
      }
    summary(data())
  })
  
  ## 
  output$datainfo <- renderPrint({
    if(is.null(data())){
      return ()
      }
    str(data())
  })
  
  ## Showing Raw Data Table
  output$table <- DT::renderDataTable({
    dat <- data()
    tryCatch({
      dat
    })
    return(dat)
  })
  
  observeEvent(input$impute, {
    dataUpdate <- data()
    if (input$impute=="rmv"){
      print(na.omit(dataUpdate))
      #dat <- na.omit(dat)
    } else if (input$impute=="avg") {
      print(dataUpdate[is.na(dataUpdate)] <- 0)
    }
    #data(dat)
    # print(input$impute)
  })
  
  # observeEvent(input$norm, {
  #   print(input$norm)
  #   if (input$norm=="std"){
      
  #   } else if (input$norm=="normin") {
       
  #   }
  # })
  #test <- reactive({
  #  if (input$impute=="avg"){
  #    print("Average selected")
  #  } else if (input$impute=="rplc") {
  #    print("Replace Random Value selected")
  #  }
  #})
  #test()
  
  ## For Plotting
  output$MyPlot <- renderPlot({
    ### Important for updating the layout
    ### we need to reinitialize the dataset
    dat <- data()[, c(input$xcol, input$ycol, input$zcol)]
    ### ONE VARIABLE
    ## conti
    conti <- input$conti_pick
    disct <- input$disct_pick 
    if (input$n_var=="one_var"&&input$one_var_pick=="conti"){
      if (conti=="geom_histogram"){ # geom_histogram
        ggplot(dat, aes_string(x=input$xcol))+geom_histogram(colour='darkblue')
      } else if (conti=="geom_density"){ # geom_density
        ggplot(dat, aes_string(x=input$xcol))+geom_density(colour='darkblue')
      } else if (conti=="geom_area_one") { # geom_area_one
        ggplot(dat, aes_string(x=input$xcol))+geom_area(stat="bin")
      } else if (conti=="geom_dotplot") { # geom_dotplot
         ggplot(dat, aes_string(x=input$xcol))+geom_dotplot()
      } else if (conti=="geom_freqpoly") { # geom_freqpoly
         ggplot(dat, aes_string(x=input$xcol))+geom_freqpoly()
      } else if (conti=="geom_qq") { # geom_qq
         ggplot(dat, aes_string(sample=input$xcol))+stat_qq()
      } 
    } else if (input$n_var=="one_var"&&input$one_var_pick=="disct") {
      if (disct=="geom_bar"){ ## disct # geom_bar
        ggplot(dat, aes_string(x=input$xcol))+geom_bar()
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="both_conti"){
      if (input$both_conti_pick=="geom_point"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_point(colour='darkblue')
      } else if (input$both_conti_pick=="geom_label"){
        ggplot(dat, aes(x=input$xcol, y=input$ycol, label=rownames(dat)))+geom_label() #NO
      } else if (input$both_conti_pick=="geom_quantile") {
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_quantile()
      } else if (input$both_conti_pick=="geom_rug"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_rug(sides = "bl")
      } else if (input$both_conti_pick=="smooth"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_smooth()
      } else if (input$both_conti_pick=="geom_text"){
        ggplot(dat, aes(x=input$xcol, y=input$ycol, label=rownames(dat)))+geom_text() #NO
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="one_dist_one_conti"){
      if (input$one_dist_one_pick=="geom_boxplot"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_boxplot()
      } else if (input$one_dist_one_pick=="geom_violin"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_violin()
      } else if (input$one_dist_one_pick=="geom_col"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_col()
      } else if (input$one_dist_one_pick=="geom_dotplot"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_dotplot(binaxis = "y", stackdir = "center") #NO
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="both_dist"){
      if (input$both_dist_pick=="geom_count"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_count()
      } else if (input$both_dist_pick=="geom_jitter"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_jitter()
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="conti_bivar_dist"){
      if (input$conti_bivar_dist_pick=="geom_bin2d"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_bin_2d()
      } else if (input$conti_bivar_dist_pick=="geom_desity_2d"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_density_2d()
      } 
      #else if (input$conti_bivar_dist_pick=="geom_hex"){
        #ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_hex() #NO
      #}
    } else if (input$n_var=="two_var"&&input$two_var_pick=="conti_func"){
      if (input$conti_func_pick=="geom_area_two"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_area()
      } else if (input$conti_func_pick=="geom_line"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_line()
      } else if (input$conti_func_pick=="geom_step"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_step()
      }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="vis_err") {
       if (input$vis_err_pick=="geom_crossbar") {
         ggplot(dat, aes_string(x=input$xcol, y=input$ycol, ymin=input$ymin, ymax=input$ymax))+geom_crossbar(fatten=1)
       } else if (input$vis_err_pick=="geom_errorbar") {
         ggplot(dat, aes_string(x=input$xcol, y=input$ycol, ymin=input$ymin, ymax=input$ymax))+geom_errorbar()
       } else if (input$vis_err_pick=="geom_linerange") {
         ggplot(dat, aes_string(x=input$xcol, y=input$ycol, ymin=input$ymin, ymax=input$ymax))+geom_linerange()
       } else if (input$vis_err_pick=="geom_pointrange") {
         ggplot(dat, aes_string(x=input$xcol, y=input$ycol, ymin=input$ymin, ymax=input$ymax))+geom_pointrange()
       }
    } else if (input$n_var=="two_var"&&input$two_var_pick=="maps"){
      print("Maps")
      #if (input$maps_pick=="geom_map"){
        #ggplot(dat, aes_string(x=input$xcol, y=input$ycol))+geom_map() #NO
      #}
    } else if (input$n_var=="three_var"){
      if (input$three_var_pick=="geom_contour"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_contour()
      } else if (input$three_var_pick=="geom_contour_filled"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_contour_filled()
      } else if (input$three_var_pick=="geom_raster"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_raster()
      } else if (input$three_var_pick=="geom_tile"){
        ggplot(dat, aes_string(x=input$xcol, y=input$ycol, z=input$zcol))+geom_tile()
      }
    }
    
    #NOTE
    ### TWO VARIABLE
    ## both_conti_pick
    # geom_label NO
    # geom_text NO
    
    ## one_dist_one_pick
    # geom_dotplot NO
  
    ## conti_bivar_dist_pick
    # geom_hex NO

    ## maps_pick
    # geom_map NO

    # geom_crossbar NO
    # geom_errorbar NO
    # geom_linerange NO
    # geom_pointrange NO

  } ,height = 400, width = 600
  )
  
})
###################################################################################################
shinyApp(ui, server)