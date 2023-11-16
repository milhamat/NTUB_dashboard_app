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
                 ## Table Out
                 tableOutput('contents')
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
                    choices = c(
                                "Contour" = "geom_contour", 
                                "Contour Filled" = "geom_contour_filled",
                                "Raster" = "geom_raster",
                                "Tile" = "geom_tile"
                    )
                  )
                ),
                ### ONE VAR
                ## conti Pick
                conditionalPanel(
                  condition = "input.one_var_pick == 'conti' && input.n_var != 'two_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "conti_pick",
                    label = "Plot Options",
                    choices = c(
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
                    choices = c(
                                "Bar Plot" = "geom_bar"
                    )
                  )
                ),
                ### TWO VAR
                ## both conti
                conditionalPanel(
                  condition = "input.two_var_pick == 'both_conti' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "both_conti_pick",
                    label = "Plot Options",
                    choices = c(
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
                    choices = c(
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
                    choices = c(
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
                    choices = c(
                                "bin2d" = "geom_bin2d",
                                "desity 2d" = "geom_desity_2d",
                                "hex" = "geom_hex"
                    )
                  )
                ),
                ## conti func
                conditionalPanel(
                  condition = "input.two_var_pick == 'conti_func' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "conti_func_pick",
                    label = "Plot Options",
                    choices = c(
                                "area" = "geom_area_two",
                                "line" = "geom_line",
                                "step" = "geom_step"
                    )
                  )
                ),
                ## maps
                conditionalPanel(
                  condition = "input.two_var_pick == 'maps' && input.n_var != 'one_var' && input.n_var != 'three_var'",
                  selectInput(
                    inputId = "maps_pick",
                    label = "Plot Options",
                    choices = c(
                                "maps" = "geom_map"
                    )
                  )
                ),
                 ## "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 
                             'X Variable', 
                             ""),
                 conditionalPanel(
                  condition = "input.n_var != 'one_var'",
                    selectInput('ycol', 
                                'Y Variable', 
                                "", 
                                selected = ""
                    )
                  ),
                 conditionalPanel(
                  condition = "input.n_var == 'three_var'",
                    selectInput('zcol', 
                                'Z Variable', 
                                "", 
                                selected = ""
                    )
                  )
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
###################################################################################################
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
    updateSelectInput(session, inputId = 'zcol', label = 'Z Variable',
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

    ## ONE VARIABLE
    ## TWO VARIABLE
    ## THREE VARIABLE
    
    if (input$conti_pick=="geom_histogram"){
      dat %>%
        ggplot(aes_string(x=input$xcol))+geom_histogram(colour='darkblue')
    } 
    # else if (input$select_plot=="geom_density"){
    #   dat %>%
    #     ggplot(aes_string(x=input$xcol))+geom_density(colour='darkblue')
    # } else if (input$select_plot=="geom_boxplot"){
    #   dat %>%
    #     ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_boxplot(colour='darkblue')
    # } else if (input$select_plot=="geom_violin"){
    #   dat %>%
    #     ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_violin(colour='darkblue')
    # } else if (input$select_plot=="geom_point"){
    #   dat %>%
    #     ggplot(aes_string(x=input$xcol, y=input$ycol))+geom_point(colour='darkblue')
    # }
  },height = 400,width = 600
  )
})
###################################################################################################
shinyApp(ui, server)