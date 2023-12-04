ui <- function(){
  
  bootstrapPage('',
                titlePanel("國立台北商業大學 校務永續發展中心"),
                navbarPage(title = 'Test Title',
                           img(src="campus_logo.png", height=80, width=300),
                           
                           tabPanel("Test 1",
                                    tabsetPanel(
                                      tabPanel("Test Tab 1",
                                               titlePanel("Test "),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   br(),# this break has making gap
                                                   radioButtons("plotType", "Plot type",
                                                                c("Scatter"="p", "Line"="l")
                                            )
                                      ),
                                      mainPanel(
                                        plotOutput("plot")
                                      )
                                    )
                                  ),
                                  tabPanel("Test Tab 2",
                                           titlePanel("Test 2"),
                                           sidebarLayout(
                                             sidebarPanel(
                                               radioButtons("plotType", "Plot type",
                                                            c("Test"="p", "Test2"="l")
                                               )
                                             ),
                                             mainPanel(
                                               plotOutput("plot")
                                             )
                                           )
                                  )
                                )
                              ),
                           tabPanel("Test 2",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("plotType", "Plot type",
                                                     c("Scatter"="p", "Line"="l")
                                        )
                                      ),
                                      mainPanel(
                                        plotOutput("plot")
                                      )
                                    )
                           ),
                           ),
                
                
                tags$style(type = 'text/css', 
                          '.navbar { background-color: #800080;
                           font-family: Arial;
                           font-size: 13px;
                           color: #ffffff; }',
                           
                           '.navbar-dropdown { background-color: #800080;
                           font-family: Arial;
                           font-size: 13px;
                           color: #ffffff; }', 
                           
                           '.navbar-default .navbar-brand {
                             color: #ffffff;
                           }'
                           
                )
          )
  
}
#800080 #FF0000 #262626 #cc3f3f
server <- function(input, output, session){
}


shinyApp(ui = ui, server = server)