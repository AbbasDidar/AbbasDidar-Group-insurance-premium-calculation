
library(shiny)
library(shinydashboard)
library( shinydashboardPlus)
library(DT)
library(dplyr)
library(readxl)
library(wordcloud2)
library(tableHTML)

options(shiny.maxRequestSize=100*1024^2)
options( scipen = 99999 )



dashboardHeader( title = span("Basic ", 
                              span("dashboard", 
                                   style = "color: red; font-size: 28px")))


ui = dashboardPage( skin = "purple" ,
                    
                    #------------------------------- Header ----------------------------------------
                    dashboardHeader( title = "ظ…ط­ط§ط³ط¨ظ‡ ط­ظ‚ ط¨غŒظ…ظ‡ ط¯ط±ظ…ط§ظ†"
                    ) ,
                    
                    
                    #------------------------------- Sidebar ---------------------------------------
                    
                    
                    
                    
                    dashboardSidebar( 
                        
                        sidebarUserPanel( "ط´ط±ع©طھ ظ…ط­ط§ط³ط¨ط§طھ ع¯غŒطھغŒ ظ¾ط±ط¯ط§ط²ط´" , image = "gity.jpeg" , subtitle = "") , 
                        
                        
                        tags$head(tags$style(HTML('* {font-family: "B Mitra"};'))) ,
                        tags$style(HTML(".sidebar-menu li a { font-size: 20px;  }")),
                        
                        
                        
                        
                        tags$head( tags$style(HTML(".help-block a {color: black !important;}"))  ) ,
                        
                        tags$head(tags$style(HTML("
                                #final_text {
                                  text-align: right;
                                
                                }
                                div.box-header {
                                  text-align: right;
                                
                                }
                                "))),
                        
                        
                        tags$style(make_css(list('.box',
                                                 c('font-size','font-family' ),
                                                 c('20px','B Mitra')))) ,
                        
                        
                        
                        # change color : #8d988b or name_color
                        
                        tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: plum;}'))) ,
                        
                        sidebarMenu( id = "Menu" ,
                                     
                                     menuItem( tabName = "Tab1" , "طµظپط­ظ‡ ط§طµظ„غŒ" , icon = icon("home")  ) , 
                                     menuItem( tabName = "Tab12" , "ط§ط·ظ„ط§ط¹ط§طھ" , icon = icon("fingerprint")  ) , 
                                     
                                     menuItem( tabName = "Tab2" , "ظ…ظپط±ظˆط¶ط§طھ" , icon = icon("list")  ) , 
                                     
                                     menuItem( tabName = "Tab3" , "ط³ظ‚ظپ ظ¾ظˆط´ط´â€Œظ‡ط§" , icon = icon("window-restore") , 
                                               menuSubItem( tabName = "subTab1" , "ظ¾ظˆط´ط´â€Œظ‡ط§غŒ ع©ظ… ط±غŒط³ع©" ) ,
                                               menuSubItem( tabName = "subTab2" , "ظ¾ظˆط´ط´â€Œظ‡ط§غŒ ظ¾ط± ط±غŒط³ع©" ) )  ,
                                     
                                     menuItem( tabName = "Tab4" , "ظ†ظ…ظˆط¯ط§ط±ظ‡ط§" , icon = icon("pie-chart") ) ,
                                     
                                     menuItem( tabName = "Tab5" , "ط¯ط§ط¯ظ‡" , icon = icon("database") ) 
                                     
                                     
                        )
                    ),
                    
                    #------------------------------- Body ------------------------------------------
                    
                    
                    dashboardBody(
                        
                        
                        
                        tabItems( tabItem( tabName =  "Tab1" ,
                                           
                                           # userBox(
                                           #   title = userDescription(
                                           #     title = "ظ…ط¬طھط¨غŒ ط¹ط§ط¨ط¯",
                                           #     subtitle = "",
                                           #     image = "abed.jpg",
                                           #     backgroundImage = "gal.jfif",
                                           #   ),
                                           #   status = "teal",
                                           #   closable = TRUE,
                                           #   maximizable = TRUE,
                                           #   "ظ…ط¯غŒط±غŒطھ ط¨غŒظ…ظ‡ ظ‡ط§غŒ ط¯ط±ظ…ط§ظ†",
                                           #   footer = ""  , width = 4
                                           # ) ,
                                           
                                           
                                           # 
                                           # userBox(
                                           #   title = userDescription(
                                           #     title = "ظ…ط­ظ…ط¯ط±ط¶ط§ ط¨ط±ط§طھغŒ",
                                           #     subtitle = "",
                                           #     image = "barati.jpg",
                                           #     backgroundImage = "gal.jfif",
                                           #   ),
                                           #   status = "teal",
                                           #   closable = TRUE,
                                           #   maximizable = TRUE,
                                           #   "ع©ط§ط±ط´ظ†ط§ط³ طھظˆط³ط¹ظ‡ ظ…ط­طµظˆظ„ ط¨غŒظ…ظ‡ ظ‡ط§غŒ ط¯ط±ظ…ط§ظ†",
                                           #   footer = ""  , width = 4
                                           # ) ,
                                           
                                           
                                           
                                           
                                           # userBox(
                                           #            title = userDescription(
                                           #              title = "ط¹ط¨ط§ط³ ط¯غŒط¯ط§ط±",
                                           #               subtitle = "",
                                           #               image = "didar.jfif",
                                           #              backgroundImage = "gal.jfif",
                                           #            ),
                                           #            status = "teal",
                                           #            closable = TRUE,
                                           #            maximizable = TRUE,
                                           #            "",
                                           #            footer = "" , width = 4
                                           #           )
                                           #         
                                           #       
                                           # ,
                                           img(src="img1.png" , height = "100%", width = "100%")
                        ) ,
                        
                        
                        tabItem( tabName =  "Tab12" ,
                                 
                                 
                                 
                                 infoBoxOutput("info_Val" , width = 4  ) ,
                                 infoBoxOutput("info_Other" , width = 4  ) ,
                                 infoBoxOutput("info_Total" , width = 4 ),
                                 
                                 infoBoxOutput("info_Val_mean_loss" , width = 4  ) ,
                                 infoBoxOutput("info_Other_mean_loss" , width = 4  ) ,
                                 infoBoxOutput("info_Total_mean_loss" , width = 4 ),
                                 
                                 infoBoxOutput("info_Val_sum_loss" , width = 4  ) ,
                                 infoBoxOutput("info_Other_sum_loss" , width = 4  ) ,
                                 infoBoxOutput("info_Total_sum_loss" , width = 4 ),
                                 
                                 box( title = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظپط±ط¯" , width = 4 ,status = "info" , solidHeader = TRUE ,
                                      plotOutput( "barplot_mean" )  ) , 
                                 box( title = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ " , width = 4 ,status = "info" , solidHeader = TRUE ,
                                      plotOutput( "barplot_sum" )  ) ,
                                 
                                 
                                 
                                 tabBox(
                                     title = "",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     id = "tabset1", width = 4 , 
                                     
                                     tabPanel("ظپط±ط§ظˆط§ظ†غŒ: ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot1") ),
                                     tabPanel("ظپط±ط§ظˆط§ظ†غŒ:ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot2") ),
                                     tabPanel("ظپط±ط§ظˆط§ظ†غŒ:ط³ط±ط¬ظ…ط¹", wordcloud2Output("plot3") )
                                     
                                     
                                 ) 
                                 
                                 
                        ) , 
                        
                        tabItem( tabName =  "Tab2" ,
                                 
                                 
                                 
                                 box( title =  "ط§ظپط²ط§غŒط´ طھط¹ط±ظپظ‡%" ,  
                                      sliderInput("tarrif",
                                                  " " ,
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "danger" , solidHeader = TRUE ) , 
                                 box( title = "طھظˆط±ظ…" ,
                                      sliderInput("interest",
                                                  "",
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "success" , solidHeader = TRUE ) , 
                                 box( title = "ط³ظˆط¯ ط³ط±ظ…ط§غŒظ‡ ع¯ط°ط§ط±غŒ" ,
                                      sliderInput("inflation",
                                                  "",
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "warning" , solidHeader = TRUE ) , 
                                 box( title = "ظ‡ط²غŒظ†ظ‡ ظ‡ط§غŒ ط§ط¯ط§ط±غŒ" , 
                                      sliderInput("loading",
                                                  "",
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "danger" , solidHeader = TRUE )  ,
                                 
                                 box( title = "ط³ظˆط¯ ظ…ظˆط±ط¯ ط§ظ†طھط¸ط§ط±" , 
                                      sliderInput("sood_Sar",
                                                  "",
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "success" , solidHeader = TRUE ) , 
                                 
                                 box( title = "ط§ظپط²ط§غŒط´ ط¨ط§ط± ظ…ط±ط§ط¬ط¹ظ‡%" ,
                                      sliderInput("bar",
                                                  "",
                                                  min = 0,
                                                  max = .99,
                                                  value = .10 ), width = 4 , status = "warning" , solidHeader = TRUE ) , 
                                 
                                 box( numericInput("pension_val",
                                                   "(ط³ط±ظ…ط§غŒظ‡ طµظ†ط¯ظˆظ‚(ظپظ‚ط· ظˆط§ظ„ط¯غŒظ†",
                                                   min = 10^6,
                                                   max = 10^12,
                                                   value = 40000000 , step = 10^6 ), width = 4 ,background = "light-blue")  ,
                                 box( numericInput("pension_other",
                                                   "(ط³ط±ظ…ط§غŒظ‡ طµظ†ط¯ظˆظ‚(ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†",
                                                   min = 10^6,
                                                   max = 10^12,
                                                   value = 100000000 , step = 10^6 ), width = 4 ,background = "light-blue")  ,
                                 box( numericInput("pension_total",
                                                   "(ط³ط±ظ…ط§غŒظ‡ طµظ†ط¯ظˆظ‚(ط³ط±ط¬ظ…ط¹",
                                                   min = 10^3,
                                                   max = 10^20,
                                                   value = 10^8 , step = 10^7 ), width = 4 ,background = "light-blue")  ,
                                 
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ظپظ‚ط· ظˆط§ظ„ط¯غŒظ†" , tableOutput( "Premium1_Val" ) , width = 4 , status = "info" , solidHeader = TRUE ),
                                 box( title = "(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†",   tableOutput( "Premium1_other" ) , width = 4 , status = "info" , solidHeader = TRUE ) ,
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط³ط±ط¬ظ…ط¹"  , tableOutput( "Premium1_Total" ) , width = 4 , status = "info" , solidHeader = TRUE )
                                 
                        ) , 
                        
                        
                        #-------------------------------------------------------------------------------     
                        
                        tabItem( tabName =  "subTab1" ,
                                 
                                 
                                 
                                 box( title = "ظ†ط§ط¨ط§ط±ظˆط±غŒ ظˆ ظ†ط§ط²ط§غŒغŒ 5" ,  numericInput("S_5",
                                                                                                   "",
                                                                                                   min = 10^6,
                                                                                                   max = 10^12,
                                                                                                   value = 60000000 , step = 10^6 ), width = 4 ,background = "light-blue")  ,
                                 
                                 
                                 
                                 box( title = "ظˆغŒظ„ع†ط± 141" , numericInput("S_141",
                                                                              "",
                                                                              min = 10^6,
                                                                              max = 10^12,
                                                                              value = 10000000 , step = 10^6), width = 4  ,background = "light-blue" )  ,
                                 
                                 box( title = "ط§ط±طھط² 142" , numericInput("S_142",
                                                                            "",
                                                                            min = 10^6,
                                                                            max = 10^12,
                                                                            value = 33950000 , step = 10^6), width = 4  ,background = "purple" )  ,
                                 
                                 
                                 box( title = "ط³ظ…ط¹ع© 17" , numericInput("S_17",
                                                                           "",
                                                                           min = 10^6,
                                                                           max = 10^12,
                                                                           value = 15000000 , step = 10^6), width = 4  ,background = "olive" )  ,
                                 
                                 box( title =  "ط¯ط³طھ ط¯ظ†ط¯ط§ظ† ع©ط§ظ…ظ„ 18" ,numericInput("S_18",
                                                                                             "",
                                                                                             min = 10^6,
                                                                                             max = 10^12,
                                                                                             value = 10000000 , step = 10^6), width = 4 ,background = "light-blue")  ,
                                 
                                 box( title = "ظ†غŒظ… ط¯ط³طھ ط¯ظ†ط¯ط§ظ† 19" ,numericInput("S_19",
                                                                                          "",
                                                                                          min = 10^6,
                                                                                          max = 10^12,
                                                                                          value =5000000 , step = 10^6), width = 4 ,background = "purple")  ,
                                 
                                 box( title =  "ع©ط§ط±ط¯ط±ظ…ط§ظ†غŒ" ,numericInput("S_20",
                                                                                  "",
                                                                                  min = 10^6,
                                                                                  max = 10^12,
                                                                                  value = 40000000 , step = 10^6), width = 4 ,background = "olive" )  ,
                                 
                                 box( title =  "ع¯ظپطھط§ط± ط¯ط±ظ…ط§ظ†غŒ" , numericInput("S_21",
                                                                                        "",
                                                                                        min = 10^6,
                                                                                        max = 10^12,
                                                                                        value = 30000000 , step = 10^6), width = 4 ,background = "light-blue" )  ,
                                 
                                 
                                 #-------------------------------------------------------------------------------     
                                 
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ظپظ‚ط· ظˆط§ظ„ط¯غŒظ†" , tableOutput( "Premium2_Val" ) , width = 5 , status = "danger" , solidHeader = TRUE ),
                                 box( title = "(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", tableOutput( "Premium2_other" ) , width = 5, status = "danger" , solidHeader = TRUE ) ,
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط³ط±ط¬ظ…ط¹" , tableOutput( "Premium2_Total" ) , width = 5 , status = "info" , solidHeader = TRUE )
                                 
                                 
                                 
                        ) ,
                        
                        
                        tabItem( tabName =  "subTab2" ,
                                 
                                 
                                 
                                 
                                 box( title =  "ط¨ط§ط±ظ…ط±ط§ط¬ط¹ظ‡ ظ„غŒط²غŒع© 3",
                                      sliderInput("bar_3",
                                                  "" ,
                                                  min = 0,
                                                  max = 3,
                                                  value = .10 , step = .05 ), width = 4 , status = "warning" , solidHeader = TRUE )  ,
                                 
                                 
                                 box( title =   "ط¨ط§ط±ظ…ط±ط§ط¬ط¹ظ‡ ط¯ظ†ط¯ط§ظ† 11",
                                      sliderInput("bar_11",
                                                  "" ,
                                                  min = 0,
                                                  max = 3,
                                                  value = .10 , step = .05 ), width = 4 , status = "warning" , solidHeader = TRUE )  ,
                                 
                                 box( title =  "ط¨ط§ط±ظ…ط±ط§ط¬ط¹ظ‡ ط¹غŒظ†ع© 12",
                                      sliderInput("bar_12",
                                                  "" ,
                                                  min = 0,
                                                  max = 3,
                                                  value = .10  , step = .05 ), width = 4 , status = "warning" , solidHeader = TRUE )  ,
                                 
                                 
                                 
                                 
                                 box( title = "ظ„غŒط²غŒع© 3" ,numericInput("S_3",
                                                                           "",
                                                                           min = 10^6,
                                                                           max = 10^12,
                                                                           value = 20000000 , step = 10^6 ), width = 4 ,background = "olive")  ,
                                 
                                 
                                 
                                 box( title = "11 ط¯ظ†ط¯ط§ظ†" ,numericInput("S_11",
                                                                            "",
                                                                            min = 10^6,
                                                                            max = 10^12,
                                                                            value = 8000000 , step = 10^6 ), width = 4 ,background = "purple" )  ,
                                 
                                 box( title = "ط¹غŒظ†ع© 12" ,numericInput("S_12",
                                                                          "",
                                                                          min = 10^6,
                                                                          max = 10^12,
                                                                          value = 3500000 , step = 10^6), width = 4 ,background = "blue" ) ,
                                 
                                 #-------------------------------------------------------------------------------     
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ظپظ‚ط· ظˆط§ظ„ط¯غŒظ†" , tableOutput( "Premium3_Val" ) , width = 4 , status = "danger" , solidHeader = TRUE ),
                                 
                                 box( title = "(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", tableOutput( "Premium3_other" ) , width = 4, status = "danger" , solidHeader = TRUE ) ,
                                 
                                 box( title ="(ط­ظ‚ ط¨غŒظ…ظ‡ ظ…ط§ظ‡ط§ظ†ظ‡ (ط³ط±ط¬ظ…ط¹" , tableOutput( "Premium3_Total" ) , width = 4 , status = "info" , solidHeader = TRUE )
                                 
                                 
                                 
                        ) ,
                        tabItem( tabName =  "Tab4" ,
                                 
                                 # box( title = "a",
                                 # 
                                 #      highchartOutput( "barplot" )
                                 #          , width = 12 , status = "warning" , solidHeader = TRUE ) ,
                                 
                                 
                                 tabBox(
                                     title = "",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     id = "tabset2", height = "600px", width = 6 , 
                                     
                                     
                                     tabPanel("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ظ‡ط§: ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot4") ),
                                     tabPanel("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ظ‡ط§:ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot5") ),
                                     tabPanel("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ظ‡ط§:ط³ط±ط¬ظ…ط¹", wordcloud2Output("plot6") )
                                     
                                     
                                 ) , 
                                 
                                 tabBox(
                                     title = "",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     id = "tabset3", height = "600px", width = 6 , 
                                     
                                     tabPanel("", wordcloud2Output("plot777") ),
                                     tabPanel("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ظ‡ط§: ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot7") ),
                                     tabPanel("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ظ‡ط§:ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", wordcloud2Output("plot8") ),
                                     tabPanel("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ظ‡ط§:ط³ط±ط¬ظ…ط¹", wordcloud2Output("plot9") )
                                     
                                 ) 
                                 
                                 
                                 
                                 
                                 
                        ) ,
                        
                        
                        tabItem( tabName =  "Tab5" , dataTableOutput( "Table1" ) ) 
                        
                        )
                        
                    )
                    
                    
                    
                    
)
