
library(dplyr)
library(readxl)
library(wordcloud2)

#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%


PREMIUM = function( X4_emp , Liability_table , tarrif , interest , inflation ,loading , bar , 
                    sood_Sar , pension , lisik_bar , dandan_bar, eynak_bar ){
    
    
    X4_emp = na.omit( X4_emp ) 
    X4_emp$loss = (1+tarrif) * X4_emp$loss
    X4_emp$compensation = (1+tarrif) * X4_emp$compensation
    
    # comulative based on National code
    
    
    X_CODE.14 = X4_emp[ X4_emp$L_code == 14 , ]
    
    X4_emp[ X4_emp$L_code == 14 , "L_code" ] = ifelse( X_CODE.14$L_sub ==  unique( X_CODE.14$L_sub )[3] , 141 , 142 )
    
    
    
    
    
    
    X4_com.N.code = X4_emp %>% 
        select( loss , L_code , N_code  ) %>% 
        group_by( L_code , N_code ) %>% 
        summarise_all( sum )
    
    
    X4_com.N.code = as.data.frame( X4_com.N.code  )
    
    
    
    
    #-------------------------------------------------------------------------------
    
    
    U_L.code = unique( X4_emp$L_code )
    
    
    U_N.code = unique( X4_emp$N_code )
    
    LOSS_LIMIT = c()
    
    for( i in 1:length( U_L.code ) ){
        
        if( (U_L.code[i] == 12) | (U_L.code[i] == 141)  ){
            
            
            LIMIT = Liability_table$Limit[ U_L.code[i] == Liability_table$Liab_code ]
            
            LOSS_LIMIT = rbind( LOSS_LIMIT , 
                                cbind( LOSS_with_Limit = .90 * ifelse( X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] > LIMIT ,
                                                                       LIMIT ,  X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] ) , 
                                       LIMIT , LOSS_without_Limit = X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] , LIAB_CODE =U_L.code[i] ) )
            
            
        }else{ 
            if( (U_L.code[i] == 142)  ){ 
                
                
                LIMIT = Liability_table$Limit[ U_L.code[i] == Liability_table$Liab_code ]
                
                LOSS_LIMIT = rbind( LOSS_LIMIT , 
                                    cbind( LOSS_with_Limit = .70 * ifelse( X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] > LIMIT ,
                                                                           LIMIT ,  X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] ) , 
                                           LIMIT , LOSS_without_Limit = X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] , LIAB_CODE =U_L.code[i] ) )
                
                
                
                
            }else{ 
                
                LIMIT = Liability_table$Limit[ U_L.code[i] == Liability_table$Liab_code ]
                
                LOSS_LIMIT = rbind( LOSS_LIMIT , 
                                    cbind( LOSS_with_Limit = ifelse( X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] > LIMIT ,
                                                                     LIMIT ,  X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] ) , 
                                           LIMIT , LOSS_without_Limit = X4_com.N.code$loss[ X4_com.N.code$L_code == U_L.code[i]  ] , LIAB_CODE =U_L.code[i] ) )
            }
            
        }
        
        
        
    }
    
    
    MEAN = cbind( LOSS_with_Limit = tapply( LOSS_LIMIT[,1] , LOSS_LIMIT[,4] , sum ) , 
                  LIMIT = tapply( LOSS_LIMIT[,2] , LOSS_LIMIT[,4] , mean ) , 
                  LOSS_without_Limit = tapply( LOSS_LIMIT[,3] , LOSS_LIMIT[,4] , sum ) ,
                  Liab_code = sort( U_L.code ) )
    
    COVER_BAR = rep( 1 , dim( MEAN )[1] ) 
    COVER_BAR[c(3,11,12)] = c( (1+lisik_bar) , (1+dandan_bar) , (1+eynak_bar)  )
    
    MEAN[,1] = COVER_BAR*MEAN[,1]
    MEAN[,3] = COVER_BAR*MEAN[,3]
    
    Yearly_Premium = ( ( 1+bar )*( 1+.10 )*sum( MEAN[,1] )  - pension ) / length( unique( X4_emp$N_code ) )
    
    
    i_p = ( (1+interest)^(1/12)-1 )*12
    a_f = (1-(1+interest)^(-1)) / i_p
    
    j_p = ( (1+inflation)^(1/12)-1 )*12
    s_f = ( (1+inflation)^1-1 ) / j_p
    
    
    
    
    if( ( i_p != 0 ) & ( j_p != 0 ) ){
        
        Monthly_Premium = round( (1+loading+sood_Sar) * Yearly_Premium / (s_f*a_f*12) )
        
    }else{
        
        Monthly_Premium = round( (1+loading+sood_Sar) * Yearly_Premium / (12) )
        
    }
    
    
    
    
    ###############################################################
    # LLL = data.frame( Liab_code = X4_emp$L_code , LOSS =X4_emp$loss  )
    # 
    # 
    # M = merge( LLL , Liability_table , by ="Liab_code" )
    # 
    # # mean( M$LOSS > M$Limit )
    # 
    # G_LIMIT = M[ ( M$LOSS > M$Limit ) , ]
    # 
    # Upper_LIMIT = data.frame( Mean = tapply( G_LIMIT$LOSS , G_LIMIT$Liab_code , sum ) )    
    
    
    ###############################################################
    
    
    
    return( list( Monthly_Premium = Monthly_Premium , Summary = MEAN )  )
    #return( Monthly_Premium )
}



#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%



X4_emp = X5_employers = read_excel("X5_employers.xlsx")
set.seed(11)
X4_emp = X4_emp[ sample(200000,20000) , ]


val =  which( ( (X4_emp$Kinship == "ظ¾ط¯ط±") | (X4_emp$Kinship == "ظ…ط§ط¯ط±") )  ) 
X4_Other = X4_emp[  -val  , ]
X4_Val = X4_emp[ val , ]



Liability_table <- read.table( "Liability_table.txt" )



#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%






# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    output$info_Val <- renderInfoBox({
        infoBox("طھط¹ط¯ط§ط¯ ظˆط§ظ„ط¯غŒظ†", length( unique( X4_Val$N_code ) ) ,icon = icon("hourglass-end", lib = "font-awesome"),
                color = "aqua", fill = TRUE )
    })
    output$info_Other <- renderInfoBox({
        infoBox("طھط¹ط¯ط§ط¯ ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", length( unique( X4_Other$N_code ) ) ,icon = icon("hourglass", lib = "font-awesome"),
                color = "aqua", fill = TRUE )
    })
    output$info_Total <- renderInfoBox({
        infoBox("طھط¹ط¯ط§ط¯ ع©ظ„", length( unique( X4_emp$N_code ) ) ,icon = icon("user", lib = "font-awesome"),
                color = "aqua", fill = TRUE )
    })
    
    
    output$info_Val_mean_loss <- renderInfoBox({
        infoBox("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظ†ظپط± : ظˆط§ظ„ط¯غŒظ†", round( mean( X4_Val$loss ) )  , icon = icon( "money-bill-wave", lib = "font-awesome"),
                color = "yellow", fill = TRUE )
    })
    output$info_Other_mean_loss <- renderInfoBox({
        infoBox("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظ†ظپط± : ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", round( mean( X4_Other$loss ) ) ,icon = icon("money-bill-wave", lib = "font-awesome"),
                color = "yellow", fill = TRUE )
    })
    output$info_Total_mean_loss <- renderInfoBox({
        infoBox("ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظ†ظپط± : ط³ط±ط¬ظ…ط¹", round( mean( X4_emp$loss ) ) ,icon = icon("money-bill-wave", lib = "font-awesome"),
                color = "yellow", fill = TRUE )
    })
    
    output$info_Val_sum_loss <- renderInfoBox({
        infoBox("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ : ظˆط§ظ„ط¯غŒظ†", sum( X4_Val$loss ) ,icon = icon("comments-dollar",lib = "font-awesome"),
                color = "purple", fill = TRUE )
    })
    output$info_Other_sum_loss <- renderInfoBox({
        infoBox("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ: ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†", sum( X4_Other$loss ) ,icon = icon("comments-dollar", lib = "font-awesome"),
                color = "purple", fill = TRUE )
    })
    output$info_Total_sum_loss <- renderInfoBox({
        infoBox("ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ع©ظ„", sum( X4_emp$loss ) ,icon = icon("comments-dollar", lib = "font-awesome"),
                color = "purple", fill = TRUE )
    })
    
    output$barplot_mean = renderPlot( barplot( c(mean( X4_Val$loss ),mean( X4_Other$loss ),mean( X4_emp$loss )) , col = c(7,5,6) , names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹")) )
    output$barplot_sum = renderPlot( barplot( c(sum( X4_Val$loss ),sum( X4_Other$loss ),sum( X4_emp$loss )) , col = c(7,5,6) , names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹")) )
    RESULT1 <- reactive({ PREMIUM( X4_Val , Liability_table = data.frame( Liab_code = Liability_table$Liab_code , 
                                                                          Limit = c( 10^13 , 10^13,  input$S_3 , 10^13 , input$S_5 , rep(10^13,5) , input$S_11 , input$S_12 ,  10^13 , input$S_141 , input$S_142 , 10^13 , 10^13  , input$S_17 , input$S_18 , input$S_19 , input$S_20 , input$S_21 )  )
                                   , tarrif = input$tarrif , bar = input$bar , sood_Sar = input$sood_Sar , 
                                   interest = input$interest , inflation = input$inflation , loading = input$loading ,
                                   lisik_bar = input$bar_3 , dandan_bar = input$bar_11 , eynak_bar = input$bar_12 , pension = input$pension_val ) })
    
    # output$p1 = renderPlot( barplot( t( RESULT1()[[1]][,c(1,3)] ) , beside = TRUE , col = c(6,7) , names.arg = rownames(  RESULT1()[[1]][,c(1,3)]  ) ,
    #                                  horiz = FALSE , legend.text = c("with limit","without limit") ) )
    # 
    # output$p2 = renderPlot( pie(  RESULT1()[[1]][,2] ,  col = c("purple", "violetred1", "green3",
    #                                                     "cornsilk", "cyan", "white")  ) )
    
    
    # output$Summary_TABLE = renderDataTable( RESULT()[[1]] )
    
    
    RESULT2 <- reactive({ PREMIUM( X4_Other , Liability_table = data.frame( Liab_code = Liability_table$Liab_code ,
                                                                            Limit = c( 10^13 , 10^13,  input$S_3 , 10^13 , input$S_5 , rep(10^13,5) , input$S_11 , input$S_12 ,  10^13 , input$S_141 , input$S_142 , 10^13 , 10^13  , input$S_17 , input$S_18 , input$S_19 , input$S_20 , input$S_21 )  )
                                   , tarrif = input$tarrif , bar = input$bar , sood_Sar = input$sood_Sar ,
                                   interest = input$interest , inflation = input$inflation , loading = input$loading ,
                                   lisik_bar = input$bar_3 , dandan_bar = input$bar_11 , eynak_bar = input$bar_12 , pension = input$pension_other ) })
    
    
    
    RESULT3 <- reactive({ PREMIUM( X4_emp , Liability_table = data.frame( Liab_code = Liability_table$Liab_code ,
                                                                          Limit = c( 10^13 , 10^13,  input$S_3 , 10^13 , input$S_5 , rep(10^13,5) , input$S_11 , input$S_12 ,  10^13 , input$S_141 , input$S_142 , 10^13 , 10^13  , input$S_17 , input$S_18 , input$S_19 , input$S_20 , input$S_21 )  )
                                   , tarrif = input$tarrif , bar = input$bar , sood_Sar = input$sood_Sar ,
                                   interest = input$interest , inflation = input$inflation , loading = input$loading ,
                                   lisik_bar = input$bar_3 , dandan_bar = input$bar_11 , eynak_bar = input$bar_12 , pension = input$pension_total ) })
    
    
    output$Premium1_Val = renderText ( as.numeric( RESULT1()[[1]] ) ) 
    output$Premium1_other = renderText( as.numeric( RESULT2()[[1]] ) ) 
    output$Premium1_Total = renderText( as.numeric( RESULT3()[[1]] ) ) 
    
    
    
    
    
    
    
    output$Premium2_Val = renderText( as.numeric( RESULT1()[[1]] ) ) 
    output$Premium2_other = renderText( as.numeric( RESULT2()[[1]] ) ) 
    output$Premium2_Total = renderText( as.numeric( RESULT3()[[1]] ) ) 
    
    
    output$Premium3_Val = renderText( as.numeric( RESULT1()[[1]] ) ) 
    output$Premium3_other = renderText( as.numeric( RESULT2()[[1]] ) ) 
    output$Premium3_Total = renderText( as.numeric( RESULT3()[[1]] ) ) 
    
    
    
    
    # output$barplot = renderHighchart({
    # 
    #     options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
    # 
    #     D = data.frame( Loss = c( RESULT3()[[2]][,1] ,  RESULT3()[[2]][,3] ) , Liab_code = as.factor( rep(  RESULT3()[[2]][,4] , 2 ) ) ,
    #                     GROUP = rep( c("LOSS_with_Limit" , "LOSS_without_Limit") , each = dim( RESULT3()[[2]] )[1]  ) )
    # 
    # 
    #     hc = D %>%
    #         hchart('column', hcaes(x = 'Liab_code', y = 'Loss', group = 'GROUP')) %>%
    #         hc_colors(c("red", "purple"))
    #     
    # 
    #     })
    
    # output$w1 = renderWordcloud2( wordcloud2( table( X4_emp$L_sub )  ) )
    # output$w2 = renderWordcloud2( wordcloud2( table( X4_emp$L_sub )  ) )
    
    
    
    output$plot1 = renderWordcloud2( wordcloud2( table( X4_Val$L_sub )  ) )
    output$plot2 = renderWordcloud2( wordcloud2( table( X4_Other$L_sub )  ) )
    output$plot3 = renderWordcloud2( wordcloud2( table( X4_emp$L_sub )  ) )
    
    output$plot4 = renderWordcloud2( wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , mean ) ) ) )
    output$plot5 = renderWordcloud2( wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , mean ) ) ) )
    output$plot6 = renderWordcloud2( wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , mean ) ) ) )
    
    output$plot7 = renderWordcloud2( wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , sum ) ) ) )
    output$plot8 = renderWordcloud2( wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , sum ) ) ) )
    output$plot9 =renderWordcloud2( wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , sum ) ) ) )
    
    
    # 
    output$Table1 = renderDataTable( X4_emp[1:1000,-3])
    #     
    # 
    #     
    # 
    # output$contents <- renderTable({
    #     
    #     # input$file1 will be NULL initially. After the user selects
    #     # and uploads a file, head of that data file by default,
    #     # or all rows if selected, will be shown.
    #     
    #     req(input$file1)
    #     
    #     # when reading semicolon separated files,
    #     # having a comma separator causes `read.csv` to error
    #     tryCatch(
    #         {
    #             df <- read.csv(input$file1$datapath,
    #                            header = input$header,
    #                            sep = input$sep,
    #                            quote = input$quote)
    #         },
    #         error = function(e) {
    #             # return a safeError if a parsing error occurs
    #             stop(safeError(e))
    #         }
    #     )
    #     
    #     if(input$disp == "head") {
    #         return(head(df))
    #     }
    #     else {
    #         return(df)
    #     }
    #     
    #    
    #     
    #     
    # })
    # 
    
    # output$Table1 = DT::renderDataTable({
    #     DT::datatable(RESULT()[[1]])       
    # })
    
    
    
    
})


