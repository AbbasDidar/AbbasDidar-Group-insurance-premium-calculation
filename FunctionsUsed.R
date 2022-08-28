

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

library( readxl )
library( dplyr )

X4_emp = X5_employers = read_excel("X5_employers.xlsx")

length( unique( X4_emp$N_code ) )
mean( X4_emp$loss )
sum( X4_emp$loss )
sd( X4_emp$loss )


dim( X4_emp )
dim( X4_emp )[1] / length( unique( X4_emp$N_code ) )


data.frame( CODE = c(1:12,14,17:21) ,
            MEAN_TOTAL = tapply( X4_emp$loss , X4_emp$L_code , mean ) ,
            SUM_TOTAL = tapply( X4_emp$loss , X4_emp$L_code , sum ) ,
            TEDAD_TOTAL = tapply( X4_emp$loss , X4_emp$L_code , length ) )


val =  which( ( (X4_emp$Kinship == "ظ¾ط¯ط±") | (X4_emp$Kinship == "ظ…ط§ط¯ط±") )  ) 

X4_Other = X4_emp[  -val  , ]

length( unique( X4_Other$N_code ) )
mean( X4_Other$loss )
sum( X4_Other$loss )
sd( X4_Other$loss )

dim( X4_Other )
dim( X4_Other )[1] / length( unique( X4_Other$N_code ) )


data.frame( table( X4_Other$Kinship ) )

data.frame( tapply( X4_Other$loss , X4_Other$Kinship , sum ) )

data.frame(  tapply( X4_Other$loss , X4_Other$Kinship , mean ) )

data.frame(  tapply( X4_Other$loss , X4_Other$Kinship , sd ) )

data.frame(  tapply( X4_Other$loss , X4_Other$Kinship , length ) / table( distinct( X4_Other[ , c("N_code","Kinship")] )[,2] ) )



data.frame( CODE = c(1:12,14,17:21) ,
            MEAN_Other= tapply( X4_Other$loss , X4_Other$L_code , mean ) ,
            SUM_Other = tapply( X4_Other$loss , X4_Other$L_code , sum ) ,
            TEDAD_Other = tapply( X4_Other$loss , X4_Other$L_code , length ) )


X4_Other %>% select(  N_code , Kinship ) %>% distinct() %>%  group_by( Kinship ) %>% summarise(n())


X4_Val = X4_emp[ val , ]

length( unique( X4_Val $N_code ) )
mean( X4_Val$loss )
sum( X4_Val$loss )
sd( X4_Val$loss )

dim( X4_Val )
dim( X4_Val )[1] / length( unique( X4_Val$N_code ) )

data.frame( CODE = c(1:12,14,17:21) ,
            MEAN_Val= tapply( X4_Val$loss , X4_Val$L_code , mean ) ,
            SUM_Val = tapply( X4_Val$loss , X4_Val$L_code , sum ) ,
            TEDAD_Val = tapply( X4_Val$loss , X4_Val$L_code , length ) )



Liability_table <- read.table( "Liability_table.txt" )



wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , sum ) ) )



png(filename = "MEAN_LOSS.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)

barplot( c(mean( X4_Val$loss ),mean( X4_Other$loss ),mean( X4_emp$loss )) , col = c(7,5,6) ,
         names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹") , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظ†ظپط±")


dev.off()


png(filename = "SUM_LOSS.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)

barplot( c(sum( X4_Val$loss ),sum( X4_Other$loss ),sum( X4_emp$loss )) , col = c(7,5,6) ,
         names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹") , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ")


dev.off()






png(filename = "SUM_LOSS_VAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Val$loss , X4_Val$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "SUM_LOSS_OTHER.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Other$loss , X4_Other$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "SUM_LOSS_TOTAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_emp$loss , X4_emp$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط³ط±ط¬ظ…ط¹" , col = rainbow(20) )


dev.off()


png(filename = "MEAN_LOSS_VAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Val$loss , X4_Val$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "MEAN_LOSS_OTHER.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Other$loss , X4_Other$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "MEAN_LOSS_TOTAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_emp$loss , X4_emp$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط³ط±ط¬ظ…ط¹" , col = rainbow(20) )


dev.off()



wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , sum ) ) )

wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , sum ) ) )


wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , sum ) ) )


wordcloud2( table(  X4_emp$L_sub  ) )
wordcloud2( table(  X4_Val$L_sub  ) )
wordcloud2( table(  X4_Other$L_sub  ) )




#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%
#%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%##%#%#%#%


X4_emp  = read_excel("D:\\Rating_project\\4.xlsx" , sheet = "en_DATA")

length( unique( X4_emp$N_code ) )
mean( X4_emp$loss )
sum( X4_emp$loss )
sd( X4_emp$loss )


dim( X4_emp )
dim( X4_emp )[1] / length( unique( X4_emp$N_code ) )




val =  which( ( (X4_emp$Kinship == "ظ¾ط¯ط±") | (X4_emp$Kinship == "ظ…ط§ط¯ط±") )  ) 

X4_Other = X4_emp[  -val  , ]

length( unique( X4_Other$N_code ) )
mean( X4_Other$loss )
sum( X4_Other$loss )
sd( X4_Other$loss )

dim( X4_Other )
dim( X4_Other )[1] / length( unique( X4_Other$N_code ) )

X4_Val = X4_emp[ val , ]

length( unique( X4_Val $N_code ) )
mean( X4_Val$loss )
sum( X4_Val$loss )
sd( X4_Val$loss )

dim( X4_Val )
dim( X4_Val )[1] / length( unique( X4_Val$N_code ) )



Liability_table <- read.table( "Liability_table.txt" )



wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , sum ) ) )



png(filename = "MEAN_LOSS.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)

barplot( c(mean( X4_Val$loss ),mean( X4_Other$loss ),mean( X4_emp$loss )) , col = c(7,5,6) ,
         names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹") , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ظ†ظپط±")


dev.off()


png(filename = "SUM_LOSS.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)

barplot( c(sum( X4_Val$loss ),sum( X4_Other$loss ),sum( X4_emp$loss )) , col = c(7,5,6) ,
         names.arg = c("ظˆط§ظ„ط¯غŒظ†","ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†","ط³ط±ط¬ظ…ط¹") , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ")


dev.off()






png(filename = "SUM_LOSS_VAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Val$loss , X4_Val$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "SUM_LOSS_OTHER.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Other$loss , X4_Other$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "SUM_LOSS_TOTAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_emp$loss , X4_emp$L_code , sum ) , main = "ظ…ط¬ظ…ظˆط¹ ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط³ط±ط¬ظ…ط¹" , col = rainbow(20) )


dev.off()


png(filename = "MEAN_LOSS_VAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Val$loss , X4_Val$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "MEAN_LOSS_OTHER.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_Other$loss , X4_Other$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط¨ط¬ط² ظˆط§ظ„ط¯غŒظ†" , col = rainbow(20) )

dev.off()


png(filename = "MEAN_LOSS_TOTAL.png",res = 500 ,
    width = 10.4, height = 8, units = "in", pointsize = 12,
    bg = "white", restoreConsole = TRUE)
barplot( tapply( X4_emp$loss , X4_emp$L_code , mean ) , main = "ظ…غŒط§ظ†ع¯غŒظ† ط®ط³ط§ط±طھ ط¨ظ‡ ط§ط²ط§غŒ ظ‡ط± ع©ط¯ ظ¾ظˆط´ط´ : ط³ط±ط¬ظ…ط¹" , col = rainbow(20) )


dev.off()



wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_Val$loss , X4_Val$L_sub , sum ) ) )

wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_Other$loss , X4_Other$L_sub , sum ) ) )


wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , mean ) ) )
wordcloud2( as.table( tapply( X4_emp$loss , X4_emp$L_sub , sum ) ) )


wordcloud2( table(  X4_emp$L_sub  ) )
wordcloud2( table(  X4_Val$L_sub  ) )
wordcloud2( table(  X4_Other$L_sub  ) )






