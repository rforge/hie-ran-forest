
# takes as input the list generated  by Hie_Importance
# usaes the four column format list with classifer name, parent name, variable name and importance as the four columns
# plots a n*k table, with n being the number of parent node (number of local classifers) and k being the number of explanatory variables
# for each combination of parent node and variable, a circle is ploted.
# the size of the circle represent the value ofmean decrease in accuracy
# the colour of the circles represent the sign of the mean decreaes in accuracy
# white - positive
# yellow - exactly zero
# light grey -  negative 

Plot_Cat_Cat_Var = function(Input_Data,                                        # data frame containing the importance data, each row is for a specific combination of local classifer and explanatory variable. default format is assumed to be the 4 column format data frame of Hie_Importance
                            X_Data    = 2,                                     # integer, the column number that contains yje data for the X axis
                            Y_Data    = 3,                                     # integer, the column number that contains the data for the y axis
                            Imp_Data  = 4,                                     # the column number that contains the importance data for tthe tiles or bubbles
                            Plot_Type = "Tile",                                # Character, meaningfull values are "Tile" (default) or "Bubble", the type of plot that will be returned. "tile" returns a geom tile, with colur of the tiel representing the mean decrease in accuracy. 
                            Imp_title = colnames(Input_Data)[Imp_Data],        # Character - the title to use for the Importance legend
                            X_Title   = colnames(Input_Data)[X_Data],          # Character, title for the x axis
                            Y_Title   = colnames(Input_Data)[Y_Data],          # Character, title for the y axis
                            low_col   = "blue" ,                               # Charcther, if type=='Tile', the colour to use for the highest importance value                 
                            high_col  = "red",                                 # Charcther, if type=='Tile', the colour to use for the lowest importance value    
                            Geom_Tile_Bor_Col = "white",                       # Charcther, the color for theif type=='Tile', the colour to use for the border of tiles. SEt to NA for no tile borders.
                            Pos_Col   = "green",                               # Charcther, if type=='Bubble', the colour to use for the  importance values >0          
                            Zero_col  = "white",                               # Charcther, if type=='Bubble', the colour to use for the importance values ==0          
                            Neg_Col   = "red",                                 # Charcther, if type=='Bubble', the colour to use for the  importance value <0          
                            ...)
{ # start the function
  require(ggplot2)
  
  ################### 
  ## Arrange data  ##
  ###################
  
  
  # define the local environment
  localenv <- environment()
 
  # check Plot_Type
  if(Plot_Type!="Tile" && Plot_Type!="Bubble")
  { stop("\nPlot_Type can be either 'Tile' or 'Bubble'\n")}
  
  # create the work data according to users input
  Work_Data           <- Input_Data[,c(X_Data,Y_Data,Imp_Data)]
  colnames(Work_Data) <- c("X_Var","Y_Var","Imp_Var")
  
  # factor the x and y
  Work_Data$X_Var <- factor(Work_Data$X_Var, 
                            as.character(Work_Data$X_Var))
  
  Work_Data$Y_Var <- factor(Work_Data$Y_Var, 
                            as.character(Work_Data$Y_Var))

 ################################# 
 ## plot when Plot_Type=="Tile" ##
 #################################
 
   if(Plot_Type=="Tile")
   { p <- ggplot(Work_Data,
                 aes(X_Var,Y_Var),
                 environment = localenv) # without the environment argument, ggplot2 will look for Work_Data in the global environment
     p <- p + geom_tile(aes(fill = Work_Data$Imp_Var),
                        color=Geom_Tile_Bor_Col)
     p <- p + xlab(X_Title) 
     p <- p + ylab(Y_Title)  
     p <- p + scale_fill_continuous(low = low_col, 
                                    high = high_col,
                                    guide = "colourbar",
                                    guide_colourbar(title =Imp_title))
   } # end plot "Tile"
 
 ################################### 
 ## plot when Plot_Type=="Bubble" ##
 ###################################
 
   if(Plot_Type=="Bubble")
   {
     # add the sign column
     for (k3 in 1:dim(Work_Data)[1])
       {
       if(Work_Data[k3,3]>0 )  {Work_Data[k3,4] <- "Positive"}
       if(Work_Data[k3,3]==0 ) {Work_Data[k3,4] <- "Zero"}
       if(Work_Data[k3,3]<=0 ) {Work_Data[k3,4] <- "Negative"}
       }
     colnames(Work_Data)[4] <- "Sign_Imp"
     
     # create the colours vector for Scale_Fill_Manual
     fillScaleValues <- c( "Positive" = Pos_Col,
                           "Zero"     = Zero_col,
                           "Negative" = Neg_Col )
     
     # start the ggplot
     p <- ggplot(Work_Data,aes(X_Var,Y_Var),
                 environment = localenv )
     p <- p + geom_point(aes(size = abs(Work_Data$Imp_Var),
                             fill=Work_Data$Sign_Imp),
                         shape=21)
     p <- p + xlab(X_Title) 
     p <- p + ylab(Y_Title)  
     p <- p + scale_size_continuous(guide = "legend",
                                    guide_legend(title =Imp_title))
     p <- p + scale_fill_manual(values = fillScaleValues, 
                                name = "Sign")  
   } # end plot "Bubble"

 #############
 ## Return  ##
 ############# 
 
return(print(p))
  
} # end function Plot_Hie_Imp.R