
# Extract the importance values for each local classifer using importance of RandomForest- mean decrease in accuracy
Hie_Importance=function(Train_Data_Ready,
                        Exp_Var_2,
                        LRF_Info,
                        All_Local_RF,
                        scale_Imp=FALSE,   # if FALSE, variable importance is not scaled by the standard error.
                        ...)
{ # start function
  
  require(randomForest)
  
  # create the iImp_Val_Table data frame
  Imp_Val_Table           <- matrix(nrow=dim(LRF_Info)[1], 
                                    ncol=length(Exp_Var_2))
  Imp_Val_Table           <- as.data.frame(Imp_Val_Table)
  colnames(Imp_Val_Table) <- colnames(Train_Data_Ready[Exp_Var_2])
  
  # create the Imp_Var_4_col data frame
  Imp_Var_4_Column <- cbind(LRF_Info[1,c(1,3)],
                         colnames(Train_Data_Ready[Exp_Var_2]),
                         row.names = NULL)
  
  for (k in 2:dim(LRF_Info)[1])
  { k1               <- cbind(LRF_Info[k,c(1,3)],
                              colnames(Train_Data_Ready[Exp_Var_2]),
                              row.names = NULL)
    Imp_Var_4_Column <- rbind(Imp_Var_4_Column,
                              k1)
  }
  
  Imp_Var_4_Column                <- as.data.frame(Imp_Var_4_Column)
  Imp_Var_4_Column[,4]            <- NA
  colnames(Imp_Var_4_Column)[3:4] <- c("Expl_Var","Mean_Dec_Accu")
     i=2                      
  for (i in 1: dim(LRF_Info)[1]) # loop the runs on each local classifer
  {
    #Extract the importance values for the local clasifer 
    Local_RF      <- All_Local_RF[[i]]$Local_RF # the focal local randomForest object
    Local_Imp     <- as.data.frame(importance(Local_RF,scale=scale_Imp))  # the importance data frame for the local RF  
    Mean_Dec_Accu <- Local_Imp$MeanDecreaseAccuracy # the mean decrease in accuracy
    
    # insert the importance values in the two output data frames
    Imp_Val_Table[i,]                          <- Mean_Dec_Accu[]
    Imp_Var_4_Column[c((1+length(Exp_Var_2)*(i-1)):(i*length(Exp_Var_2))),4] <- Mean_Dec_Accu[]
  }
  
  # add a column with Classifier_ID to the Imp_Val_table data frame
  Imp_Val_Table              <- cbind(LRF_Info[,1], Imp_Val_Table)
  colnames(Imp_Val_Table)[1] <- "Classifier_ID"
  
  # create the return list
  
  list(Imp_Val_Table    = Imp_Val_Table,
       Imp_Var_4_Column = Imp_Var_4_Column)
  
} # end Hie_Importance function