#' Mutltiple flat and hierarchical performance measures for crisp classifcation.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param Hie_RF            Object of class Hier.Random.Forest - the output of
#'   Run_HRF.
#' @param Per_Index         The performance and accuracy indices to compute.See
#'   details below.
#' @param Crisp_Rule        The method of translating proportion of votes to a
#'   crisp category. See details below.
#' @param Perm_Num          Integer,number of random permutations for each case
#'   if 'Multiplicative_Permutation' is applied.
#' @param By_Node           Logical, if TRUE performances indices will be
#'   estimated for each terminal node as well as for the overall confusion
#'   matrix.
#' @param Div_Logical       Logical, if TRUE progress when
#'   'Multiplicative_Permutation' is applied will be printed every Div_Print
#'   permutations
#' @param Div_Print         Ser above
#' @param Beta_H_F          Numeric in the range Beta_H_F>=0. Controls weights
#'   in the hierarchical F measure index. See Hie_F_Measure for details.
#' @param ...               Optional parameters to be passed to the low level
#'   functions.
#'   




# function, returns mutltiple flat and hierarchical performance measures for a crisp classifcation. Crispt classifcation may be based on multiplicative majority, stepwise majority or multiplicative permutations 


HRF_Performance = function(Hie_RF,                                         
                           Per_Index = c("Flat_Measures","Hie_F_Measure"),      
                           Crisp_Rule  = c("Multiplicative_Majority","Multiplicative_Permutation","Setpwise_Majority"), # Wether the multiplicative majority rule shoudl be used to convert proportion of votes to classifed node or wether permutation based accuracy  should be estimated.
                           Perm_Num = 500,                                 # Integer, number of random votes to take for each case
                           By_Node = TRUE,                                 # logical, indicating whether several indices for each node should be returned, if TRUE, the indices will appear in Hie_Performance after the overall performance measures 
                           Div_Logical = TRUE,                             # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                           Div_Print = 25,
                           Beta_H_F = 1,
                           ...)
   
 { # Start function
   
   # required packages
   require(randomForest)
   require(e1071)
   require(caret)
   require(reshape)
   
   ######################################################################################
   ### STEP 1 - PERFORM CHECKS                                                        ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n", "-->  Veryfing Call","\n",sep=""))
   
   # check class of Hie_RF
   if(class(Hie_RF)!="Hier.Random.Forest")
   {stop(paste("\n","In HRF_Perfomance:  Hie_RF should be of class Hier.Random.Forest","\n",sep=""))}
   
   # check Per_Index
   Temp_Vec <-c("Flat_Measures","Hie_F_Measure") 
   
   if(length(intersect(Per_Index,Temp_Vec))<1)
   {stop(paste("\n","In HRF_Perfomance, atleast one valid option for Per_Index is required", "\n",sep=""))}
   
   if(length(intersect(Per_Index,Temp_Vec))>0)
   {cat(paste("\n", "Performance will be evaluated using: ",Per_Index,"\n",sep=""))}
   
   rm(Temp_Vec)
  
   # check Crisp_Rule
   
   Temp_Vec <-c("Multiplicative_Majority","Multiplicative_Permutation","Setpwise_Majority") 
   
   if(length(intersect(Crisp_Rule,Temp_Vec))<1)
   {stop(paste("\n","In HRF_Perfomance, atleast one valid option for Crisp_Rule is required", "\n",sep=""))}
   
   if(length(intersect(Crisp_Rule,Temp_Vec))>0)
   {cat(paste("\n", "Performance will be evaluated using crisp classification based on: ",Crisp_Rule,"\n",sep=""))}
   
   # check Perm_Num
   if(!is.numeric(Perm_Num))
   {stop(paste("\n","In HRF_Perfomance, Perm_Num should be a positive integer", "\n",sep=""))}
   
   if(Perm_Num<1)
   {stop(paste("\n","In HRF_Perfomance, Perm_Num should be a positive integer", "\n",sep=""))}
   
   if(round(Perm_Num,0)!=Perm_Num)
   {stop(paste("\n","In HRF_Perfomance, Perm_Num should be a positive integer", "\n",sep=""))}
   
   # Check By_Node
   if(!is.logical(By_Node))
   {cat(paste("\n", "In HRF_Perfomance, By_Node should be Logical. default of By_Node=TRUE is used","\n",sep=""))
    By_Node <- TRUE}
   
   # Check Div_Logical
   if(!is.logical(Div_Logical))
   {cat(paste("\n", "In HRF_Perfomance, Div_Logical should be Logical. default of Div_Logical=TRUE is used","\n",sep=""))
    Div_Logical <- TRUE}
   
   # check Div_Print
   if(!is.numeric(Div_Print))
   {cat(paste("\n","In HRF_Perfomance, Div_Print should be a positive integer", "\n",
              "Default of Div_Print=25 is used","\n",sep=""))
    Div_Print <- 25 }
   
   if(Div_Print<1)
   {cat(paste("\n","In HRF_Perfomance, Div_Print should be a positive integer", "\n",
              "Default of Div_Print=25 is used","\n",sep=""))
    Div_Print <- 25 }
   
   if(round(Div_Print,0)!=Div_Print)
   {cat(paste("\n","In HRF_Perfomance, Div_Print should be a positive integer", "\n",
              "Default of Div_Print=25 is used","\n",sep=""))
    Div_Print <- 25 }
   
   # check Beta_H_F
   if(!is.numeric(Beta_H_F))
   {cat(paste("\n","In HRF_Perfomance, Beta_H_F should be a positive number", "\n",
              "Default of Beta_H_F=1 is used","\n",sep=""))
    Beta_H_F <- 1    }
   
   if(Beta_H_F<0)
   {cat(paste("\n","In HRF_Perfomance, Beta_H_F should be non-negative", "\n",
              "Default of Beta_H_F=1 is used","\n",sep=""))
    Beta_H_F <- 1    }
   
   cat(paste("\n", "-->  Call Verified","\n",sep=""))
  
   ######################################################################################
   ### STEP 2 - CREATE LOGICAL ARGUMENTS FOR EACH OPTION OF Per_Index and Crisp_Rule    ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
 
      # Multiplicative Majority rule
   if(!is.na(match("Multiplicative_Majority",Crisp_Rule))) # the Multiplicative_Majority rule was called by the user
   { Mult_Maj_Rule_Logical <- TRUE  }
   if(is.na(match("Multiplicative_Majority",Crisp_Rule))) # the Multiplicative_Majority rule was not called by the user
   { Mult_Maj_Rule_Logical <- FALSE }  
   
      # Multiplicative_Permutation
   if(!is.na(match("Multiplicative_Permutation",Crisp_Rule))) # the Multiplicative_Permutation was called by the user
   { Mult_Perm_Logical     <- TRUE  } 
   if(is.na(match("Multiplicative_Permutation",Crisp_Rule))) # the Multiplicative_Permutation was not called by the user
   { Mult_Perm_Logical     <- FALSE }  
   
      # Setpwise_Majority
   if(!is.na(match("Setpwise_Majority",Crisp_Rule))) # the Setpwise_Majority was called by the user
   { Step_Maj_Rule_Logical <- TRUE  } 
   if(is.na(match("Setpwise_Majority",Crisp_Rule))) # the Setpwise_Majority was not called by the user
   { Step_Maj_Rule_Logical <- FALSE }
   
      # Flat_Measures
   if(!is.na(match("Flat_Measures",Per_Index))) # the Flat_Measures was called by the user
   { Flat_Measures_Logical <- TRUE  }
   if(is.na(match("Flat_Measures",Per_Index))) # the Flat_Measures was not called by the user
   { Flat_Measures_Logical <- FALSE }  
   
      # Hie_F_Measure
   if(!is.na(match("Hie_F_Measure",Per_Index))) # the Hie_F_Measure was called by the user
   { Hie_F_Measure_Logical <- TRUE  }
   if(is.na(match("Hie_F_Measure",Per_Index))) # the Hie_F_Measure was not called by the user
   { Hie_F_Measure_Logical <- FALSE }  
   
   # add additional logical variables here if new crisp rules and/or measurees are added
   
   
   ######################################################################################
   ### STEP 3 - ARRANGE TERMINAL NODES FOR THE TRAINING DATA                          ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n","-->  Preparing the observed terminal nodes ","\n",sep=""))
   
   # extract info from the Hie_RF object
   cat(paste("\n","Extracting info from Hie_RF ","\n",sep=""))
   Train_Data_Ready  <- Hie_RF$Train_Data_Ready
   Case_ID_2         <- Hie_RF$Case_ID_2
   Hie_Levels_2      <- Hie_RF$Hie_Levels_2
   End_Path_Name     <- Hie_RF$call$End_Path_Name
   Unique_Path       <- Hie_RF$Hier_Struc$Unique_Path
   
   # prepare the Train_Data_acc data frame
   Train_Data_Acc <- subset(Train_Data_Ready,select=Case_ID_2)
   Train_Data_Acc$Obs_Term_Node <- NA 
   
   cat(paste("\n","Evaluating terminal node for each case ","\n",sep=""))
   
   # Get the terminal node of each case
   for (K2 in 1:nrow(Train_Data_Ready))
   {Focal_Path      <- Train_Data_Ready[K2,Hie_Levels_2] # the column with the hierarchical data for case k2
    Focal_Term_Node <- Get_Terminal_Node(End_Path_Name = End_Path_Name,                  #  Character, the name used to represent the End_Path
                                         Unique_Path = Focal_Path,                    #  data frame, specifying all the pathes from the tree root to terminal nodes. with each path from tree root as a row,and each column as a depth in the hierarchy
                                         Level_Depth=ncol(Focal_Path)-2)
    
    Train_Data_Acc[K2,"Obs_Term_Node"] <- Focal_Term_Node$Term_Node_Name 
   }
   
   # remove objects used in the k2 loop
   rm(list=c("K2","Focal_Path","Focal_Term_Node"))
   
   # order according to
   cat(paste("\n","Ordering according to Case_ID ","\n",sep=""))
   
   Train_Data_Acc <- Train_Data_Acc[order(Train_Data_Acc[,1]),]
   
   # all the terminal nodes found in the Train_Data_Acc
   Unique_Nodes   <- unique(Train_Data_Acc$Obs_Term_Node)
   
   cat(paste("\n","-->  Observed terminal nodes ready,  ","\n",sep=""))
      
   ######################################################################################
   ### STEP 4 - CREATE THE RETURN DATA FRAME                                          ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n","-->  Createing the Crisp_Rule by Per_Index data frames","\n",sep=""))
   
   cat(paste("\n","Identifying indices called by user","\n",sep=""))
   ## Add the relevant measures columns                        ##
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
    
   Hie_Performance <-data.frame(Crisp_Rule     = c(NA))
   
   if(Flat_Measures_Logical)
   { ### the columns from confusionMatrix$overall
     Hie_Performance$Accuracy       <- NA
     Hie_Performance$Kappa          <- NA 
     Hie_Performance$AccuracyLower  <- NA 
     Hie_Performance$AccuracyUpper  <- NA 
     Hie_Performance$AccuracyNull   <- NA 
     Hie_Performance$AccuracyPValue <- NA 
     Hie_Performance$McnemarPValue  <- NA    }
   
   if(Hie_F_Measure_Logical)
   { # the columns for the overall hierarchical F measure
     Hie_Performance$H_Precision  <- NA
     Hie_Performance$H_ReCall     <- NA
     Hie_Performance$H_F_Measure  <- NA      }
   
   ### add additional overall measures here ###
   
   
   
   if(By_Node)
   { # Start the By_Node condition
    if(Flat_Measures_Logical) 
    { # the columns for the individual nodes measures of Accuracy
      
      # The columns names according to the output of confusionMatrix$byClass
      Nodes_Acc_Ind <- c("Sensitivity","Specificity","Pos Pred Value","Neg Pred Value","Prevalence","Detection Rate","Detection Prevalence","Balanced Accuracy")
      
      # create a data frame with all pairs of nodes and indices
      Nodes_Measures                 <- expand.grid(Unique_Nodes,Nodes_Acc_Ind)
      colnames(Nodes_Measures)       <- c("Term_Node","Index")
      Nodes_Measures$Term_Node_Index <- paste(Nodes_Measures$Term_Node,
                                              Nodes_Measures$Index,
                                              sep=";")
      
      Hie_Performance[,Nodes_Measures$Term_Node_Index] <- NA
      # update the return data frame
      Nodes_Measures_columns <- Nodes_Measures    }
      
      #
    
    if(Hie_F_Measure_Logical)
    { # The columns names for the hierarchical F measures
      Nodes_H_Acc_Ind <- c("N_H_Precision","N_H_ReCall","N_H_F_Measure")
      
      # create a data frame with all pairs of nodes and indices
      Nodes_H_Measures                 <- expand.grid(Unique_Nodes,Nodes_H_Acc_Ind)
      colnames(Nodes_H_Measures)       <- c("Term_Node","Index")
      Nodes_H_Measures$Term_Node_Index <- paste(Nodes_H_Measures$Term_Node,
                                              Nodes_H_Measures$Index,
                                              sep=";")
      
      Hie_Performance[,Nodes_H_Measures$Term_Node_Index] <- NA
      # update the return data frame
      Nodes_Measures_columns <- Nodes_H_Measures    }
    
    #  add additional By_node measures here if needed
    
    
    # update the return data frame
    if(Flat_Measures_Logical && Hie_F_Measure_Logical)
    {Nodes_Measures_columns <- rbind(Nodes_Measures,Nodes_H_Measures)}
    
   } # End the By_Node condition
   
   
   
   ## start the data frame for each crisp rule                 ##
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat(paste("\n","Starting output data frames for each crisp rule","\n",sep=""))
   
   if(Mult_Maj_Rule_Logical)
   { Hie_Perf_Mult_Maj <- Hie_Performance
     Hie_Perf_Mult_Maj[1,"Crisp_Rule"] <- "Multiplicative_Majority_Rule" }
   
   if(Mult_Perm_Logical)
   { Hie_Perf_Mult_Perm <- Hie_Performance
     Hie_Perf_Mult_Perm[c(1:Perm_Num),"Crisp_Rule"] <-c(paste("Perm_",1:Perm_Num,sep=""))  }
   
   if(Step_Maj_Rule_Logical)
   { Hie_Perf_Step_Maj <- Hie_Performance
     Hie_Perf_Step_Maj[1,"Crisp_Rule"] <- "Setpwise_Majority"  }
   
   ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
   ####                                                                   ####
   ##                                                                       ##
   #    Add here addtional Hie_Perf data frames if new methods are added     #
   ##                                                                       ##
   ####                                                                   ####         
   ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
   
   cat(paste("\n","-->  Crisp_Rule by Per_Index data frames created","\n",sep=""))
   cat("######################")
   
   ######################################################################################
   ### STEP 5 - PREPARING THE TERMINAL NODES AS PREDICTED BY THE HRF                  ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat(paste("\n","-->  Preparing the predicted terminal nodes from the hierarchical RandomForest ","\n",sep=""))
   
   # Extract the votes, defaults are used for all other arguments 
   Raw_Votes   <- Extract_Votes(Hie_RF = Hie_RF) 
   focal_Votes <- Raw_Votes$Prop_Vote_Train
   
   # the data frame that collects all the crisp rules
   Crisp_Case_Class <- focal_Votes[,c(1,2)] 
   
   # Treat the multiplicative majority and/or permutations
   if(Mult_Maj_Rule_Logical || Mult_Perm_Logical)
   { # start the crisp rules that relay on multiplicative probabilites
     cat(paste("\n","Estimating the Multiplicative probabilities down the hierarchical tree ","\n",sep=""))
     
     
     Multiplicative_Prop_Last_level <- Multiplicative_Prop_Votes(Prop_Vote   = focal_Votes,
                                                                  Unique_Path = Unique_Path,
                                                                  All_Levels = FALSE)
     
     Multiplicative_Prop <- Multiplicative_Prop_Last_level[[1]] # the deepest level of the hierarchical nodes tree
     
     # if the user called the multiplicative majority rule
     if(Mult_Maj_Rule_Logical)
     {
       cat(paste("\n","Adding the multiplicative majority Rule","\n",sep=""))
       Mult_Maj_Rule <- Multiplicative_Majority_Rule(Prop_Multiplicative_Votes = Multiplicative_Prop,     # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is Train_or_Test and the second is Case_ID
                                                           Bind_Prop_Mul_Maj=FALSE)
       
       Crisp_Case_Class <- cbind(Crisp_Case_Class,Mult_Maj_Rule[3])
       cat(paste("\n","Multiplicative majority Rule added","\n",sep=""))
     }
     
     # if the user called the multiplicative permutations rule
     if(Mult_Perm_Logical)
     {
       cat(paste("\n"," Adding: ",Perm_Num, " permutations","\n",sep=""))
       Mult_Perm_Rule <- Perm_Multiplicative_Term_Node(Multiplicative_Prop_Votes = Multiplicative_Prop,       # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. onr of the outputs of Multiplicative_Majority_Rule function. First column is the Train_or_test, second column is the Case_ID, 
                                                            Perm_Num          = Perm_Num,          # Integer, number of random votes to take for each case 
                                                            Div_Logical       = Div_Logical,       # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                                                            Div_Print         = Div_Print,
                                                            Bind_Prop_Perm    = FALSE)              # logical, if true, the permutated terminal nodes are added at the end of the Multiplicative_Prop_Votes. If FALSE, a seperate data frame is returned, cointating only the Case_ID and the permuted terminal nodes
       Crisp_Case_Class <- cbind(Crisp_Case_Class,Mult_Perm_Rule[3:ncol(Mult_Perm_Rule)])
       
       cat(paste("\n",Perm_Num," Permutations added","\n",sep=""))
     } 
     
     Multiplicative_Prop <- Multiplicative_Prop[order(Multiplicative_Prop[,2]),]
   } # end the crisp rules that relay on multiplicative probabilites
   
   
   # Treat the stepwise majority rule
   if(Step_Maj_Rule_Logical)
   {
     cat(paste("\n","Adding the stepwise majority Rule","\n",sep=""))
     Step_Maj_Rule <-  Stepwise_Majority_Rule(Hie_RF=Hie_RF,
                                              Prop_Vote=focal_Votes,
                                              Bind_Prop_Step_Maj = FALSE) 
     
     Crisp_Case_Class <- cbind(Crisp_Case_Class,Step_Maj_Rule[3])  
     cat(paste("\n","Stepwise majority Rule added","\n",sep=""))
   }
   
   
   # Order according to Case_ID
   cat(paste("\n","Ordering according to Case_ID ","\n",sep=""))
   
   Crisp_Case_Class    <- Crisp_Case_Class[order(Crisp_Case_Class[,2]),]
   
   # check if Case_ID of the training and predicted data match perfectly
   
   if(!all(Train_Data_Acc[,1]==Crisp_Case_Class[,2]))
   { stop(paste("\n","Error in function: HRF_Performance", "\n",
                "Case_ID names (rows) of the training data and predicted data do not match perfectly","\n",
                sep=""))  }
 
   cat(paste("\n","-->  Predicted terminal nodes ready ","\n",sep=""))
   
   
   
   
   ######################################################################################
   ### STEP 6 - ESTIMATING ACCURACY FOR EACH ROW IN Hie_Performance                           ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   cat("######################")
   cat(paste("\n","-->  Start estimating performance measures ","\n",sep=""))
  
   ######################################################
   ####    treat the Multiplicative Majority Rule     ###
   ######################################################
   
   if(Mult_Maj_Rule_Logical)
   { #### start treatment of  the Multiplicative Majority Rule
     
     cat(paste("\n","Estimating performance measures for the multiplicative majority rule ","\n",sep=""))
     
     
     Focal_detail  <- "Multiplicative_Majority_Rule"
     Pred_Nodes    <- Crisp_Case_Class[,match(Focal_detail,colnames(Crisp_Case_Class))] 
     
     # make sure the levels of Pred_Nodes and Obse_Nodes are identical
     Joined_levels <- Join_Levels(Vector_1 = Pred_Nodes,
                                  Vector_2 = Train_Data_Acc$Obs_Term_Node)
     
     Pred_Nodes    <- Joined_levels$Vector_1
     Obse_Nodes    <- Joined_levels$Vector_2
     
     rm(Joined_levels) # remove 
     
     # Use the confusionmatrix function caret package to get the confusion matrix
     Conf_Matr <- confusionMatrix(data=Pred_Nodes,
                                  reference= Obse_Nodes,
                                  dnn = c("Prediction", "Observed"))
     
     Conf_Matr_Table <- Conf_Matr$table
     Conf_Matr_Overall <- as.data.frame(t(Conf_Matr$overall))
     Conf_Matr_ByClass <- Conf_Matr$byClass
     
     # remove the prefix "Class: " from the row names of the By_Class table
     rownames(Conf_Matr_ByClass)<-sub("Class: ","",rownames(Conf_Matr_ByClass))
     
     # insert results to Hie_Performance if Flat_Measures_Logical==TRUE
     if(Flat_Measures_Logical)
     {  # start if Flat
       Hie_Perf_Mult_Maj$Accuracy[1]       <- Conf_Matr_Overall$Accuracy
       Hie_Perf_Mult_Maj$Kappa[1]          <- Conf_Matr_Overall$Kappa 
       Hie_Perf_Mult_Maj$AccuracyLower[1]  <- Conf_Matr_Overall$AccuracyLower 
       Hie_Perf_Mult_Maj$AccuracyUpper[1]  <- Conf_Matr_Overall$AccuracyUpper 
       Hie_Perf_Mult_Maj$AccuracyNull[1]   <- Conf_Matr_Overall$AccuracyNull 
       Hie_Perf_Mult_Maj$AccuracyPValue[1] <- Conf_Matr_Overall$AccuracyPValue 
       Hie_Perf_Mult_Maj$McnemarPValue[1]  <- Conf_Matr_Overall$McnemarPValue
     } # End if Flat
     
     
     if(Flat_Measures_Logical && By_Node)
     {  # start if Flat and By_Node
       Melt_ByClass <- melt(Conf_Matr_ByClass)
       colnames(Melt_ByClass)       <- c("Term_Node","Index","Value")
       Melt_ByClass$Term_Node_Index <- paste(Melt_ByClass$Term_Node,
                                             Melt_ByClass$Index,
                                             sep=";")
       for (i in 1:nrow(Melt_ByClass))
       { Col_NUM                        <- match(Melt_ByClass$Term_Node_Index[i],
                                                 colnames(Hie_Perf_Mult_Maj))
         Hie_Perf_Mult_Maj[1,Col_NUM] <- Melt_ByClass$Value[i]       }  
     } # end if Flat and By_Node
     
     if(Hie_F_Measure_Logical)
     { # start if Hie measures
       # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if By_N0de=TRUE, to each node as well
       Results_Hie_F_Measure = Hie_F_Measure(Conf_Matr = Conf_Matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                             Unique_Path = Unique_Path,     # Data frame, the Unique_Path data frame from Run_HRF
                                             Beta_H_F = Beta_H_F,
                                             By_Node=By_Node)
       
       # Store each result in the correct place in Hie_Performance
       for(count_measure in 1: nrow(Results_Hie_F_Measure))
       { Col_Num_2 <- match(Results_Hie_F_Measure[count_measure,"Measure"],
                            colnames(Hie_Perf_Mult_Maj))
         Hie_Perf_Mult_Maj[1,Col_Num_2] <-Results_Hie_F_Measure[count_measure,"Values"] }
       
     } # end if Hie measures
   }  #### End treatment of the Multiplicative Majority Rule
   
   #########################################################
   ####    treat the Multiplicative Permutation Rule     ###
   #########################################################
   
   if(Mult_Perm_Logical)
   { #### Start treatment of the multiplicative permutation Rule
     cat(paste("\n","Estimating performance measures for the multiplicative permutations rule ","\n",sep=""))
     
     for (count_perm in 1:nrow(Hie_Perf_Mult_Perm))
     { # Start the count_perm for loop
       
       # print the progress
       if(Div_Logical &&  round(count_perm/Div_Print,0)==count_perm/Div_Print)
       {cat(paste("\n","  Estimating performance measures for permutation number: ",count_perm ,sep=""))}
       
       # the focal pair of observed and expected to work on
       Focal_detail  <- Hie_Perf_Mult_Perm[count_perm,"Crisp_Rule"] 
       Pred_Nodes    <- Crisp_Case_Class[,match(Focal_detail,colnames(Crisp_Case_Class))] 
       
       
       # make sure the levels of Pred_Nodes and Obse_Nodes are identical
       Joined_levels <- Join_Levels(Vector_1 = Pred_Nodes,
                                    Vector_2 = Train_Data_Acc$Obs_Term_Node)
       
       Pred_Nodes    <- Joined_levels$Vector_1
       Obse_Nodes    <- Joined_levels$Vector_2
       rm(Joined_levels) # remove 
       
       
       # Use the confusionmatrix function caret package to get the confusion matrix
       Conf_Matr <- confusionMatrix(data=Pred_Nodes,
                                    reference= Obse_Nodes,
                                    dnn = c("Prediction", "Observed"))
       
       Conf_Matr_Table <- Conf_Matr$table
       Conf_Matr_Overall <- as.data.frame(t(Conf_Matr$overall))
       Conf_Matr_ByClass <- Conf_Matr$byClass
       
       # remove the prefix "Class: " from the row names of the By_Class table
       rownames(Conf_Matr_ByClass)<-sub("Class: ","",rownames(Conf_Matr_ByClass))
       
       # insert results to Hie_Performance if Flat_Measures_Logical==TRUE
       if(Flat_Measures_Logical)
       { # start if Flat
         Hie_Perf_Mult_Perm$Accuracy[count_perm]       <- Conf_Matr_Overall$Accuracy
         Hie_Perf_Mult_Perm$Kappa[count_perm]          <- Conf_Matr_Overall$Kappa 
         Hie_Perf_Mult_Perm$AccuracyLower[count_perm]  <- Conf_Matr_Overall$AccuracyLower 
         Hie_Perf_Mult_Perm$AccuracyUpper[count_perm]  <- Conf_Matr_Overall$AccuracyUpper 
         Hie_Perf_Mult_Perm$AccuracyNull[count_perm]   <- Conf_Matr_Overall$AccuracyNull 
         Hie_Perf_Mult_Perm$AccuracyPValue[count_perm] <- Conf_Matr_Overall$AccuracyPValue 
         Hie_Perf_Mult_Perm$McnemarPValue[count_perm]  <- Conf_Matr_Overall$McnemarPValue
         
       } # end if Flat
       
       
       # insert results to Hie_Performance if Flat_Measures_Logical==TRUE for each node
       if(Flat_Measures_Logical && By_Node)
       {  # start if Flat By_Node
         Melt_ByClass <- melt(Conf_Matr_ByClass)
         colnames(Melt_ByClass)       <- c("Term_Node","Index","Value")
         Melt_ByClass$Term_Node_Index <- paste(Melt_ByClass$Term_Node,
                                               Melt_ByClass$Index,
                                               sep=";")
         for (i in 1:nrow(Melt_ByClass))
         { Col_NUM                        <- match(Melt_ByClass$Term_Node_Index[i],
                                                   colnames(Hie_Perf_Mult_Perm))
           Hie_Perf_Mult_Perm[count_perm,Col_NUM] <- Melt_ByClass$Value[i]       }  
       } # end if Flat By_Node
       
       if(Hie_F_Measure_Logical)
       { # Start if Hie measures
         # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if By_N0de=TRUE, to each node as well
         Results_Hie_F_Measure = Hie_F_Measure(Conf_Matr = Conf_Matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                               Unique_Path = Unique_Path,     # Data frame, the Unique_Path data frame from Run_HRF
                                               Beta_H_F = Beta_H_F,
                                               By_Node=By_Node)
         
         # Store wach result in the correct place in Hie_Performance
         for(count_measure in 1: nrow(Results_Hie_F_Measure))
         { Col_Num_2 <- match(Results_Hie_F_Measure[count_measure,"Measure"],
                              colnames(Hie_Perf_Mult_Perm))
           Hie_Perf_Mult_Perm[count_perm,Col_Num_2] <-Results_Hie_F_Measure[count_measure,"Values"] }
       } # end if Hie measures
     } # End the count_perm for loop
   } #### End treatment of the multiplicative permutation Rule
   
   
   #########################################################
   ####    treat the Stepwise Majority Rule              ###
   #########################################################
   
   if(Step_Maj_Rule_Logical)
   { #### start treatment of  the stepwise Majority Rule
     cat(paste("\n","Estimating performance measures for the stepwise majority rule ","\n",sep=""))
     
     Focal_detail  <- "Stepwise_Majority_Rule"
     Pred_Nodes    <- Crisp_Case_Class[,match(Focal_detail,colnames(Crisp_Case_Class))] 
     
     # make sure the levels of Pred_Nodes and Obse_Nodes are identical
     Joined_levels <- Join_Levels(Vector_1 = Pred_Nodes,
                                  Vector_2 = Train_Data_Acc$Obs_Term_Node)
     
     Pred_Nodes    <- Joined_levels$Vector_1
     Obse_Nodes    <- Joined_levels$Vector_2
     
     rm(Joined_levels) # remove 
     
     # Use the confusionmatrix function caret package to get the confusion matrix
     Conf_Matr <- confusionMatrix(data=Pred_Nodes,
                                  reference= Obse_Nodes,
                                  dnn = c("Prediction", "Observed"))
     
     Conf_Matr_Table <- Conf_Matr$table
     Conf_Matr_Overall <- as.data.frame(t(Conf_Matr$overall))
     Conf_Matr_ByClass <- Conf_Matr$byClass
     
     # remove the prefix "Class: " from the row names of the By_Class table
     rownames(Conf_Matr_ByClass)<-sub("Class: ","",rownames(Conf_Matr_ByClass))
     
     # insert results to Hie_Performance if Flat_Measures_Logical==TRUE
     if(Flat_Measures_Logical)
     { # Start if Flat
       Hie_Perf_Step_Maj$Accuracy[1]       <- Conf_Matr_Overall$Accuracy
       Hie_Perf_Step_Maj$Kappa[1]          <- Conf_Matr_Overall$Kappa 
       Hie_Perf_Step_Maj$AccuracyLower[1]  <- Conf_Matr_Overall$AccuracyLower 
       Hie_Perf_Step_Maj$AccuracyUpper[1]  <- Conf_Matr_Overall$AccuracyUpper 
       Hie_Perf_Step_Maj$AccuracyNull[1]   <- Conf_Matr_Overall$AccuracyNull 
       Hie_Perf_Step_Maj$AccuracyPValue[1] <- Conf_Matr_Overall$AccuracyPValue 
       Hie_Perf_Step_Maj$McnemarPValue[1]  <- Conf_Matr_Overall$McnemarPValue
     } # End if Flat
     
     # insert results to Hie_Performance if Flat_Measures_Logical==TRUE for each node
     if(Flat_Measures_Logical && By_Node)
     { # Start if Flat and By_Node
       Melt_ByClass <- melt(Conf_Matr_ByClass)
       colnames(Melt_ByClass)       <- c("Term_Node","Index","Value")
       Melt_ByClass$Term_Node_Index <- paste(Melt_ByClass$Term_Node,
                                             Melt_ByClass$Index,
                                             sep=";")
       for (i in 1:nrow(Melt_ByClass))
       { Col_NUM                        <- match(Melt_ByClass$Term_Node_Index[i],
                                                 colnames(Hie_Perf_Step_Maj))
         Hie_Perf_Step_Maj[1,Col_NUM] <- Melt_ByClass$Value[i]       }  
     } # End if Flat and By_Node
     
     if(Hie_F_Measure_Logical)
     { # Start if Hie measure
       # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if By_N0de=TRUE, to each node as well
       Results_Hie_F_Measure = Hie_F_Measure(Conf_Matr = Conf_Matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                             Unique_Path = Unique_Path,     # Data frame, the Unique_Path data frame from Run_HRF
                                             Beta_H_F = Beta_H_F,
                                             By_Node=By_Node)
       
       # Store each result in the correct place in Hie_Performance
       for(count_measure in 1: nrow(Results_Hie_F_Measure))
       { Col_Num_2 <- match(Results_Hie_F_Measure[count_measure,"Measure"],
                            colnames(Hie_Perf_Step_Maj))
         Hie_Perf_Step_Maj[1,Col_Num_2] <-Results_Hie_F_Measure[count_measure,"Values"] }
     } # End if Hie measure
   }  #### End treatment of the stepwise Majority Rule
   
   cat(paste("\n","-->  Performance measures estimated  ","\n",sep=""))
   cat("######################")
   
   ######################################################################################
   ### STEP 7 - Create the return list                                                ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   
   Return_List <- list(Raw_Votes        = focal_Votes ,       # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
                       Train_Data_Acc   = Train_Data_Acc,     # Data frame, the Case_ID and observed terminal node of each case
                       Crisp_Case_Class = Crisp_Case_Class    # Data frame, containing all the crisp classification of each case (as row) 
                       )
   
   if(Mult_Maj_Rule_Logical || Mult_Perm_Logical)
   { New_List <- list(Multiplicative_Prop=Multiplicative_Prop)
     Return_List <- c(Return_List,New_List)}
   
   if(Mult_Maj_Rule_Logical)
   { New_List <- list(Hie_Perf_Mult_Maj=Hie_Perf_Mult_Maj)
     Return_List <- c(Return_List,New_List)}
     
   if(Mult_Perm_Logical)
   { New_List <- list(Hie_Perf_Mult_Perm=Hie_Perf_Mult_Perm)
     Return_List <- c(Return_List,New_List)}
   
   if(Step_Maj_Rule_Logical)
   { New_List <- list(Hie_Perf_Step_Maj=Hie_Perf_Step_Maj)
     Return_List <- c(Return_List,New_List)}
   
   if(By_Node)
   { New_List <- list(Nodes_Measures_columns = Nodes_Measures_columns)
     Return_List <- c(Return_List,New_List)}
   
   
   # add the call
   New_List <- list(call           = match.call())
   Return_List <- c(Return_List,New_List)
   
   # set the new class
   class(Return_List) <- "Hier.RF.Performance"
   return(Return_List)
  
  
 } # End function 

 