#' Runs a flat classifcation on the same data as Hie_RF and asses performance.
#' 
#' 
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param Hie_RF             Object of class Hier.Random.Forest - the output of 
#'   Run_HRF.
#' @param mtry               Number of variables randomly sampled as candidates 
#'   at each split. Default is to use the same method used in Hie_RF
#' @param ntree              Number of trees to grow in the flat classifier.See 
#'   ?randomForest for additional details, Default is the same ntree used in 
#'   each local classifer of Hie_RF.
#' @param importance         Logical, if TRUE importance of variables will be 
#'   assesed. See ?randomForest for additional details.Default is the same as 
#'   Hie_RF.
#' @param proximity          Logical, If TRUE, proximity will be calcualted. See
#'   ?randomForest for additional details. Default is the same as Hie_RF.
#' @param keep.forest        Logical, if TRUE (recommended) the forest will be 
#'   retained. Default is the same as Hie_RF.
#' @param keep.inbag         Logical, if TRUE an n by ntree matrix be returned 
#'   that keeps track of which samples are "in-bag” in which trees. n being the 
#'   number of cases in the training set. Required for the permutation-based 
#'   performance assesments. Default is the same as Hie_RF.
#' @param Per_Index          The performance and accuracy indices to compute.
#'   See details in HRF_Performance.
#' @param Crisp_Rule         The method of translating proportion of votes to a 
#'   crisp category. See details in HRF_Performance.
#' @param Perm_Num           Integer, number of random permutations for each
#'   case if 'Multiplicative_Permutation' is applied. See details in
#'   HRF_Performance.
#' @param By_Node            Logical, if TRUE, performance measures are 
#'   estimated for each terminal node as well.
#' @param Div_Logical        Logical, if TRUE progress when 
#'   'Multiplicative_Permutation' is applied will be printed every Div_Print 
#'   permutations
#' @param Beta_H_F           Numeric in the range Beta_H_F>=0. Controls weights 
#'   in the hierarchical F measure index. See Hie_F_Measure for details.
#' @param ...                Optional parameters to be passed to the low level 
#'   functions
#'   
#'   



Flat_RF_Performance = function(Hie_RF,                                   
                               mtry         = Hie_RF$call$mtry,          
                               ntree        = Hie_RF$call$ntree,         
                               importance   = Hie_RF$call$importance,    # Should importance of predictors be assessed?
                               proximity    = Hie_RF$call$proximity,     # should the proximity be calcualted? 
                               keep.forest  = Hie_RF$call$keep.forest,   
                               keep.inbag   = Hie_RF$call$keep.inbag,    
                               Per_Index = c("Flat_Measures","Hie_F_Measure"),      # the accurcary index to use. 
                               Crisp_Rule  = c("Multiplicative_Majority","Multiplicative_Permutation"), # Wether the multiplicative majority rule should be used to convert proportion of votes to classifed node or wether permutation based accuracy  should be estimated.
                               Perm_Num = 500,                                 # Integer, number of random votes to take for each case
                               By_Node = TRUE,                                 # logical, indicating whether several indices for each node should be returned, if TRUE, the indices will appear in Hie_Performance after the overall performance measures 
                               Div_Logical = TRUE,                             # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                               Div_Print = 25,
                               Beta_H_F = 1,
                               ...)
{ # Start function
  

  # required packages
  # require(randomForest)
  # require(e1071)
  # require(caret)
  # require(reshape)
  
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  Unique_Path  <- Hie_RF$Hier_Struc$Unique_Path
  # additinal checks are performed within Run_HRF_As_Flat
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call","\n",sep=""))
  
  # check class of Hie_RF
  if(class(Hie_RF)!="Hier.Random.Forest")
  {stop(paste("\n","Flat_RF_Performance:  Hie_RF should be of class Hier.Random.Forest","\n",sep=""))}
  
  # check Per_Index
  Temp_Vec <-c("Flat_Measures","Hie_F_Measure") 
  
  if(length(intersect(Per_Index,Temp_Vec))<1)
  {stop(paste("\n","In Flat_RF_Performance, atleast one valid option for Per_Index is required", "\n",sep=""))}
  
  if(length(intersect(Per_Index,Temp_Vec))>0)
  {cat(paste("\n", "Performance will be evaluated using: ",Per_Index,"\n",sep=""))}
  
  rm(Temp_Vec)
  # check Crisp_Rule
  
  Temp_Vec <-c("Multiplicative_Majority","Multiplicative_Permutation") 
  
  if(length(intersect(Crisp_Rule,Temp_Vec))<1)
  {stop(paste("\n","In Flat_RF_Performance, atleast one valid option for Crisp_Rule is required", "\n",sep=""))}
  
  if(length(intersect(Crisp_Rule,Temp_Vec))>0)
  {cat(paste("\n", "Performance will be evaluated using crisp classification based on: ",Crisp_Rule,"\n",sep=""))}
  
  # check Perm_Num
  if(!is.numeric(Perm_Num))
  {stop(paste("\n","In Flat_RF_Performance, Perm_Num should be a positive integer", "\n",sep=""))}
  
  if(Perm_Num<1)
  {stop(paste("\n","In Flat_RF_Performance, Perm_Num should be a positive integer", "\n",sep=""))}
  
  if(round(Perm_Num,0)!=Perm_Num)
  {stop(paste("\n","In Flat_RF_Performance, Perm_Num should be a positive integer", "\n",sep=""))}
  
  # Check By_Node
  if(!is.logical(By_Node))
  {cat(paste("\n", "In Flat_RF_Performance, By_Node should be Logical. default of By_Node=TRUE is used","\n",sep=""))
   By_Node <- TRUE}
  
  # Check Div_Logical
  if(!is.logical(Div_Logical))
  {cat(paste("\n", "In Flat_RF_Performance, Div_Logical should be Logical. default of Div_Logical=TRUE is used","\n",sep=""))
   Div_Logical <- TRUE}
  
  # check Div_Print
  if(!is.numeric(Div_Print))
  {cat(paste("\n","In Flat_RF_Performance, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  if(Div_Print<1)
  {cat(paste("\n","In Flat_RF_Performance, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  if(round(Div_Print,0)!=Div_Print)
  {cat(paste("\n","In Flat_RF_Performance, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  # check Beta_H_F
  if(!is.numeric(Beta_H_F))
  {cat(paste("\n","In Flat_RF_Performance, Beta_H_F should be a positive number", "\n",
             "Default of Beta_H_F=1 is used","\n",sep=""))
   Beta_H_F <- 1    }
  
  if(Beta_H_F<0)
  {cat(paste("\n","In Flat_RF_Performance, Beta_H_F should be non-negative", "\n",
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
  ### STEP 3 - Run the flat RandomForest                                             ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # run the flat RF
  Run_Flat <- Run_HRF_As_Flat(Hie_RF= Hie_RF,                                   # object of class Hier.Random.Forest - the output of Run_HRF
                              mtry         = mtry,          # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier
                              ntree        = ntree,         # number of trees to grow in each local classifier
                              importance   = importance,    # Should importance of predictors be assessed?
                              proximity    = proximity,     # should the proximity be calcualted? 
                              keep.forest  = keep.forest,   
                              keep.inbag   = keep.inbag    
  )
  
  # extract the output
  Flat_RF         <- Run_Flat$Flat_RF
  Train_Flat_Case <- Run_Flat$Train_Flat_Case
  
  
  
  # all the terminal nodes found in the Train_Flat_Case
  Unique_Nodes   <- unique(Train_Flat_Case$Obs_Term_Node)
  
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
  
  
  ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
  #    Add here addtional Hie_Perf data frames if new methods are added     #
  ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
  
  
  cat(paste("\n","-->  Crisp_Rule by Per_Index data frames created","\n",sep=""))
  cat("######################")
  
  
  ######################################################################################
  ### STEP 5 - PREPARING THE TERMINAL NODES AS PREDICTED BY THE HRF                  ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n","-->  Preparing the predicted terminal nodes from the flat RandomForest ","\n",sep=""))
  
  # for each case (rows) the proportion of OOB votes for each node according to the flatt classification
  focal_Votes <- as.data.frame(Flat_RF$votes)
  
  # check row sums = 1
  if(!all(abs(rowSums(focal_Votes)-1)<=10^-8))
  {stop(paste("\n","In Flat_RF_Performance, Row sums of proportion of votes do not sum to 1", "\n",sep=""))}
  
  # add to focal_Votes the Train_or_Test columna nd the case_ID column
  focal_Votes <- cbind(Train_Flat_Case[1],focal_Votes)
  Temp_vec <- data.frame(Train_or_Test = c(NA))
  Temp_vec[c(1:nrow(focal_Votes)),"Train_or_Test"] <- "Train"
  focal_Votes <- cbind(Temp_vec,focal_Votes)
  rm(Temp_vec)
  
  # reorder  focal votes and 
  focal_Votes <- focal_Votes[order(focal_Votes[,2]),]
  
  # re-order the Train_Flat_Case according to Case_ID
  Train_Flat_Case <- Train_Flat_Case[order(Train_Flat_Case[,1]),]
  
  # check consistancy of case_ID
  if(!all(focal_Votes[,2]==Train_Flat_Case[,1]))
  {stop(paste("\n","In Flat_RF_Performance, inconsistent Case_ID between observed and predicted porportion of votes", "\n",sep=""))}
  

  
  # the data frame that collects all the crisp rules
  Crisp_Case_Class <- focal_Votes[,c(1,2)] 
  
  # if the user called the multiplicative majority rule
  if(Mult_Maj_Rule_Logical)
  {
    cat(paste("\n","Adding the multiplicative majority Rule","\n",sep=""))
    Mult_Maj_Rule <- Multiplicative_Majority_Rule(Prop_Multiplicative_Votes = focal_Votes,     # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is Train_or_Test and the second is Case_ID
                                                  Bind_Prop_Mul_Maj=FALSE)
    
    Crisp_Case_Class <- cbind(Crisp_Case_Class,Mult_Maj_Rule[3])
    cat(paste("\n","Multiplicative majority Rule added","\n",sep=""))
  }
  
  # if the user called the multiplicative permutations rule
  if(Mult_Perm_Logical)
  {
    cat(paste("\n"," Adding: ",Perm_Num, " permutations","\n",sep=""))
    Mult_Perm_Rule <- Perm_Multiplicative_Term_Node(Multiplicative_Prop_Votes = focal_Votes,       # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. onr of the outputs of Multiplicative_Majority_Rule function. First column is the Train_or_test, second column is the Case_ID, 
                                                    Perm_Num          = Perm_Num,          # Integer, number of random votes to take for each case 
                                                    Div_Logical       = Div_Logical,       # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                                                    Div_Print         = Div_Print,
                                                    Bind_Prop_Perm    = FALSE)              # logical, if true, the permutated terminal nodes are added at the end of the Multiplicative_Prop_Votes. If FALSE, a seperate data frame is returned, cointating only the Case_ID and the permuted terminal nodes
    Crisp_Case_Class <- cbind(Crisp_Case_Class,Mult_Perm_Rule[3:ncol(Mult_Perm_Rule)])
    
    cat(paste("\n",Perm_Num," Permutations added","\n",sep=""))
  } 
  
  cat(paste("\n","-->  Predicted terminal nodes ready ","\n",sep=""))
  
  
  ######################################################################################
  ### STEP 6 - ESTIMATING ACCURACY FOR EACH ROW IN Hie_Performance                   ###
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
                                 Vector_2 = Train_Flat_Case$Obs_Term_Node)
    
    Pred_Nodes    <- Joined_levels$Vector_1
    Obse_Nodes    <- Joined_levels$Vector_2
    
    rm(Joined_levels) # remove 
    
    # Use the confusionmatrix function caret package to get the confusion matrix
    Conf_Matr <- caret::confusionMatrix(data      = Pred_Nodes,
                                        reference = Obse_Nodes,
                                        dnn       = c("Prediction", "Observed"))
    
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
      Melt_ByClass <- reshape::melt(Conf_Matr_ByClass)
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
                                   Vector_2 = Train_Flat_Case$Obs_Term_Node)
      
      Pred_Nodes    <- Joined_levels$Vector_1
      Obse_Nodes    <- Joined_levels$Vector_2
      rm(Joined_levels) # remove 
      
      
      # Use the confusionmatrix function caret package to get the confusion matrix
      Conf_Matr <- caret::confusionMatrix(data=Pred_Nodes,
                                   reference= Obse_Nodes,
                                   dnn = c("Prediction", "Observed"))
      
      Conf_Matr_Table   <- Conf_Matr$table
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
        Melt_ByClass <- reshape::melt(Conf_Matr_ByClass)
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
        
        # Store each result in the correct place in Hie_Performance
        for(count_measure in 1: nrow(Results_Hie_F_Measure))
        { Col_Num_2 <- match(Results_Hie_F_Measure[count_measure,"Measure"],
                             colnames(Hie_Perf_Mult_Perm))
          Hie_Perf_Mult_Perm[count_perm,Col_Num_2] <-Results_Hie_F_Measure[count_measure,"Values"] }
      } # end if Hie measures
    } # End the count_perm for loop
  } #### End treatment of the multiplicative permutation Rule
  
  
  
  cat(paste("\n","-->  Performance measures estimated  ","\n",sep=""))
  cat("######################")
  
  ######################################################################################
  ### STEP 7 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  Return_List <- list(Flat_RF         = Flat_RF,       # Object of class randomForest, the result of flat classification
                      Train_Flat_Case = Train_Flat_Case,
                      Crisp_Case_Class = Crisp_Case_Class    # Data frame, containing all the crisp classification of each case (as row) 
  )
  
 
  
  if(Mult_Maj_Rule_Logical)
  { New_List <- list(Hie_Perf_Mult_Maj=Hie_Perf_Mult_Maj)
    Return_List <- c(Return_List,New_List)}
  
  if(Mult_Perm_Logical)
  { New_List <- list(Hie_Perf_Mult_Perm=Hie_Perf_Mult_Perm)
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
                              