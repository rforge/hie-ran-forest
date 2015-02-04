#' predict and asses performance of New_Data, for which the 'true' class is 
#' known.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param Hie_RF              Object of class Hier.Random.Forest - the output of
#'   Run_HRF.
#' @param New_Data            The data frame containing additional cases that 
#'   were note a part of the original training.
#' @param New_Data_Case_ID    Integer, specifying the column number with the 
#'   Case_ID in the New_Data data frame. The Case ID values should be unique and
#'   different from those in the training data.
#' @param New_Data_Exp_Var    Vector of integers, specifying the columns of 
#'   New_Data that contains the same set of explanatory variables as used in the
#'   training of Hie_RF. Default is all column except New_Data_Case_ID and 
#'   New_Data_Hie.
#' @param New_Data_Hie        Vector of character or integers, containing the 
#'   names or column numbers of the hierarchical levels in New_Data. Order of 
#'   columns should be from the tree root to the terminal nodes. If a single 
#'   column is provided, it should contain only terminal nodes.
#' @param Crisp_Rule         The method of translating proportion of votes to a 
#'   crisp category. See details in HRF_Performance.
#' @param Perm_Num           Integer, number of random permutations for each
#'   case if 'Multiplicative_Permutation' is applied. See details in
#'   HRF_Performance.
#' @param Div_Logical        Logical, if TRUE progress when 
#'   'Multiplicative_Permutation' is applied will be printed every Div_Print 
#'   permutations
#' @param Per_Index         The performance and accuracy indices to compute. See
#'   details in HRF_Performance.
#' @param By_Node           Logical, if TRUE performances indices will be 
#'   estimated for each terminal node as well as for the overall confusion 
#'   matrix.
#' @param Beta_H_F          Numeric in the range Beta_H_F>=0. Controls weights 
#'   in the hierarchical F measure index. See Hie_F_Measure for details.
#' @param ...               Optional parameters to be passed to low level 
#'   functions.
#'   


# Function - predict and asses performance of New_Data, for which the 'true' class is known

HRF_Performance_New_Data = function(Hie_RF,
                                    New_Data,
                                    New_Data_Case_ID = 1,
                                    New_Data_Exp_Var = NA,
                                    New_Data_Hie,
                                    Crisp_Rule  = c("Multiplicative_Majority","Multiplicative_Permutation","Setpwise_Majority"),
                                    Perm_Num = 500,
                                    Div_Logical = TRUE,                          
                                    Div_Print = 25,
                                    Per_Index = c("Flat_Measures","Hie_F_Measure"),
                                    By_Node = TRUE,
                                    Beta_H_F = 1,
                                    ...
                                    )
  
{
 
  
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
  
  # Most of the checks are performed within 'Predict_New'
  
  
  if(missing(New_Data_Exp_Var))
  {cat("Note! all columns other than those specified by New_Data_Case_ID and New_Data_Hie will be used as input variables")}
  
  
  #####
  # check Per_Index
  Temp_Vec <-c("Flat_Measures","Hie_F_Measure") 
  
  if(length(intersect(Per_Index,Temp_Vec))<1)
  {stop(paste("\n","In HRF_Perfomance, atleast one valid option for Per_Index is required", "\n",sep=""))}
  
  if(length(intersect(Per_Index,Temp_Vec))>0)
  {cat(paste("\n", "Performance will be evaluated using: ",Per_Index,"\n",sep=""))}
  
  rm(Temp_Vec)
  
  #####
  # Check By_Node
  if(!is.logical(By_Node))
  {cat(paste("\n", "In HRF_Perfomance, By_Node should be Logical. default of By_Node=TRUE is used","\n",sep=""))
   By_Node <- TRUE}
  
  #####
  # check Beta_H_F
  if(!is.numeric(Beta_H_F))
  {cat(paste("\n","In HRF_Perfomance, Beta_H_F should be a positive number", "\n",
             "Default of Beta_H_F=1 is used","\n",sep=""))
   Beta_H_F <- 1    }
  
  if(Beta_H_F<0)
  {cat(paste("\n","In HRF_Perfomance, Beta_H_F should be non-negative", "\n",
             "Default of Beta_H_F=1 is used","\n",sep=""))
   Beta_H_F <- 1    }
  
  
  #####
  # check New_Data_Hie
  
  # Change from a character to numeric column number 
  if(is.character(New_Data_Hie))
  {New_Data_Hie  <- match(New_Data_Hie,names(New_Data))}
  
  # checks if the columns specified in Hie_Levels contains factors
  Check_Hie_Levels(New_Data[,New_Data_Hie])
  
  ######################################################################################
  ### STEP 2 - Extract values from Hie_RF                                            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  Unique_Path       <- Hie_RF$Hier_Struc$Unique_Path
  End_Path_Name     <- Hie_RF$call$End_Path_Name
  
  
  
  ######################################################################################
  ### STEP 3 - Arrange New_Data_Exp_Var + extract values from Hie_RF                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  if(is.character(New_Data_Exp_Var))
  {New_Data_Exp_Var  <- match(New_Data_Exp_Var,names(New_Data))}
  
  # when New_Data_Exp_Var is not given by the user 
  if(is.na(New_Data_Exp_Var))
  {
    New_Data_Exp_Var <- 1:length(names(New_Data)) 
    New_Data_Exp_Var <- New_Data_Exp_Var[New_Data_Exp_Var!=New_Data_Case_ID]         # Remove Case_ID
    New_Data_Exp_Var <- New_Data_Exp_Var[!New_Data_Exp_Var %in% New_Data_Hie] # Remove Hie_Levels 
  }
  
 
  ######################################################################################
  ### STEP 4 - Apply the Predict function                                            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  
  
  #### Predict the raw proportion of votes, the multiplicative proportion of votes and the crisp classifcation for each case 
  cat("######################")
  cat(paste("\n", "-->  Calling Predict_New for additional checks and votes extraction","\n",sep=""))
  
  
  Predict_New_Data =   Predict_New(Hie_RF= Hie_RF,                                         # object of class Hier.Random.Forest - the output of Run_HRF
                                  New_Data= New_Data         ,                #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                                  New_Data_Case_ID = New_Data_Case_ID,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                                  New_Data_Exp_Var = New_Data_Exp_Var,  # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the Hie_RF
                                  Crisp_Rule  = Crisp_Rule,
                                  Perm_Num= Perm_Num,
                                  Div_Logical = Div_Logical,
                                  Div_Print = Div_Print
                                  )
  
  
  # Extract values from the output of Predict_New
  Raw_Votes           <- Predict_New_Data$Raw_Votes             # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
  Multiplicative_Prop <- Predict_New_Data$Multiplicative_Prop  # data frame, containing the muitplication of votes down each path from tree root to terminal node for each case
  Crisp_Case_Class    <- Predict_New_Data$Crisp_Case_Class
  
 
  
  ######################################################################################
  ### STEP 5 - Prepare observed terminal nodes                                       ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n","-->  Preparing the observed terminal nodes ","\n",sep=""))
  
  # prepare the Train_Data_acc data frame
  Train_Data_Acc <- subset(New_Data,select=New_Data_Case_ID)
  Train_Data_Acc$Obs_Term_Node <- NA
 
  # Get the terminal node of each case
  for (K2 in 1:nrow(New_Data))
  {
    Focal_Path      <- New_Data[K2,New_Data_Hie] # the column with the hierarchical data for case k2
    
    if(length(which(Focal_Path==End_Path_Name))==0)
    {  Train_Data_Acc[K2,"Obs_Term_Node"] <- as.character(Focal_Path[1,ncol(Focal_Path)])    }  
    
    if(length(which(Focal_Path==End_Path_Name))!=0)
    {   Train_Data_Acc[K2,"Obs_Term_Node"] <- as.character(Focal_Path[1,(min(which(Focal_Path==End_Path_Name))-1)])  } 
    
    rm(Focal_Path)
  } 
  rm(K2)
  
  # order according to Case_ID and change to Factors
  Train_Data_Acc <- Train_Data_Acc[order(Train_Data_Acc[,1]),]
  Train_Data_Acc$Obs_Term_Node <- factor(Train_Data_Acc$Obs_Term_Node)
  
  cat(paste("\n","-->  Observed terminal nodes ready,  ","\n",sep=""))
  
  
  
   
  
  ######################################################################################
  ### STEP 6 - Arrange data for performance analysis                                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n","-->  Createing the Crisp_Rule by Per_Index data frames","\n",sep=""))
  
  cat(paste("\n","Identifying indices called by user","\n",sep=""))
  ## Add the relevant measures columns                        ##
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  ### Create logical object for each option of Crisp_Rule and Per_Index
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
  
  
  
  # Add column to Hie_Performance for each performance index
  
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
  
  # If performance is to be estimated for each terminal node
  if(By_Node)
  { # Start the By_Node condition
    
    # Extract all the terminal nodes in the original Hie_RF and insert the levels to Unique_Nodes
    Nodes_Info<- Hie_RF$Hier_Struc$Nodes_Info
    Nodes_Info <- subset(Nodes_Info, Nodes_Info$Term_Int_node=="Term_Node")
    Unique_Nodes   <- unique(Nodes_Info$Node_Name)
    rm(Nodes_Info)
    
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
  
  
  cat(paste("\n","-->  Crisp_Rule by Per_Index data frames created","\n",sep=""))
  cat("######################")
  
  
  ######################################################################################
  ### STEP 7 - ESTIMATING ACCURACY FOR EACH ROW IN Hie_Performance                           ###
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
  
  
  Return_List <- list(Raw_Votes        = Raw_Votes ,       # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
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