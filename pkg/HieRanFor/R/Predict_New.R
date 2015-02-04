#' predict function for New_Data using an existing  object of class \code{"Hier.Random.Forest"}
#' 
#' 
#' 


# Predict new data according to an existing HRF object

Predict_New = function(Hie_RF,                                         # object of class Hier.Random.Forest - the output of Run_HRF
                       New_Data,                               #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                       New_Data_Case_ID = 1,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                       New_Data_Exp_Var = c(2:ncol(New_Data)),  # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the Hie_RF
                       Crisp_Rule  = c("Multiplicative_Majority","Multiplicative_Permutation","Setpwise_Majority"),
                       Perm_Num = 500,
                       Div_Logical = TRUE,                             # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                       Div_Print = 25
                       )
  
{
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  require(randomForest)
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call","\n",sep=""))
  
  #####
  # check class of Hie_RF
  if(class(Hie_RF)!="Hier.Random.Forest")
  {stop(paste("\n","In Predict_New:  Hie_RF should be of class Hier.Random.Forest","\n",sep=""))}
  
  #####
  # check Perm_Num
  if(!is.numeric(Perm_Num))
  {stop(paste("\n","In Predict_New, Perm_Num should be a positive integer", "\n",sep=""))}
  
  if(Perm_Num<1)
  {stop(paste("\n","In Predict_New, Perm_Num should be a positive integer", "\n",sep=""))}
  
  if(round(Perm_Num,0)!=Perm_Num)
  {stop(paste("\n","In Predict_New, Perm_Num should be a positive integer", "\n",sep=""))}
  
  #####
  # Check Div_Logical
  if(!is.logical(Div_Logical))
  {cat(paste("\n", "In Predict_New, Div_Logical should be Logical. default of Div_Logical=TRUE is used","\n",sep=""))
   Div_Logical <- TRUE}
  
  #####
  # check Div_Print
  if(!is.numeric(Div_Print))
  {cat(paste("\n","In Predict_New, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  if(Div_Print<1)
  {cat(paste("\n","In Predict_New, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  if(round(Div_Print,0)!=Div_Print)
  {cat(paste("\n","In Predict_New, Div_Print should be a positive integer", "\n",
             "Default of Div_Print=25 is used","\n",sep=""))
   Div_Print <- 25 }
  
  #####
  # check Crisp_Rule
  
  Temp_Vec <-c("Multiplicative_Majority","Multiplicative_Permutation","Setpwise_Majority") 
  
  if(length(intersect(Crisp_Rule,Temp_Vec))<1)
  {stop(paste("\n","In Predict_New, atleast one valid option for Crisp_Rule is required", "\n",sep=""))}
  
  if(length(intersect(Crisp_Rule,Temp_Vec))>0)
  {cat(paste("\n", "Estimating crisp classification based on: ",Crisp_Rule,"\n",sep=""))}
  
  
  ###############################
  # Check New_Data              #
  ###############################
  
  if(missing(New_Data))
  {stop("\n No New_Data was specified")}
  
 
  
  #####
  # Check NA and missing values #
   
  Work_Data <- New_Data[,c(New_Data_Case_ID,New_Data_Exp_Var)]  
  
  if(length(which(Work_Data[]=="" | Work_Data[]==" " | is.na(Work_Data)))!=0)
  {stop("\n At least one columns contains missing values or NA. \n Please remove missing values from  New_Data columns of New_Data_Case_ID, Hie_Levels and New_Data_Exp_Var")}
  
  rm(Work_Data)
  
  #####
  # check if the same explanatory variables are called 
  
  Check_New_Exp_Var(Hie_RF=Hie_RF,
                    New_Data=New_Data,
                    New_Data_Exp_Var=New_Data_Exp_Var)
  
  #####
  # Check if the New_Data_Case_ID are different from those used in the Hie_RF
  Check_New_ID(Hie_RF=Hie_RF,
               New_ID =New_Data[,New_Data_Case_ID])
  
  
  #####
  # checks if the column specified as Case_ID contains a single entery for each ID
  Check_Case_ID(New_Data[,New_Data_Case_ID])
  
  
  
  
  
  
  
  
  cat(paste("\n", "-->  Call Verified","\n",sep=""))
  
 
  
  ######################################################################################
  ### STEP 2 - CREATE LOGICAL ARGUMENTS FOR EACH OPTION OF Crisp_Rule                ###
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
  
  ######################################################################################
  ### STEP 3 - Extract the raw votes                                                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n","-->  Preparing the predicted terminal nodes from the hierarchical RandomForest ","\n",sep=""))
  
  Votes_New <- Extract_Votes(Hie_RF=Hie_RF,                                 # object of class Hier.Random.Forest - the output of Run_HRF
                             Train_Predict    = FALSE,                # logical, if true, the OOB votes that each case received for each local classifier are returned
                             New_Data         = New_Data,                # Optional data frame containing additional cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                             New_Data_Case_ID = New_Data_Case_ID,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                             New_Data_Exp_Var = New_Data_Exp_Var, # vector of integers, specifying the columns of New_Data that contains the same set of explanatory variables as used in the training of the hierarchical Random Forest. Note about the levels
                             Bind_Train_New      = FALSE)
  
  focal_Votes <- Votes_New$Prop_Vote_New
  
  ######################################################################################
  ### STEP 4 - estimate crisp classifcation                                          ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # the data frame that collects all the crisp rules
  Crisp_Case_Class <- focal_Votes[,c(1,2)] 
  
  Unique_Path       <- Hie_RF$Hier_Struc$Unique_Path
  
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
  
  cat(paste("\n","-->  Predicted terminal nodes ready ","\n",sep=""))
  
  
  
  ######################################################################################
  ### STEP 5 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  Return_List <- list(Raw_Votes        = focal_Votes ,       # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
                      Multiplicative_Prop=Multiplicative_Prop,  # data frame, containing the muitplication of votes down each path from tree root to terminal node for each case
                      Crisp_Case_Class = Crisp_Case_Class,    # Data frame, containing all the crisp classification of each case (as row) 
                      call           = match.call()
                      )
  
  return(Return_List)
  
  
  
} # end function