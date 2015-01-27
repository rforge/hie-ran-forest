
# returns the porportion of OOB votes that each node recieved in each local classifer
# if Train_Predict=TRUE, runs on all the training data, 
# if is.null(New_Data)==FALSE, runs also on the New_Data (sould be in similar form as the train_data)
# throughout, Case_ID is used to avoid mixing different cases
# For the training data and any local classifier:
# The OOB votes are used for cases that are a real child of the parent node
# the ouput of predict.randomForest is used for cases that are not 'real' chiles of the parent node


Extract_Votes = function(Hie_RF,                                 # object of class Hier.Random.Forest - the output of Run_HRF
                         Train_Predict    = TRUE,                # logical, if true, the OOB votes that each case received for each local classifier are returned
                         New_Data         = NULL,                # Optional data frame containing additional cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                         New_Data_Case_ID = 1,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                         New_Data_Exp_Var = c(2:ncol(New_Data)), # vector of integers, specifying the columns of New_Data that contains the same set of explanatory variables as used in the training of the hierarchical Random Forest. Note about the levels
                         Bind_Train_New      = FALSE,               # logical, If is.null(New_Data)==FALSE, should the prediction of the training
                         ...)
{ # start function
  
  require(randomForest)
  # check that there is some data to work on
  if(is.null(New_Data) && !Train_Predict)
  {stop(paste("\n",
              "Error in function: Extract_Votes", 
              "\n", 
              "No data to produce proportion of votes,",
              "\n",
              "Please set Train_Predict to TRUE or provide New_Data",sep=""))}
  
  # extract the relevant information from Hie_RF
  LRF_Info         <- Hie_RF$Hier_Struc$LRF_Info      # the info data frame on each local classifer
  Nodes_Info       <- Hie_RF$Hier_Struc$Nodes_Info    # the info data frame on each node
  Unique_Path      <- Hie_RF$Hier_Struc$Unique_Path   # all the pathes from tree root to terminla nodes
  Train_Data_Ready <- Hie_RF$Train_Data_Ready         # the re-arranged Train_data, as output of Hie_Ran_For
  Case_ID_2        <- Hie_RF$Case_ID_2                # The column in Train_Data_Ready that contains the Case_ID
  Exp_Var_2        <- Hie_RF$Exp_Var_2                # The column in Train_Data_Ready that contains the Explanatory variables
  All_Local_RF     <- Hie_RF$All_Local_RF             # list containing all the local random forests. For each random forest, there is a list with two lists: the Local_Data and the Local_RF
  
  
  ###################################
  ## Deal with Train_Predict==TRUE ##
  ###################################
  
  if (Train_Predict)
  { # start the if (Train_Predict) condition
    
    cat(paste("\n","     --> Start votes extraction for the Training data","\n",sep=""))
    
  Prop_Vote_Train              <- as.data.frame(Train_Data_Ready[,Case_ID_2])
  colnames(Prop_Vote_Train)[1] <- colnames(Train_Data_Ready)[Case_ID_2]
  
  # make sure the cases are ordered according to Case_Id
  Prop_Vote_Train <- Prop_Vote_Train[order(Prop_Vote_Train[,1]),]
  
  for (i in 1:dim(LRF_Info)[1]) # loop that runs on each local classifier
  { # start the 'i' loop
    
    cat(paste("\n","Start votes extraction (training data) for local classifer= ",LRF_Info[i,1],"\n",sep=""))
    
    
    # extract data for the local classifer
    Local_RF_Obj  <- All_Local_RF[[i]]        # a list with Local_Data and Local_Rf
    Local_Data    <- Local_RF_Obj$Local_Data  # list, the local dat used in local classifer i
    Local_RF      <- Local_RF_Obj$Local_RF    # object of class RandomForest for local classifer i
    
    Local_Train_Case_ID              <- as.data.frame(Local_Data$Local_Train_Case_ID)      # the Case_ID of the Local_Train_Data
    Local_Votes                      <- as.data.frame(Local_RF$votes)                      # the porportion of OOB votes for each category of each case in the Local_RF
    Local_Case_ID_Votes              <- cbind(Local_Train_Case_ID,Local_Votes)             # binding the Case_ID with the votes
    colnames(Local_Case_ID_Votes)[1] <- colnames(Train_Data_Ready)[Case_ID_2]              # arranging the colomn name for Case_ID
    
    # three options  
    # 1 - all the cells of Train_Data_Ready are found in the local training data 
    # 2 - not all the cells of Train_Data_Ready are found in the local training data 
    # 3 - an error message when there are more cases in the local Train data than in Train_Data_Ready
    
    ##########################################################
    
    # Option 1: all the cells of Train_Data_Ready are found in the local training data 
    if(dim(Local_Case_ID_Votes)[1]==dim(Train_Data_Ready)[1]) # all the cases are found in the train data of the local classifer
    { # start option 1
      
      
      
      Local_Case_ID_Votes <- Local_Case_ID_Votes[order(Local_Case_ID_Votes[,1]),] # arange order according to Case_ID
      Local_Case_ID_Votes <- Local_Case_ID_Votes[,2:dim(Local_Case_ID_Votes)[2]]  # Remove the Case_Id column
      Prop_Vote_Train        <- cbind(Prop_Vote_Train,Local_Case_ID_Votes)                        # Add the votes to Data Collect 
      
      # remove all the local data to ensure next i start from clear
      rm(list=c("Local_RF_Obj",
                "Local_Data",
                "Local_RF",
                "Local_Train_Case_ID",
                "Local_Votes"))
    } # End option 1
    
    ##########################################################
    
    # option 2: not all the cells of Train_Data_Ready are found in the local training data
    if(dim(Local_Case_ID_Votes)[1]<dim(Train_Data_Ready)[1]) # some cases are found in Train_data_Ready but not in the local data
    { # Start option 2
      
      # subset from Train_Data_Ready all the cases that are not in the Local_Train_Data
      Not_Local_Train  <- subset(Train_Data_Ready,!(Prop_Vote_Train[,1] %in% Local_Case_ID_Votes[,1]))
      
      
      # Run the Not_Local_Train cases down the local classifer using predict.randomForest #
      Predict_Local_RF <- predict(object=Local_RF,
                                  newdata=Not_Local_Train[,Exp_Var_2] ,# the explanatory variables for the local train data
                                  type="vote",
                                  norm.votes=TRUE)
      
      # extract the Case_Ids for the Not_Local_Data and cbind with the votes
      Not_Local_Case_ID                    <- as.data.frame(Not_Local_Train[,Case_ID_2])
      Not_Local_Case_ID_Votes              <- cbind(Not_Local_Case_ID,Predict_Local_RF)
      colnames(Not_Local_Case_ID_Votes)[1] <- colnames(Train_Data_Ready)[Case_ID_2]
      
      # bind the votes for the local train data and not local train data
      All_Votes    <- rbind(Local_Case_ID_Votes,Not_Local_Case_ID_Votes)
      
      All_Votes    <- All_Votes[order(All_Votes[,1]),] # arange order according to Case_ID
      All_Votes    <- All_Votes[,2:dim(All_Votes)[2]]  # Remove the Case_Id column  
      Prop_Vote_Train <- cbind(Prop_Vote_Train,All_Votes)    # Add the votes to Data Collect 
      
      # remove all the local data to ensure next i start from clear
      rm(list=c("Local_RF_Obj",
                "Local_Data",
                "Local_RF",
                "Local_Train_Case_ID",
                "Local_Votes",
                "Not_Local_Train",
                "Predict_Local_RF",
                "Not_Local_Case_ID",
                "Not_Local_Case_ID_Votes",
                "All_Votes"))
    } # End option 2
    
    ########################################################## 
    
    # option 3: an error message when there are more cases in the local TRain data than in Train_Data_Ready
    if(dim(Local_Case_ID_Votes)[1]>dim(Train_Data_Ready)[1]) # some cases are found in Trai-data_Ready but not in the local data
    { # Start option 3
      stop(paste("\n","function: Multiplicative_Prop_Votes", 
                 "\n", 
                 "In local classifier: ",
                 LRF_Info[i,1],
                 "\n",
                 " There are less cases in the overall training data than in the local training data",
                 "\n",
                 sep=""))
    } # End option 3
    rm(Local_Case_ID_Votes)
    cat(paste("end votes extraction (training data) for local classifer= ",LRF_Info[i,1],"\n",sep=""))
  } # end the 'i' loop
  
  
  
  ################################################################################
  ### add nodes with no siblings - their parents are internal with frequancy 1 ###
  ### add also a node with TRee_Root and End_Path_Name                         ###
  ### Assign these node a proportion of votes=1                                ###
  ### cbind with Prop_Vote_Train                                                ###
  ################################################################################
  
  
  
  Nodes_as_1   <- subset(Nodes_Info,Nodes_Info$Lev_Above_Clas_In!=1)
  Nodes_as_1   <- subset(Nodes_as_1 ,!is.na(Nodes_as_1 $Lev_Above_Clas_In))
  Nodes_add    <- as.data.frame(setNames(replicate(nrow(Nodes_as_1),numeric(0), simplify = F), Nodes_as_1$Node_Name))
  
  Nodes_add[1:nrow(Train_Data_Ready),]                 <- 1
  Nodes_add[,levels(Unique_Path[1,1])]                 <- 1
  Nodes_add[,levels(Unique_Path[1,ncol(Unique_Path)])] <- 1
  
  
  Prop_Vote_Train              <- cbind(Prop_Vote_Train,Nodes_add)
  colnames(Prop_Vote_Train)[1] <- colnames(Train_Data_Ready)[Case_ID_2]
  
  # add the TRain_TEst_column
  Train_Cat                                          <- data.frame(Train_or_Test="Train")
  Train_Cat[1:nrow(Prop_Vote_Train),"Train_or_Test"] <- "Train"
  Prop_Vote_Train                                    <- cbind(Train_Cat,Prop_Vote_Train)
  
  # remove some objects
  rm(list=c("Nodes_as_1","Nodes_add","Train_Cat"))
  cat(paste("\n","     --> End votes extraction for the Training data","\n",sep=""))
  
  } # end the if(Train_Predict) condition
  
  
  if(!is.null(New_Data)) # there is an input of New_Data
  { # start the if(!is.null(New_Data)) condition
   
    cat(paste("\n","     --> Start votes extraction for the new data","\n",sep=""))
    # order New_Data according to Case_ID 
    New_Data    <- New_Data[order(New_Data[,New_Data_Case_ID]),] 
    
    # start the data frame to collect the votes
    Prop_Vote_New= as.data.frame(New_Data[,New_Data_Case_ID])             
    colnames(Prop_Vote_New)[1]=colnames(Train_Data_Ready)[Case_ID_2]
    
    for (i in 1:dim(LRF_Info)[1]) # loop that runs on each local classifier
    { # start the 'i' loop
      
      cat(paste("\n","Start votes extraction (new data) for local classifer= ",LRF_Info[i,1],sep=""))
      
      Local_RF_Obj  <- All_Local_RF[[i]]        # a list with Local_Data and Local_Rf
     
      Local_RF      <- Local_RF_Obj$Local_RF    # object of class RandomForest for local classifer i
      
      Predict_Local_RF <- predict(object=Local_RF,
                                 newdata=New_Data[,New_Data_Exp_Var] ,# the explanatory variables for the local train data
                                 type="vote",
                                 norm.votes=TRUE)
     
     Prop_Vote_New <- cbind(Prop_Vote_New,Predict_Local_RF)
     cat(paste("\n","End votes extraction (new data) for local classifer= ",LRF_Info[i,1],"\n",sep=""))
    } # end the 'i' loop
    
    
    Nodes_as_1   <- subset(Nodes_Info,Nodes_Info$Lev_Above_Clas_In!=1)
    Nodes_as_1   <- subset(Nodes_as_1 ,!is.na(Nodes_as_1 $Lev_Above_Clas_In))
    Nodes_add    <- as.data.frame(setNames(replicate(nrow(Nodes_as_1),numeric(0), simplify = F), Nodes_as_1$Node_Name))
    
    Nodes_add[1:nrow(New_Data),]                         <- 1
    Nodes_add[,levels(Unique_Path[1,1])]                 <- 1
    Nodes_add[,levels(Unique_Path[1,ncol(Unique_Path)])] <- 1
    
    
    Prop_Vote_New              <- cbind(Prop_Vote_New,Nodes_add)
    colnames(Prop_Vote_New)[1] <- colnames(Train_Data_Ready)[Case_ID_2]
    
    # add the Train_or_Test column
    Train_Cat_New                                         <- data.frame(Train_or_Test="Test")
    Train_Cat_New[1:nrow(Prop_Vote_New),"Train_or_Test"]  <- "Test"
    Prop_Vote_New                                         <- cbind(Train_Cat_New,Prop_Vote_New)
    
    cat(paste("\n","     --> End votes extraction for the new data","\n",sep=""))
    
  } # end the if(!is.null(New_Data)) condition
  
  if(is.null(New_Data))
  {Return_List=list(Prop_Vote_Train = Prop_Vote_Train)}
  
  if(!Train_Predict)
  {Return_List=list(Prop_Vote_New   = Prop_Vote_New)}
  
  
  if(!Bind_Train_New && Train_Predict && !is.null(New_Data))
  {Return_List=list(Prop_Vote_Train = Prop_Vote_Train,
                    Prop_Vote_New   = Prop_Vote_New)}
  
  if(Bind_Train_New && Train_Predict && !is.null(New_Data))
  { 
    Prop_Vote_Full   <- rbind(Prop_Vote_Train,Prop_Vote_New)
    Return_List <- list(Prop_Vote_Full  = Prop_Vote_Full,
                        Prop_Vote_Train = Prop_Vote_Train,
                        Prop_Vote_New   = Prop_Vote_New)
  }
  
  Return_List
} # end function