#' Run the same training data as flat randomForest classifcation

# function, takes as input an object of class Hier.Random.Forest and runs a flat randomforest.By default, use the same rnadomForest parameters as each local classifer in Hie_RF

Run_HRF_As_Flat = function(Hie_RF,                                   # object of class Hier.Random.Forest - the output of Run_HRF
                           mtry         = Hie_RF$call$mtry,          # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier
                           ntree        = Hie_RF$call$ntree,         # number of trees to grow in each local classifier
                           importance   = Hie_RF$call$importance,    # Should importance of predictors be assessed?
                           proximity    = Hie_RF$call$proximity,     # should the proximity be calcualted? 
                           keep.forest  = Hie_RF$call$keep.forest,   
                           keep.inbag   = Hie_RF$call$keep.inbag,    
                           ...)
{ # Start function
  require(randomForest)
  
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call","\n",sep=""))
  
  ## check class of Hie_RF
  if(class(Hie_RF)!="Hier.Random.Forest")
  {stop(paste("\n","In Run_HRF_As_Flat:  Hie_RF should be of class Hier.Random.Forest","\n",sep=""))}
  
  ## Check mtry        

  if(mtry!="tuneRF")
  {
    if(!is.numeric(mtry))
    {stop("\n if mtry is given, it should be a positive integer \n")} 
    
    if(mtry<=0)
    {stop("\n if mtry is given, it should be a positive integer \n")}
    
    if(round(mtry,0)!=mtry)
    {stop("\n if mtry is given, it should be a positive integer \n")}
  }
  
  ## Check ntree        
  
  if(!is.numeric(ntree))
  {stop("\n if ntree is given, it should be a positive integer \n")} 
  
  if(ntree<=0)
  {stop("\n if ntree is given, it should be a positive integer \n")}
  
  if(round(ntree,0)!=ntree)
  {stop("\n if ntree is given, it should be a positive integer \n")}
  
  
  # Check importance
  if(!is.logical(importance))
  {cat(paste("\n", "In Run_HRF_As_Flat, importance should be Logical. default of importance is used","\n",sep=""))
   importance <- Hie_RF$call$importance}
  
  # Check proximity
  if(!is.logical(proximity))
  {cat(paste("\n", "In Run_HRF_As_Flat, proximity should be Logical. default of proximity is used","\n",sep=""))
   proximity <- Hie_RF$call$proximity}
  
  # Check keep.forest
  if(!is.logical(keep.forest))
  {cat(paste("\n", "In Run_HRF_As_Flat, keep.forest should be Logical. default of keep.forest is used","\n",sep=""))
   keep.forest <- Hie_RF$call$keep.forest}
  
  # Check keep.inbag
  if(!is.logical(keep.inbag))
  {cat(paste("\n", "In Run_HRF_As_Flat, keep.inbag should be Logical. default of keep.forest is used","\n",sep=""))
   keep.inbag <- Hie_RF$call$keep.inbag}
  
  cat(paste("\n", "-->  Call Verified","\n",sep=""))
  
  ######################################################################################
  ### STEP 2 - prepare the flat training data and the explantory variable            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n", "-->  Preparing data for flat RandomForest analysis","\n",sep=""))
  
  cat(paste("\n", "Extracting info from Hie_RF","\n",sep=""))
  # extract info from Hie_RF
  Train_Data_Ready  <- Hie_RF$Train_Data_Ready
  Unique_Path       <- Hie_RF$Hier_Struc$Unique_Path
  End_Path_Name     <- Hie_RF$call$End_Path_Name             
  Case_ID_2         <- Hie_RF$Case_ID_2
  Path_Name_2       <- Hie_RF$Path_Name_2
  Hie_Levels_2      <- Hie_RF$Hie_Levels_2
  Exp_Var_2         <- Hie_RF$Exp_Var_2
  
  ######################
  
  cat(paste("\n", "Identifying terminal node for each case","\n",sep=""))
  
  # identify the terminal node of each path
  Hie_Data <- Get_Terminal_Node(End_Path_Name = End_Path_Name,                  #  Character, the name used to represent the End_Path
                                Unique_Path   = unique(Train_Data_Ready[,Hie_Levels_2]))
  
  # add the Path_Name
  Path_Col <-unique(Train_Data_Ready[,Path_Name_2,drop=FALSE])
  Hie_Data <-cbind(Path_Col,Hie_Data)
  
  # start the Train_Flat_Y data frame
  Train_Flat_Y <- data.frame(Obs_Term_Node = c(NA))
  
  # identify the terminal node for each case
  for(count_case in 1:nrow(Train_Data_Ready))
  {
    Case_Path_Num  <- Train_Data_Ready[count_case,Path_Name_2]
    
    Case_Term_Node <- Hie_Data[match(Case_Path_Num,Hie_Data[,1]),
                               ncol(Hie_Data)] 
    Train_Flat_Y[count_case,"Obs_Term_Node"] <- Case_Term_Node
    rm(list=c("Case_Path_Num","Case_Term_Node"))
  }
  
  rm(count_case)
  
 
 
  ######################
   
  cat(paste("\n", "Identifying explanatory varaibles for each case","\n",sep=""))
  
  # the explanatory variables
  Train_Exp_X <- Train_Data_Ready[,Exp_Var_2]
  
  
  cat(paste("\n", "-->  Data ready for flat RandomForest analysis","\n",sep=""))
  
  
  ######################################################################################
  ### STEP 3 - running the flat random forest                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n", "-->  Running the flat RandomForest analysis","\n",sep=""))
  
  Train_y <- factor(Train_Flat_Y$Obs_Term_Node)
  
  # Adjaust the mtry according to user choice - run tuneRF if needed
  if(mtry=="tuneRF")
  {
    cat(paste("\n","Estimating mtry (tuneRF) for the flat randomForest ","\n",sep=""))
    Tune_RF    <- tuneRF(y=Train_y ,x=Train_Exp_X)   # run the tuneRF function of RandomForest
    mtry_local <- Tune_RF[match(x=min(Tune_RF[,2]),table=Tune_RF[,2]),1] # identify the suggested mtry
  }else {mtry_local <- mtry}
  
  cat(paste("\n","mtry = ",mtry_local,"\n",sep="" ))
  
  cat(paste("\n","start randomForest","\n",sep=""))
  # run the randomForest with the user defined or default parameters 
  Flat_RF <- randomForest(y            = Train_y,
                          x            = Train_Exp_X,
                          mtry         = mtry_local,
                          ntree        = ntree,
                          importance   = importance,
                          proximity    = proximity,
                          keep.forest  = keep.forest,
                          keep.inbag   = keep.inbag) 
 
  
  cat(paste("\n", "--> randomForest object was created for the flat classification","\n",sep=""))
  
  ######################################################################################
  ### STEP 4 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  Train_Flat_Y <- cbind(Train_Data_Ready[Case_ID_2],Train_Flat_Y)
  
  Return_List <- list(Flat_RF         = Flat_RF,       # Object of class randomForest, the result of flat classification
                      Train_Flat_Case = Train_Flat_Y,  # data frame contating the _case_ID and the Obs_Term_Node of each case
                      call            = match.call())  # the call
  
  
} # End_function