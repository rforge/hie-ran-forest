

# function, takes as input an object of class HRF and runs a flat randomforest.By default, use the same rnadomForest parameters as each local classifer in hie.RF

RunFlatHRF = function(hie.RF,                                   # object of class HRF - the output of RunHRF
                      mtry         = hie.RF$call$mtry,          # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier
                      ntree        = hie.RF$call$ntree,         # number of trees to grow in each local classifier
                      importance   = hie.RF$call$importance,    # Should importance of predictors be assessed?
                      proximity    = hie.RF$call$proximity,     # should the proximity be calcualted? 
                      keep.forest  = hie.RF$call$keep.forest,   
                      keep.inbag   = hie.RF$call$keep.inbag,    
                      ...)
{ # Start function
  #require(randomForest)
  
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n" , "-->  Veryfing Call" , "\n" , sep=""))
  
  # treat the missing call parameters if needed:
  if(is.null(mtry))        {mtry        <- "tuneRF"}
  if(is.null(ntree))       {ntree       <- 500} 
  if(is.null(importance))  {importance  <- TRUE} 
  if(is.null(proximity))   {proximity   <- TRUE} 
  if(is.null(keep.forest)) {keep.forest <- TRUE}
  if(is.null(keep.inbag))  {keep.inbag  <- TRUE}
  
  
  
  ## check class of hie.RF
  if(class(hie.RF)!="HRF")
  {stop(paste("\n" , "In RunFlatHRF:  hie.RF should be of class HRF" , "\n" , sep=""))}
  
  ## Check mtry        

  if(mtry!="tuneRF" &&  mtry!="tuneRF2")
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
  {cat(paste("\n" , "In RunFlatHRF, importance should be Logical. default of importance is used" , "\n" , sep=""))
   importance <- hie.RF$call$importance
   if(is.null(importance))  {importance  <- TRUE}}
  
  # Check proximity
  if(!is.logical(proximity))
  {cat(paste("\n" , "In RunFlatHRF, proximity should be Logical. default of proximity is used" , "\n" , sep=""))
   proximity <- hie.RF$call$proximity
   if(is.null(proximity))   {proximity   <- TRUE}}
  
  # Check keep.forest
  if(!is.logical(keep.forest))
  {cat(paste("\n" , "In RunFlatHRF, keep.forest should be Logical. default of keep.forest is used" , "\n" , sep=""))
   keep.forest <- hie.RF$call$keep.forest
   if(is.null(keep.forest)) {keep.forest <- TRUE}}
  
  # Check keep.inbag
  if(!is.logical(keep.inbag))
  {cat(paste("\n" , "In RunFlatHRF, keep.inbag should be Logical. default of keep.forest is used" , "\n" , sep=""))
   keep.inbag <- hie.RF$call$keep.inbag
   if(is.null(keep.inbag))  {keep.inbag  <- TRUE}}
  
  cat(paste("\n" , "-->  Call Verified" , "\n" , sep=""))
  
  ######################################################################################
  ### STEP 2 - prepare the flat training data and the explantory variable            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n" , "-->  Preparing data for flat RandomForest analysis" , "\n" , sep=""))
  
  cat(paste("\n" , "Extracting info from hie.RF" , "\n" , sep=""))
  
  # extract info from hie.RF
  train.data.ready  <- hie.RF$train.data.ready
  unique.path       <- hie.RF$hier.struc$unique.path
  end.path.name     <- hie.RF$call$end.path.name 
  if(is.null(end.path.name)){end.path.name <- "END.PATH"}
  case.ID           <- hie.RF$case.ID
  path.name         <- hie.RF$path.name
  hie.levels        <- hie.RF$hie.levels
  exp.var           <- hie.RF$exp.var
  
  ######################
  
  cat(paste("\n" , "Identifying terminal node for each case" , "\n" , sep=""))
  
  # identify the terminal node of each path
  hie.data <- GetTerminalNode(end.path.name = end.path.name,                  #  Character, the name used to represent the end.path
                              unique.path   = unique(train.data.ready[ , hie.levels]))
  
  # add the path.name
  path.col <- unique(train.data.ready[ , path.name , drop=FALSE])
  hie.data <- cbind(path.col , hie.data)
  
  # start the train.flat.Y data frame
  train.flat.Y <- data.frame(obs.term.node = c(NA))
  
  # identify the terminal node for each case
  for(count.case in 1:nrow(train.data.ready))
  {
    case.path.num  <- train.data.ready[count.case , path.name]
    
    case.term.node <- hie.data[match(case.path.num , hie.data[ , 1]),
                               ncol(hie.data)] 
    train.flat.Y[count.case , "obs.term.node"] <- case.term.node
    rm(list=c("case.path.num" , "case.term.node"))
  }
  
  rm(count.case)
  
 
 
  ######################
   
  cat(paste("\n" , "Identifying explanatory varaibles" , "\n" , sep=""))
  
  # the explanatory variables
  train.exp.X <- train.data.ready[ , exp.var]
  
  
  cat(paste("\n" , "-->  Data ready for flat RandomForest analysis" , "\n" , sep=""))
  
  
  ######################################################################################
  ### STEP 3 - running the flat random forest                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n" , "-->  Running the flat RandomForest analysis" , "\n" , sep=""))
  
  train.Y <- factor(train.flat.Y$obs.term.node)
  
  mtry.local <- mtry
  
  # estimate mtry ith tuneRF
  if(mtry=="tuneRF")
  {
    cat(paste("\n" , "Estimating mtry (tuneRF) for the flat randomForest " , "\n" , sep=""))
    tune.RF.opt <- tuneRF(y = train.Y , x = train.exp.X , plot = FALSE)   # run the tuneRF function of RandomForest
    mtry.local  <- tune.RF.opt[match(x = min(tune.RF.opt[ , 2]) , table = tune.RF.opt[ , 2]) , 1] # identify the suggested mtry
  } 
  
  # estimate mtry ith tuneRF2
  if(mtry=="tuneRF2")
  {
    cat(paste("\n","Estimating mtry (tuneRF2) for the flat randomForest ","\n",sep=""))
    tune.RF.opt <- tuneRF2(y = train.Y , x = train.exp.X)   # run the tuneRF2 function of HieRanFor
    mtry.local  <- tune.RF.opt[match(x = min(tune.RF.opt[ , 2]), table = tune.RF.opt[ , 2]) , 1] # identify the suggested mtry
  } 
  
  cat(paste("\n","mtry = ", mtry.local , "\n" , sep="" ))
  
  
  
  cat(paste("\n" , "start randomForest","\n" , sep=""))
  
  # run the randomForest with the user defined or default parameters 
  flat.RF <- randomForest(y            = train.Y,
                          x            = train.exp.X,
                          mtry         = mtry.local,
                          ntree        = ntree,
                          importance   = importance,
                          proximity    = proximity,
                          keep.forest  = keep.forest,
                          keep.inbag   = keep.inbag) 
 
  
  cat(paste("\n" , "--> randomForest object was created for the flat classification" , "\n" , sep=""))
  
  ######################################################################################
  ### STEP 4 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  train.flat.Y <- cbind(train.data.ready[case.ID] , train.flat.Y)
  
  return.list <- list(flat.RF         = flat.RF,       # Object of class randomForest, the result of flat classification
                      train.flat.case = train.flat.Y,  # data frame contating the case.ID and the obs.term.node of each case
                      call            = match.call())  # the call
  
  
} # End.function