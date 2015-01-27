

Run_HRF=function(Train_Data,                      # the data frame with the training data
                 Case_ID,                         # a character or integer, specifying the name of the column or number of column  (respectivaly) that should be used as case IDs in Train_Data,                                  
                 Hie_Levels,                      # a vector of character or integers, containing the names of column or the numberrs of the columns (respectively) of the hierarchical levels in Train_Data. order  of columns in Train_Data should be from the root to the leaves
                 Internal_End_Path = FALSE,       # Logical (TRUE/FALSE)- are all terminal nodes ending in the lowest level of the hierarchy?
                 End_Path_Name     = "End_Path",  # Character - the name used in level i+1 for terminal nodes ending in level i.
                 Root_Include      = FALSE,       # Logical (TRUE/FALSE)is the tree root included in Hie_Levels?
                 Root_Name         = "TREE_ROOT", # Character - name to use for the tree root
                 Exp_Var           = NA,          # a vector of character or integers, containing the names of column or the numberrs of the columns (respectively in Train_Data to be used as explanatory variables in the random forest, default takes all columns other than Case_ID and Hie_Levels                            
                 mtry              = if(!is.integer(mtry)) mtry="tuneRF",    # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier. setting mtry="tuneRF_2" will use a sligtly different version of tuneRF
                 ntree             = 500,         # number of trees to grow in each local classifier
                 importance        = TRUE,        # Should importance of predictors be assessed?
                 proximity         = TRUE,        # should the proximity be calcualted? 
                 keep.forest       = TRUE,        # Should proximity measure among the rows be calculated?
                 keep.inbag        = TRUE,        # should a n by ntree matrix be returned that keeps track of which samples are "in-bag” in which trees (but not how many times, if sampling with replacement). Required for the permutation-based accuracy assesments
                 ...)
{
  
  require(randomForest)
  
  
  ################################################
  ### Section 1 - Check input + error messages ###
  ################################################
  cat(paste("\n","####################################", "\n",sep=""))
  cat(paste("\n","--> Performing initial checks...", "\n",sep=""))
  
  #################
  # check Missing #
  #################
  
  if(missing(Case_ID))
  {stop("\n No Case_ID was specified in the Train_Data")}
  
  if(missing(Hie_Levels))
  {stop("\n No Hie_Levels were specified in the Train_Data")}
  
  if(missing(Exp_Var))
  {cat("Note! all columns other than those specified by Case_ID and Hie_Levels will be used as input variables")}
  
  #################
  # Check Case_ID #
  #################
  
  # Change from a character to numeric column number 
  
  if(is.character(Case_ID))
  {Case_ID_Cha <- Case_ID
   Case_ID     <- match(Case_ID,names(Train_Data))}
  
  # checks if the column specified as Case_ID contains a single entery for each ID
  Check_Case_ID(Train_Data[,Case_ID])
  
  #################### 
  # Check Hie_Levels # 
  ####################
  
  # Change from a character to numeric column number 
  if(is.character(Hie_Levels))
  {Hie_Levels  <- match(Hie_Levels,names(Train_Data))}
  
  # checks if the columns specified in Hie_Levels contains factors
  Check_Hie_Levels(Train_Data[,Hie_Levels])
  
  #################### 
  # Check mtry       # 
  ####################
  if(mtry!="tuneRF")
  {
    if(!is.numeric(mtry))
    {stop("\n if mtry is given, it should be a positive integer \n")} 
    
    if(mtry<=0)
    {stop("\n if mtry is given, it should be a positive integer \n")}
    
    if(round(mtry,0)!=mtry)
    {stop("\n if mtry is given, it should be a positive integer \n")}
  }
  ###################
  # Arrange Exp_Var #
  ###################
  
  if(is.character(Exp_Var))
  {Exp_Var  <- match(Exp_Var,names(Train_Data))}
  
  # when Exp_Var is not given by the user 
  if(is.na(Exp_Var))
  {
    Exp_Var <- 1:length(names(Train_Data)) 
    Exp_Var <- Exp_Var[Exp_Var!=Case_ID]         # Remove Case_ID
    Exp_Var <- Exp_Var[!Exp_Var %in% Hie_Levels] # Remove Hie_Levels 
  }
  
  ###############################
  # Check NA and missing values #
  ###############################
  
  Work_Data <- Train_Data[,c(Case_ID,Hie_Levels,Exp_Var)]  
 

  # Check NAs
 
  if(length(which(Work_Data[]=="" | Work_Data[]==" " | is.na(Work_Data)))!=0)
  {stop("\n At least one columns contains missing values or NA. \n Please remove missing values from  Train_Data columns of Case_ID, Hie_Levels and Exp_Var")}

  rm(Work_Data)

  cat(paste("\n","--> Initial checks OK", "\n",sep=""))
  cat(paste("\n","####################################", "\n",sep=""))
########################################################### 
### Section 2 - identify the structure of the hierarchy ###
###########################################################

  cat(paste("\n","--> Identifying hierarchical structure", "\n",sep=""))

  # create the Unique_Path data frame, to be used by the Ide_Hie_Str function
  Unique_Path <- Train_Data[,Hie_Levels]
  
  # Run the Ide_Hie_Str function
  
  Hier_Struc <- Ide_Hie_Str(Unique_Path       = Unique_Path, 
                            Internal_End_Path = Internal_End_Path, 
                            End_Path_Name     = End_Path_Name,
                            Root_Include      = Root_Include,
                            Root_Name         = Root_Name)

# extract the local clasifers info and nodes info, as well as the updated Unique_Path
  LRF_Info     <- Hier_Struc$LRF_Info
  Nodes_Info   <- Hier_Struc$Nodes_Info
  Unique_Path  <- Hier_Struc$Unique_Path

cat(paste("\n","--> Hierarchical structure idenitfied", "\n",sep=""))
cat(paste("\n","####################################", "\n",sep=""))

################################################################# 
### Section 3 - Arrange the Train_Data_Ready table            ###         
#################################################################

cat(paste("\n","--> Restructuring the Training data", "\n",sep=""))

#  the structure of the train data table should be: Case_ID, Path_Name, Hie_Levels, Exp_Var
#  THe hierarchy details should be as in Unique_Path, inlcuding the Root and End_Path levels
#  Case_ID_2, Path_Name_2, Hie_Levels_2, Exp_Var_2 should refere to column number in the arranged Train_Data data frame

# Data for Case_ID and Exp_Var
Case_ID_Data <- Train_Data[,Case_ID]
Exp_Var_Data <- Train_Data[,Exp_Var]

# Add Path_Name column to Unique_Path
Path_Name        <- c(1:dim(Unique_Path)[1])
Unique_Path_Name <- cbind(Path_Name,Unique_Path)

# start the Hie_Levels_Data
Hie_Levels_Data     <- Unique_Path_Name[1,]
Hie_Levels_Data[1,] <- NA

if (Root_Include)
      {Path_Data <- Unique_Path_Name[,Hie_Levels]   }
if (!Root_Include)
      {Path_Data <- Unique_Path_Name[,Hie_Levels+1] }




# a loop that runs on each row of train data
for (i in 1:dim(Train_Data)[1])
{
    # the Path of the corrent case
    Train_Path <- Train_Data[i,Hie_Levels]
    
    # a loop that finds the path in Path_Data
    for (j in 1:dim(Unique_Path)[1])
    {
      if(all(Path_Data[j,] == Train_Path)) # if this is the correct path
      {Hie_Levels_Data[i,] <- Unique_Path_Name[j,]  } # insert all the hierarchical information to the  row
    }
}   
   

# by Now the three main informations are ready - need to update the columns numbers

Train_Data_Ready <- cbind(Case_ID_Data,Hie_Levels_Data,Exp_Var_Data)

colnames(Train_Data_Ready)[1] <- Case_ID_Cha

Case_ID_2     <- 1 
Path_Name_2   <- 2 
Hie_Levels_2  <- c(3:(1+dim(Unique_Path_Name)[2]))
Exp_Var_2     <- c((max(Hie_Levels_2)+1):dim(Train_Data_Ready)[2])

Path_Name_Data  <- Hie_Levels_Data[,1]
Hie_Levels_Data <- Hie_Levels_Data[,2:dim(Hie_Levels_Data)[2]]

# order  Train_Data_Ready according to Case_ID
Train_Data_Ready <- Train_Data_Ready[order(Train_Data_Ready[,1]),]

# From now on - 
# 1. Train_Data_Ready is the data frame to work with, 
# 2. Case_ID_2, Path_Name_2, Hie_Levels_2 and Exp_Var_2 refer to column numbers in Train_Data_Ready
# 3. Case_ID_Data, Path_Name_Data, Hie_Levels_Data and Exp_Var_Data contains the relevant information from Train_Data_Ready
# 



cat(paste("\n","--> Training data - Restructured", "\n",sep=""))
cat(paste("\n","####################################", "\n",sep=""))

########################################################### 
### Section 4 - Run the local classifier loop           ###
###########################################################

cat(paste("\n","--> Starting RandomForest", "\n",sep=""))

# create the list to collect the info on each local classifer
All_Local_RF <- vector("list", dim(LRF_Info)[1])

# The location in All_Local_RF in which the data and model of each local classifer is stored
Classifer_in_All_Local_RF <- data.frame(Classifier_ID = c(NA),
                                        Place_In_List = c(NA))


# a  loop that runs on each local classifers
for (i in 1:dim(LRF_Info)[1])
{
 # use Subset_LRF_Cases to subset the relevant input information for the local classifier
  Local_Data <- Subset_LRF_Cases(Local_Class      = i,                  # Integer, specifying the row in LRF_Info for which subset of Train_Data should be returned
                                 LRF_Info         = LRF_Info,           # data frame, specifying each local classifer as a row- the output of Ide_Hie_Str
                                 Train_Data_Ready = Train_Data_Ready,   # the data frame with the training data  
                                 Hie_Levels_2     = Hie_Levels_2,       # columns in Train_Data_ready that contains the information on the hierarchical levels
                                 Exp_Var_2        = Exp_Var_2)          # columns in Train_Data_ready that contains the iexplanatory variables
  
  Classifer_in_All_Local_RF[i,1] <- LRF_Info[i,1]
  Classifer_in_All_Local_RF[i,2] <- i
  
  Y_RF <- Local_Data$Local_Train_Cat   # the categories to be classifed
  X_RF <- Local_Data$Local_Train_Vars  # the explanatory variables
  
  cat(paste("\n","####################################", "\n",sep=""))
  cat(paste("\n","Starting the RandomForest algorithm for local classifer: ",LRF_Info[i,1],"\n",sep=""))
  
  # estimate mtry
  if(mtry=="tuneRF")
    {
    cat(paste("\n","Estimating mtry (tuneRF) for local classifer: ",LRF_Info[i,1],"\n",sep=""))
    Tune_RF    <- tuneRF(y=Y_RF,x=X_RF)   # run the tuneRF function of RandomForest
    mtry_local <- Tune_RF[match(x=min(Tune_RF[,2]),table=Tune_RF[,2]),1] # identify the suggested mtry
    }
  
  # estimate mtry
  if(mtry=="tuneRF_2")
  {
    cat(paste("\n","Estimating mtry (tuneRF) for local classifer: ",LRF_Info[i,1],"\n",sep=""))
    Tune_RF    <- tuneRF_2(y=Y_RF,x=X_RF)   # run the tuneRF function of RandomForest
    mtry_local <- Tune_RF[match(x=min(Tune_RF[,2]),table=Tune_RF[,2]),1] # identify the suggested mtry
  }
  
  else {mtry_local <- mtry}
  
  cat(paste("\n","mtry = ",mtry_local,"\n",sep="" ))
 
  cat(paste("\n","Fitting the RandomForest algorithm","\n",sep=""))
  
  # run the local RandomForest, using the arguemnt from Run_HRF call
  Local_RF <- randomForest(y            = Y_RF,
                           x            = X_RF,
                           mtry         = mtry_local,
                           ntree        = ntree,
                           importance   = importance,
                           proximity    = proximity,
                           keep.forest  = keep.forest,
                           keep.inbag   = keep.inbag) 
  
  cat(paste("\n","Storing the local RandomForest object and additional data","\n",sep=""))
  # assign the Local_Data and the Local_RF to their slot in the All_Local_RF list 
  All_Local_RF[[i]] <- list(Local_Data = Local_Data,
                            Local_RF   = Local_RF) 
  
  cat(paste("\n","Ending the RandomForest algorithm for local classifer: ",LRF_Info[i,1],"\n",sep=""))
  cat(paste("\n","####################################", "\n",sep=""))
}


cat(paste("\n","--> Ending RandomForest", "\n",sep=""))
cat(paste("\n","####################################", "\n",sep=""))

#return(Hier_Struc)
Hie_RF <- list(Hier_Struc       = Hier_Struc,
               Train_Data_Ready = Train_Data_Ready,
               Case_ID_2        = Case_ID_2,
               Path_Name_2      = Path_Name_2,
               Hie_Levels_2     = Hie_Levels_2,
               Exp_Var_2        = Exp_Var_2,
               All_Local_RF     = All_Local_RF,
               Classifer_in_All_Local_RF = Classifer_in_All_Local_RF, # The location in All_Local_RF in which the data and model of each local classifer is stored
               call             = match.call()                        # the call
               )
class(Hie_RF) <- "Hier.Random.Forest"
return(Hie_RF)


} # end main function loop


