#' Runs the hierarchical randomForest on the training data.
#' 
#' Tha main function of the package that identifies the hierarchial class 
#' structure from the input training data and runs a randomForest as the local 
#' classifier at each parent node. A local randomForest classifer is added for 
#' each internal node that has more than one child node. Returns an object of 
#' class Hier.Random.Forest that contains all the local randomForest objects
#' along with additional information on the hierachical structure.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param Train_Data         Data frame with the training data.
#' @param Case_ID            A character or integer, specifying the name or
#'   column number used as case IDs in \code{Train_Data}. Case ID values
#'   must be unique.
#' @param Hie_Levels         A vector of characters or integers, containing the
#'   names or column numbers of the hierarchical levels in \code{Train_Data}. Order  of
#'   columns in \code{Train_Data} should be from the tree root to the terminal nodes.
#' @param Internal_End_Path  Logical, \code{TRUE} if all terminal nodes are in the
#'   lowest level of the class hierarchy.
#' @param End_Path_Name      Character - the name used in level i+1 for terminal
#'   nodes ending in level i. The only factor in the class hierarchy that can
#'   apear in more than one hierarchy level
#' @param Root_Include       Logical, \code{TRUE} if \code{ROOT_NAME} is included in
#'   \code{Hie_Levels}.
#' @param Root_Name          Character - name to use for the tree root.
#' @param Exp_Var            A vector of characters or integers, containing the
#'   names or column numbers in \code{Train_Data} with the explanatory variables. 
#'   Default takes all columns other than \code{Case_ID} and
#'  \code{Hie_Levels}.
#' @param mtry               Number of variables randomly sampled as candidates
#'   at each split. Note that the default is to use \code{tuneRF} function of
#'   \code{randomForest} package for each local classifier. Setting \code{mtry="tuneRF_2"} will use a
#'   sligtly different version of \code{tuneRF}.
#' @param ntree              Number of trees to grow in each local classifier.
#'   See \code{?randomForest} for additional details.
#' @param importance         Logical, if \code{TRUE} importance of variables will be
#'   assesed and saved at each local classifer. See \code{?randomForest} for additional
#'   details.
#' @param proximity          Logical, if \code{TRUE}, proximity will be calcualted for
#'   each local classifier. See \code{?randomForest} for additional details.
#' @param keep.forest        Logical, if \code{TRUE} (recommended) the forest of each
#'   local classifer will be retained. If \code{FALSE}, the predict and performance functions will return an error. 
#' @param keep.inbag         Logical, if \code{TRUE} an \emph{n} by \code{ntree} matrix be returned
#'   that keeps track of which cases are "in-bag” in which trees. \emph{n} being the
#'   number of cases in the training set of a local classifer. Required for the
#'   permutation-based performance assesments.
#' @param ...                Optional parameters to be passed to the low level
#'   functions.
#' 
#' @details  Implements Breiman's (2001) random forest algorithm based on the \code{\link{randomForest}} package for classification.
#'   
#'   
#' @return  An object of class \code{"Hier.Random.Forest"} that constains the following: 
#'   \item{Hier_Struc}{A list containing three data frames: 
#'   \describe{
#'   \item{\code{$LRF_Info} - information on each local randomForest, containing the following columns:}{}
#'      {\tabular{lll}{
#'       \code{"Classifer_ID"} \tab \tab The name of the local classifer. \cr
#'       \code{"Par_Level"}    \tab \tab The name of the parent node in the local classifer. \cr
#'       \code{"Par_Name"}     \tab \tab The name of the parent node in the local classifer. \cr
#'       \code{"Par_Clas_ID"}  \tab \tab The name of the classifer in which the parent node was classfied.  \cr
#'       \code{"Num_Sib_Tot"}  \tab \tab The number of sibling nodes in the local classifer.  \cr
#'       \code{"Num_Sib_Ter"}  \tab \tab The number of terminal sibling nodes in the local classifer.  \cr
#'       \code{"Num_Sib_Int"}  \tab \tab The number of internal sibling nodes in the local classifer.  \cr
#'        }}
#'   \item{\code{$Nodes_Info} - information on each internal or terminal node in the class hierarchy, containing the following columns:}{}
#'       {\tabular{lll}{
#'       \code{"Node_NamD"}        \tab \tab The name of the node. \cr
#'       \code{"Node_Level"}       \tab \tab  The level of the node in the class hierarchy. \cr
#'       \code{"Node_Freq"}        \tab \tab  The number of times the node appears in the training data. \cr
#'       \code{"Node_Par_Lev"}     \tab \tab  The level in which the parent of the node resides. \cr
#'       \code{"Node_Par_Name"}    \tab \tab The name of the node's parent node. \cr
#'       \code{"Term_Int_node"}    \tab \tab If the node is terminal or internal. \cr
#'       \code{"Clas_yes_no"}      \tab \tab If the node is the parent node in a local classifer. \cr
#'       \code{"Classifier_ID"}    \tab \tab If the node is the parent node in a local classifer, the name of the local classifer. \cr
#'       \code{"Classified_In"}    \tab \tab The name of the local classifer in which the node is classified. \cr
#'       \code{"Lev_Above_Clas_In"}\tab \tab The number of levels above the node's level in which it is  classified. \cr
#'        }}
#'   \item{\code{$Unique_Path} - information on all the unique pathes from the tree root to each of the terminal nodes. 
#'   Include two additional columns for \code{Root_Name} and \code{End_Path_Name}.}{}}}
#'   
#'   \item{Train_Data_Ready}{Data frame containing the  training data after 
#'   restructuring to the usable format of \code{Run_HRF}} 
#'   \item{Case_ID_2}{Column 
#'   number in \code{Train_Data_Ready} containing the case IDs} 
#'   \item{Path_Name_2}{Column number in Train_Data_Ready containing the path 
#'   names} 
#'   \item{Hie_Levels_2}{Column number in \code{Train_Data_Ready} containing the
#'   hierarchical information} 
#'   \item{Exp_Var_2}{Column number in \code{Train_Data_Ready} containing the explanatory variables.}
#'   \item{All_Local_RF}{The main large list with all the information on 
#'   each local classifer. For each local classifer, the function returns a list
#'   consisting of three objects:
#'   \tabular{lll}{
#'   \code{Local_LRF_Info}\tab \tab The information on the local randomForest as found in the \code{$Hier_Struc$LRF_Info} data frame. \cr
#'   \code{Local_Data}    \tab \tab The Case ID's of all cases that were used as training data for the local randomForest. \cr
#'   \code{Local_RF}      \tab \tab an object of class ranomForest for the local classifier. See package randomForest for details. \cr
#'   }}
#'   \item{Order_Local_RF}{data frame containing the order in which local randomForests are stored in the \code{All_Local_RF} list.}
#'   \item{call}{the call to function \code{Run_HRF}.}
#'   
#'   @seealso 
#'   \code{\link{Extract_Votes}} for extracting the proportion of votes, 
#'   \code{\link{HRF_Importance}} for variable importance,
#'   \code{\link{Run_HRF_As_Flat}} for running a flat classification,
#'   \code{\link{Plot_Hie_Tree}} for plotting the class structure,
#'   \code{\link{HRF_Performance}} for assising accuracy,
#'   
#'   @references
#'   Breiman, L. 2001. Random forests. \emph{Machine Learning} 45:5–32.
 




Run_HRF=function(Train_Data, Case_ID, 
                 Hie_Levels, 
                 Internal_End_Path = FALSE,       
                 End_Path_Name     = "End_Path",  
                 Root_Include      = FALSE,       
                 Root_Name         = "TREE_ROOT", 
                 Exp_Var           = NA,                                    
                 mtry              = if(!is.integer(mtry)) mtry="tuneRF",   
                 ntree             = 500,         
                 importance        = TRUE,        
                 proximity         = TRUE,        
                 keep.forest       = TRUE,        
                 keep.inbag        = TRUE,        
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
  mtry_local <- mtry
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
  } #else {mtry_local <- mtry}
  
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
  All_Local_RF[[i]] <- list(Local_LRF_Info =  Local_Data$Local_Class_Info,   # the infromation on the local classifier
                            Local_Data = Local_Data$Local_Train_Case_ID,    # the Case Id's of the local training data
                            Local_RF   = Local_RF)                           # The local randomForest object
  
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
               Order_Local_RF   = Classifer_in_All_Local_RF, # The location in All_Local_RF in which the data and model of each local classifer is stored
               call             = match.call()                        # the call
               )
class(Hie_RF) <- "Hier.Random.Forest"
return(Hie_RF)


} # end main function loop


