#' Run the Hierarchical randomForest on the training data.
#' 
#' The main function of the package that identifies the hierarchical class 
#' structure from the input training data and runs a \code{randomForest}
#' algorithm as the local classifier at each internal node that has more than
#' one child node. Returns an object of class \code{"HRF"} that contains all the
#' local \code{randomForest} objects along with additional information on the
#' hierarchical structure.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param train.data         Data frame with the training data.
#' 
#' @param case.ID            A character or integer, specifying the name or 
#'   column number used as case IDs in \code{train.data}. Case ID values must be
#'   unique.
#'   
#' @param hie.levels         A vector of characters or integers, containing the 
#'   names or column numbers of the hierarchical levels in \code{train.data}.
#'   Order of columns in \code{train.data} should be from the tree root to the
#'   terminal nodes.
#'   
#' @param internal.end.path  Logical, \code{FALSE} (default) if all terminal
#'   nodes are in the lowest level of the class hierarchy. \code{TRUE} if some
#'   terminal noes are not at the lowest hierarchy level.
#'   
#' @param end.path.name      Character - the factor value used in level \emph{i
#'   + 1} for terminal nodes ending in level \emph{i}. The only factor in the
#'   class hierarchy that can appear in more than one hierarchy level.
#'   
#' @param root.include       Logical, \code{TRUE} if \code{root.name} is
#'   included in \code{hie.levels}.
#'   
#' @param root.name          Character - name to use for the tree root.
#' 
#' @param exp.var            A vector of characters or integers, containing the 
#'   names or column numbers in \code{train.data} with the explanatory
#'   variables. Default takes all columns other than \code{case.ID} and 
#'   \code{hie.levels}.
#'   
#' @param mtry               Number of variables randomly sampled as candidates 
#'   at each split. Note that the default is to use \code{tuneRF} function of 
#'   \code{randomForest} package for each local classifier. Setting \code{mtry}
#'   to \code{\link{tuneRF2}} will use a slightly different version of
#'   \code{tuneRF}.
#'   
#' @param ntree              Number of trees to grow in each local classifier. 
#'   See \code{?randomForest} for additional details.
#'   
#' @param importance         Logical, if \code{TRUE} importance of variables
#'   will be assessed and saved at each local classifier. See
#'   \code{?randomForest} for additional details.
#'   
#' @param proximity          Logical, if \code{TRUE}, proximity will be
#'   calculated for each local classifier. See \code{?randomForest} for
#'   additional details.
#'   
#' @param keep.forest        Logical, if \code{TRUE} (recommended) the forest of
#'   each local classifier will be retained. If \code{FALSE}, the predict and
#'   performance functions will return an error.
#'   
#' @param keep.inbag         Logical, if \code{TRUE} an \emph{n} by \code{ntree}
#'   matrix be returned that keeps track of which cases are in-bag in which
#'   trees. \emph{n} being the number of cases in the training set of a local
#'   classifier.
#'   
#' @param \dots    Optional parameters to be passed to low level functions.
#'  
#' @details  Implements Breiman's (2001) random forest algorithm based on the
#'   \code{\link{randomForest}} package for classification. \cr In hierarchical
#'   randomForest, the additional information on class hierarchy is used to
#'   train more than 1 local classifier. Each case of a certain terminal node is
#'   also used as a training case for any internal nodes in the path leading
#'   from the tree root to the terminal node. For example, in the
#'   \code{"\link{OliveOilHie}"} dataset (figure below), a training case for the
#'   terminal class \emph{Apulia.north} is used in local classifier \emph{C.5}
#'   to separate \emph{Apulia.north} and \emph{Apulia.south}. The same case also
#'   represent \emph{Apulia} in local classifier \emph{C.4} to separate
#'   \emph{Apulia} from \emph{Calabria} and \emph{Sicily}. Finally, the same
#'   case represent \emph{South} in local classifier \emph{C.1}.
#'  
#'    
#' \if{html}{\figure{OliveOilClassHie.jpeg}}
#' \if{latex}{\figure{OliveOilClassHie.jpeg}{options: width=7cm}}
#' 
#' @return  An object of class \code{"HRF"} that contains the following: 
#'   \item{hier.struc}{A list containing three data frames: 
#'   \describe{
#'   \item{\code{$lRF.info} - information on each local randomForest, containing
#'   the following columns:}{}
#'      {\tabular{lll}{
#'       \code{"classifier.ID"} \tab \tab The name of the local classifier,
#'       labelled as C.1, C.2... \cr
#'       
#'       \code{"par.level"}     \tab \tab The level in the class hierarchy of
#'       the parent node of the local classifier. \cr
#'       
#'       \code{"par.name"}      \tab \tab The name of the parent node in the
#'       local classifier. \cr
#'       
#'       \code{"par.clas.iD"}   \tab \tab The \code{classifer.ID} in which the
#'       parent node was classified.  \cr
#'       
#'       \code{"num.sib.tot"}   \tab \tab The number of sibling nodes in the
#'       local classifier.  \cr
#'       
#'       \code{"num.sib.ter"}   \tab \tab The number of terminal sibling nodes
#'       in the local classifier.  \cr
#'       
#'       \code{"num.sib.int"}   \tab \tab The number of internal sibling nodes
#'       in the local classifier.  \cr
#'       
#'        }}
#'   \item{\code{$nodes.info} - information on each internal or terminal node in
#'   the class hierarchy, containing the following columns:}{} {\tabular{lll}{
#'   
#'       \code{"node.name"}        \tab \tab The name of the node. \cr
#'       
#'       \code{"node.level"}       \tab \tab  The level of the node in the class
#'       hierarchy. \cr
#'       
#'       \code{"node.freq"}        \tab \tab  The number of times the node
#'       appears in the training data. \cr
#'       
#'       \code{"node.par.lev"}     \tab \tab  The level in which the parent of
#'       the node resides. \cr
#'       
#'       \code{"node.par.name"}    \tab \tab The name of the node's parent node.
#'       \cr
#'       
#'       \code{"term.int.node"}    \tab \tab If the node is terminal or
#'       internal. \cr
#'       
#'       \code{"clas.yes.no"}      \tab \tab If the node is the parent node in a
#'       local classifier. \cr
#'       
#'       \code{"classifier.ID"}    \tab \tab If the node is the parent node in a
#'       local classifier, the name of the local classifer. \cr
#'       
#'       \code{"classified.in"}    \tab \tab The name of the local classifier in
#'       which the node is classified. \cr
#'       
#'       \code{"lev.above.clas.in"}\tab \tab The number of levels above the
#'       node's level in which it is classified. \cr     
#'        }}
#'        
#'   \item{\code{$unique.path} - information on all the unique paths from the
#'   tree root to each of the terminal nodes. Include two additional columns for
#'   \code{root.name} and \code{end.path.name}.}{}}}
#'   
#'   \item{train.data.ready}{Data frame containing the training data after 
#'   restructuring to the usable format of \code{RunHRF}}
#'   
#'   \item{case.ID}{Column 
#'   number in \code{train.data.ready} containing the case IDs} 
#'   \item{path.name}{Column number in \code{train.data.ready} containing the
#'   path names}
#'   \item{hie.levels}{Column number in \code{train.data.ready} containing the
#'   hierarchical information} 
#'   \item{exp.var}{Column number in \code{train.data.ready} containing the
#'   explanatory variables.}
#'   \item{all.local.RF}{The main large list with all the information on each
#'   local classifier. For each local classifier, the function returns a list 
#'   consisting of three objects:
#'   \tabular{lll}{
#'   \code{local.lRF.info}\tab \tab The information on the local randomForest as
#'   found in the \code{$hier.struc$lRF.info} data frame. \cr
#'   
#'   \code{local.data}    \tab \tab The Case ID's of all cases that were used as
#'   training data for the local randomForest. \cr
#'   
#'   \code{local.RF}      \tab \tab an object of class \code{ranomForest} for
#'   the local classifier. See package randomForest for details. \cr
#'   
#'   }}
#'   \item{order.local.RF}{data frame containing the order in which local
#'   randomForests are stored in the \code{all.local.RF} list.}
#'   
#'   \item{call}{the call to function \code{RunHRF}.}
#'   
#' @seealso 
#' \code{\link{predict.HRF}}    for extracting the proportion of votes, 
#' \code{\link{plot.HRF}}       for plotting the class structure,
#' \code{\link{ImportanceHie}}  for variable importance,
#' \code{\link{PerformanceHRF}} for assessing performance and accuracy,
#' \code{\link{PerformanceFlatRF}} for running an \code{"HRF"} object in a flat
#' classifier and assessing performance.
#' 
#' @references
#' Breiman, L. 2001. Random forests. \emph{Machine Learning} 45:5-32.
#'   
#' @examples
#' # create random HRF data
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data <- random.hRF$train.data
#'
#' # Run the Hierarchial randomForest
#' hie.RF.random <- RunHRF(train.data = train.data, 
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#'
#' # S3 method for plot -> the class hierarchy
#' plot(hie.RF.random) 
#'
#' # extracting information
#' lRF.info         <- hie.RF.random$hier.struc$lRF.info
#' nodes.info       <- hie.RF.random$hier.struc$nodes.info
#' unique.path      <- hie.RF.random$hier.struc$unique.path
#' train.data.ready <- hie.RF.random$train.data.ready
#' case.ID          <- hie.RF.random$case.ID
#' path.name        <- hie.RF.random$path.name
#' hie.levels       <- hie.RF.random$hie.levels
#' exp.var          <- hie.RF.random$exp.var
#' all.local.RF     <- hie.RF.random$all.local.RF
#' order.local.RF   <- hie.RF.random$order.local.RF
#' fun.call         <- hie.RF.random$call
#'
#' # extracting the info for local classifier C.2
#' c.2.local.classifer <- all.local.RF[[order.local.RF[
#'                          order.local.RF$classifier.ID == "C.2", 2]]]
#'
#' # structure for each local classifier
#' # info on the local classifier
#' c.2.local.lRF.info <- c.2.local.classifer$local.lRF.info 
#' # case.ID that were used to train the randomForest
#' c.2.local.case.ID  <- c.2.local.classifer$local.data 
#' # object of class randomForest    
#' c.2.local.RF       <- c.2.local.classifer$local.RF       
#' class(c.2.local.RF)
#' 
#' ################
#' # the OliveOIlHie dataset contains terminal nodes at levels 2 and 3
#' # RunHRF Will return an error if internal.end.path is not set to TRUE
#'
#' data(OliveOilHie)
#' # don't run - an error message is returned
#' 
#' # hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#' #                     case.ID           = "case.ID",
#' #                     hie.levels        = c(2:4),
#' #                     mtry              = "tuneRF2",
#' #                     internal.end.path = FALSE)
#' 
#' # no error message
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE)
#'
#' plot(x = hie.RF.OO, text.size = 9, split.text = 10)
#'   
#'   
#'   
#' @aliases randomForest
#' @export
#' @exportClass HRF
#' @import randomForest 
#'   
 




RunHRF = function(train.data, 
                  case.ID, 
                  hie.levels, 
                  internal.end.path = FALSE,       
                  end.path.name     = "END.PATH",  
                  root.include      = FALSE,       
                  root.name         = "TREE.ROOT", 
                  exp.var           = NA,                                    
                  mtry              = "tuneRF",   
                  ntree             = 500,         
                  importance        = TRUE,        
                  proximity         = TRUE,        
                  keep.forest       = TRUE,        
                  keep.inbag        = TRUE,        
                  ...)
{
  
  #require(randomForest)
  
  
  ################################################
  ### Section 1 - Check input + error messages ###
  ################################################
  #cat(paste("\n", mtry, "\n" , sep=""))
  
  cat(paste("\n" , "####################################" , "\n" , sep=""))
  cat(paste("\n" , "--> Performing initial checks..."     , "\n" , sep=""))
  
  #################
  # check Missing #
  #################
  
  if(missing(case.ID))
  {stop("\n No case.ID was specified in the train.data")}
  
  if(missing(hie.levels))
  {stop("\n No hie.levels were specified in the train.data")}
  
  if(missing(exp.var))
  {cat("Note! all columns other than those specified by case.ID and hie.levels will be used as input variables")}
  
  #################
  # Check case.ID #
  #################
  
  # Change from a character to numeric column number 
  
  if(is.character(case.ID))
  {case.ID.cha <- case.ID
   case.ID     <- match(case.ID.cha , names(train.data))}
  
  
  # checks if the column specified as case.ID contains a single entery for each ID
  CheckCaseID(train.data[ , case.ID])
  
  #################### 
  # Check hie.levels # 
  ####################
  
  # Change from a character to numeric column number 
  if(is.character(hie.levels)){hie.levels  <- match(hie.levels , names(train.data))}
  
  # checks if the columns specified in hie.levels contains factors
  CheckHieLevels(train.data[, hie.levels])
  
  #################### 
  # Check mtry       # 
  ####################
  if(mtry!="tuneRF" && mtry !="tuneRF2")
  {
    if(!is.numeric(mtry))
    {stop("\n if mtry is given, it should be a positive integer \n")} 
    
    if(mtry <= 0)
    {stop("\n if mtry is given, it should be a positive integer \n")}
    
    if(round(mtry , 0) !=mtry)
    {stop("\n if mtry is given, it should be a positive integer \n")}
  }
  
  ###################
  # Arrange exp.var #
  ###################
  
  if(is.character(exp.var)){exp.var  <- match(exp.var , names(train.data))}
  
  # when exp.var is not given by the user 
  if(length(exp.var) == 1){
    if(is.na(exp.var)){
      exp.var <- 1:length(names(train.data)) 
      exp.var <- exp.var[exp.var!=case.ID]         # Remove case.ID
      exp.var <- exp.var[!exp.var %in% hie.levels] # Remove hie.levels 
    }}
  
  
  ###############################
  # Check NA and missing values #
  ###############################
  
  work.data <- train.data[ , c(case.ID , hie.levels , exp.var)]  
 

  # Check NAs
 
  if(length(which(work.data[]=="" | work.data[]==" " | is.na(work.data)))!=0)
  {stop("\n At least one columns contains missing values or NA. \n Please remove missing values from  train.data columns of case.ID, hie.levels and exp.var")}

  rm(work.data)

  cat(paste("\n" , "--> Initial checks OK"                , "\n" , sep=""))
  cat(paste("\n" , "####################################" , "\n" , sep=""))
  
########################################################### 
### Section 2 - identify the structure of the hierarchy ###
###########################################################

  cat(paste("\n" , "--> Identifying hierarchical structure" , "\n" , sep=""))

  # create the unique.path data frame, to be used by the IdentifyHieStruc function
  unique.path <- train.data[ , hie.levels]
  
  # Run the IdentifyHieStruc function
  
  hier.struc <- IdentifyHieStruc(unique.path       = unique.path, 
                                 internal.end.path = internal.end.path, 
                                 end.path.name     = end.path.name,
                                 root.include      = root.include,
                                 root.name         = root.name)

# extract the local clasifers info and nodes info, as well as the updated unique.path
  lRF.info     <- hier.struc$lRF.info
  nodes.info   <- hier.struc$nodes.info
  unique.path  <- hier.struc$unique.path

cat(paste("\n" , "--> Hierarchical structure idenitfied" , "\n" , sep=""))
cat(paste("\n" , "####################################"  , "\n" , sep=""))

################################################################# 
### Section 3 - Arrange the train.data.ready table            ###         
#################################################################

cat(paste("\n" , "--> Restructuring the Training data" , "\n" , sep=""))

#  the structure of the train data table should be: case.ID, path.name, hie.levels, exp.var
#  THe hierarchy details should be as in unique.path, inlcuding the Root and End.Path levels
#  case.ID.2, path.name.2, hie.levels.2, exp.var.2 should refer to column number in the arranged train.data data frame

# Data for case.ID and exp.var
case.ID.data <- train.data[ , case.ID]
exp.var.data <- train.data[ , exp.var]

# Add path.name column to unique.path
path.name        <- c(1:dim(unique.path)[1])
unique.path.name <- cbind(path.name , unique.path)

# start the hie.levels.data
hie.levels.data       <- unique.path.name[1 , ]
hie.levels.data[1 , ] <- NA

if  (root.include){path.data <- unique.path.name[ , 2:(2 + length(hie.levels))]}
if (!root.include){path.data <- unique.path.name[ , 3:(2 + length(hie.levels))]}

#2:ncol(unique.path.name)


# a loop that runs on each row of train data
for (i in 1:dim(train.data)[1])
{
    # the Path of the corrent case
    train.path <- train.data[i , hie.levels]
    
    # a loop that finds the path in path.data
    for (j in 1:dim(unique.path)[1])
    {
      if(all(path.data[j , ] == train.path)) # if this is the correct path
      {hie.levels.data[i , ] <- unique.path.name[j , ]} # insert all the hierarchical information to the  row
    }
}   
   

# by Now the three main informations are ready - need to update the columns numbers

train.data.ready <- cbind(case.ID.data , hie.levels.data , exp.var.data)

colnames(train.data.ready)[1] <- names(train.data)[case.ID]

case.ID.2       <- 1 
path.name.2     <- 2 
hie.levels.2    <- c(3:(1 + dim(unique.path.name)[2]))
exp.var.2       <- c((max(hie.levels.2) + 1):dim(train.data.ready)[2])

path.name.data  <- hie.levels.data[ , 1]
hie.levels.data <- hie.levels.data[ , 2:dim(hie.levels.data)[2]]

# order  train.data.ready according to case.ID
train.data.ready <- train.data.ready[order(train.data.ready[ , 1]) , ]

# From now on - 
# 1. train.data.ready is the data frame to work with, 
# 2. case.ID.2, path.name.2, hie.levels.2 and exp.var.2 refer to column numbers in train.data.ready
# 3. case.ID.data, path.name.data, hie.levels.data and exp.var.data contains the relevant information from train.data.ready
# 



cat(paste("\n" , "--> Training data - Restructured"     , "\n" , sep=""))
cat(paste("\n" , "####################################" , "\n" , sep=""))

########################################################### 
### Section 4 - Run the local classifier loop           ###
###########################################################

cat(paste("\n" , "--> Starting RandomForest" , "\n" , sep=""))

# create the list to collect the info on each local classifer
all.local.RF <- vector("list" , dim(lRF.info)[1])

# The location in all.local.RF in which the data and model of each local classifer is stored
order.local.RF <- data.frame(classifier.ID = c(NA),
                             place.in.list = c(NA))


# a  loop that runs on each local classifers
for (i in 1:dim(lRF.info)[1])
{
 # use SubsetCasesLRF to subset the relevant input information for the local classifier
  local.data <- SubsetCasesLRF(local.class      = i,                  # Integer, specifying the row in lRF.info for which subset of train.data should be returned
                               lRF.info         = lRF.info,           # data frame, specifying each local classifer as a row- the output of IdentifyHieStruc
                               train.data.ready = train.data.ready,   # the data frame with the training data  
                               hie.levels       = hie.levels.2,       # columns in train.data.ready that contains the information on the hierarchical levels
                               exp.var          = exp.var.2)          # columns in train.data.ready that contains the iexplanatory variables
  
  order.local.RF[i , 1] <- lRF.info[i , 1]
  order.local.RF[i , 2] <- i
  
  y.RF <- local.data$local.train.cat   # the categories to be classifed
  x.RF <- local.data$local.train.vars  # the explanatory variables
  
  cat(paste("\n" , "####################################" , "\n" , sep=""))
  cat(paste("\n" , "Starting the RandomForest algorithm for local classifier: " ,lRF.info[i , 1] , "\n" , sep=""))
  
  
  # estimate mtry
  mtry.local <- mtry
  if(mtry=="tuneRF")
    {
    cat(paste("\n" , "Estimating mtry (tuneRF) for local classifer: " ,lRF.info[i , 1] , "\n" , sep=""))
    tune.RF.opt <- tuneRF(y = y.RF , x = x.RF, plot = FALSE)   # run the tuneRF function of RandomForest
    mtry.local  <- tune.RF.opt[match(x = min(tune.RF.opt[ , 2]) , table = tune.RF.opt[ , 2]) , 1] # identify the suggested mtry
    } 
  
  # estimate mtry
  if(mtry=="tuneRF2")
  {
    cat(paste("\n" , "Estimating mtry (tuneRF2) for local classifer: ",lRF.info[i , 1] , "\n" , sep=""))
    tune.RF.opt <- tuneRF2(y = y.RF,x = x.RF)   # run the tuneRF function of RandomForest
    mtry.local  <- tune.RF.opt[match(x = min(tune.RF.opt[ , 2]) , table = tune.RF.opt[ , 2]) , 1] # identify the suggested mtry
  } #else {mtry.local <- mtry}
  
  cat(paste("\n" , "mtry = " , mtry.local , "\n" , sep="" ))
 
  cat(paste("\n" , "Fitting the RandomForest algorithm" , "\n" , sep=""))
  
  # run the local RandomForest, using the arguemnt from RunHRF call
  local.RF <- randomForest(y            = y.RF,
                           x            = x.RF,
                           mtry         = mtry.local,
                           ntree        = ntree,
                           importance   = importance,
                           proximity    = proximity,
                           keep.forest  = keep.forest,
                           keep.inbag   = keep.inbag) 
  
  cat(paste("\n" , "Storing the local RandomForest object and additional data" , "\n" , sep=""))
  # assign the local.data and the local.RF to their slot in the all.local.RF list 
  all.local.RF[[i]] <- list(local.lRF.info =  local.data$local.class.info,       # the infromation on the local classifier
                            local.data     = local.data$local.train.case.ID,     # the Case Id's of the local training data
                            local.RF       = local.RF)                           # The local randomForest object
  
  cat(paste("\n" , "Ending the RandomForest algorithm for local classifer: " , lRF.info[i , 1] , "\n" , sep=""))
  cat(paste("\n" , "####################################", "\n" , sep=""))
}


cat(paste("\n" , "--> Ending RandomForest", "\n" , sep=""))
cat(paste("\n" , "####################################" , "\n" , sep=""))

#return(hier.struc)
hie.RF <- list(hier.struc       = hier.struc,
               train.data.ready = train.data.ready,
               case.ID          = case.ID.2,
               path.name        = path.name.2,
               hie.levels       = hie.levels.2,
               exp.var          = exp.var.2,
               all.local.RF     = all.local.RF,
               order.local.RF   = order.local.RF, # The location in all.local.RF in which the data and model of each local classifer is stored
               call             = match.call()                        # the call
               )
class(hie.RF) <- "HRF"
return(hie.RF)


} # end main function loop


