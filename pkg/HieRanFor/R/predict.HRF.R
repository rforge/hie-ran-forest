#' For all cases, the proportion of OOB votes that each class received in
#' each local randomForest classifier.
#' 
#' 
#' The function takes as input an object of class \code{"HRF"}. For each case in
#' the original training data or in \code{new.data}, the function extracts the 
#' proportion of OOB votes for each node (internal and terminal) in each local 
#' classifier.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param object              Object of class \code{"HRF"} - the output of
#'   \code{RunHRF}.
#'   
#' @param train.predict       Logical, if \code{TRUE}, returns for each case in
#'   the training data the proportion of OOB votes that each class received in
#'   each local randomForesst classifier. If \code{FALSE}, only the votes for
#'   \code{new.data} are returned.
#'   
#' @param new.data            Optional data frame containing additional cases 
#'   that were note a part of the original training set, for which the
#'   proportion of votes should be extracted.
#'   
#' @param new.data.case.ID    Integer, specifying the column number with the 
#'   \code{case.ID} in the \code{new.data} data frame. The \code{case.ID} values
#'   should be unique and different from those in the training data.
#'   
#' @param new.data.exp.var    Vector of integers, specifying the columns of 
#'   \code{new.data} that contains the same set of explanatory variables as used
#'   in the training of \code{hie.RF}. Before running the \code{RunHRF}, we
#'   recommend using the \code{link{JoinLevels}} function on each categorical
#'   variables to ensure the extraction of votes for \code{new.data}.
#'   
#' @param bind.train.new      Logical, if \code{TRUE} the cases in the training
#'   set and \code{new.data} will be returned as one output data frame (along
#'   with the two separate ones).
#'   
#' @param \dots   Optional parameters to be passed to the low level functions.
#' 
#' @details For the training data, only OOB votes are used in each local
#' classifier.\cr 
#' Inherited from \code{\link{randomForest}}, predictions for \code{new.data}
#' cannot be made if the \code{new.data} contains factor levels (both for
#' classes and for categorical explanatory variables) that were not represented
#' in the training data. Before running \code{RunHRF} we recommend either
#' sub-setting the training and new data from one general data frame or running
#' the \code{\link{JoinLevels}} function on each categorical variable.
#' 
#' @return  a list consisting of up to three of the following data frames:
#' \item{prop.vote.train}{The proportion of OOB votes that each case from the
#'   training dataset received in each local classifier for each class.}
#' \item{prop.vote.new}{The proportion of OOB votes that each case from the
#'   \code{new.data} dataset received in each local classifier for each class.}
#' \item{prop.vote.full}{Bind of \code{prop.vote.train} and \code{prop.vote.new}
#' if \code{bind.train.new} is \code{TRUE}.}
#' 
#' @examples
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data <- random.hRF$train.data
#' new.data   <- random.hRF$new.data
#'
#' # run HRF
#' hie.RF.random <- RunHRF(train.data = train.data,
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#'
#' # predict only for the training data
#' Predict.HRF.train    <- predict(hie.RF.random)
#' prop.votes.lRF.train <- Predict.HRF.train$prop.vote.train
#'
#' # predict only for new.data
#' Predict.HRF.new <- predict(object           = hie.RF.random,
#'                            train.predict    = FALSE,                
#'                            new.data         = new.data,                
#'                            new.data.case.ID = 1,                   
#'                            new.data.exp.var = c(2:ncol(new.data)),  
#'                            bind.train.new   = FALSE)
#' prop.votes.lRF.new <- Predict.HRF.new$prop.vote.new
#'
#' # predict for training and new data + bind
#'
#' Predict.HRF.both <- predict(object = hie.RF.random,                                 
#'                             train.predict    = TRUE,                
#'                             new.data         = new.data,                
#'                             new.data.case.ID = 1,                   
#'                             new.data.exp.var = c(2:ncol(new.data)),  
#'                             bind.train.new   = TRUE)
#' attributes(Predict.HRF.both)
#' prop.votes.lRF.both <- Predict.HRF.both$prop.vote.full
#' 
#' # the prop.votes.lRF.both data frame contains 
#' # one additional column: 'train.or.test'
#' # cases from the training data sat are listed as train
#' # cases from the new.data data set are listed as test
#' names(prop.votes.lRF.both)[1]
#' levels(prop.votes.lRF.both$train.or.test)
#' 
#' @seealso 
#' \code{\link{RunHRF}} for running a hierarchical randomForest analysis, 
#' \code{\link{GetMultPropVotes}} for estimating the multiplicative proportion
#' of votes from the output of \code{predict.HRF}, \code{\link{PerformanceHRF}}
#' for assessing performance and accuracy, \code{\link{PredictNewHRF}} for
#' predicting crisp class for each case of \code{new.data}.
#' 
#' @method predict HRF
#' 
#' @aliases predict
#' 
#' @export 
#' @import randomForest



# returns the porportion of OOB votes that each node recieved in each local
# classifer if train.predict=TRUE, runs on all the training data, if
# is.null(new.data)==FALSE, runs also on the new.data (sould be in similar form
# as the train.data) throughout, case.ID is used to avoid mixing different cases
# For the training data and any local classifier: The OOB votes are used for
# cases that are a real child of the parent node the ouput of
# predict.randomForest is used for cases that are not 'real' childs of the
# parent node


predict.HRF = function(object,                                 
                       train.predict    = TRUE,                
                       new.data         = NULL,                
                       new.data.case.ID = 1,                   
                       new.data.exp.var = c(2:ncol(new.data)),  # vector of integers, specifying the columns of new.data that contains the same set of explanatory variables as used in the training of the hierarchical Random Forest. Note about the levels
                       bind.train.new   = FALSE,                # logical, If is.null(new.data)==FALSE, should the prediction of the training
                       ...)
{ # start function
  
  #require(randomForest)
  hie.RF <- object
  # check that there is some data to work on
  if(is.null(new.data) && !train.predict)
  {stop(paste("\n",
              "Error in function: Predict.HRF", 
              "\n", 
              "No data to produce proportion of votes,",
              "\n",
              "Please set train.predict to TRUE or provide new.data", sep=""))}
  
  # extract the relevant information from hie.RF
  lRF.info         <- hie.RF$hier.struc$lRF.info      # the info data frame on each local classifer
  nodes.info       <- hie.RF$hier.struc$nodes.info    # the info data frame on each node
  unique.path      <- hie.RF$hier.struc$unique.path   # all the pathes from tree root to terminla nodes
  train.data.ready <- hie.RF$train.data.ready         # the re-arranged train.data.ready, as output of RunHRF
  case.ID          <- hie.RF$case.ID                  # The column in train.data.ready that contains the case.ID
  exp.var          <- hie.RF$exp.var                  # The column in train.data.ready that contains the Explanatory variables
  all.local.RF     <- hie.RF$all.local.RF             # list containing all the local random forests. For each random forest, there is a list with two lists: the local.data and the local.RF
  
  
  ###################################
  ## Deal with train.predict==TRUE ##
  ###################################
  
  if (train.predict)
  { # start the if (train.predict) condition
    
    cat(paste("\n", "     --> Start votes extraction for the Training data", "\n", sep=""))
    
  prop.vote.train              <- as.data.frame(train.data.ready[, case.ID])
  colnames(prop.vote.train)[1] <- colnames(train.data.ready)[case.ID]
  
  # make sure the cases are ordered according to case.ID
  prop.vote.train <- prop.vote.train[order(prop.vote.train[, 1]), ]
  
  for (i in 1:dim(lRF.info)[1]) # loop that runs on each local classifier
  { # start the 'i' loop
    
    cat(paste("\n", "Start votes extraction (training data) for local classifer= ", lRF.info[i, 1], "\n", sep=""))
    
    
    # extract data for the local classifer
    local.RF.obj           <- all.local.RF[[i]]                       # a list with local.data and local.RF
    local.train.case.ID    <- as.data.frame(local.RF.obj$local.data)  # the case.ID of the local.train.data
    local.RF               <- local.RF.obj$local.RF                   # object of class RandomForest for local classifer i
    
   
    local.votes                      <- as.data.frame(local.RF$votes)              # the porportion of OOB votes for each category of each case in the local.RF
    local.case.ID.votes              <- cbind(local.train.case.ID, local.votes)    # binding the case.ID with the votes
    colnames(local.case.ID.votes)[1] <- colnames(train.data.ready)[case.ID]        # arranging the colomn name for case.ID
    
    # three options  
    # 1 - all the cells of train.data.ready are found in the local training data 
    # 2 - not all the cells of train.data.ready are found in the local training data 
    # 3 - an error message when there are more cases in the local Train data than in train.data.ready
    
    ##########################################################
    
    # Option 1: all the cells of train.data.ready are found in the local training data 
    if(dim(local.case.ID.votes)[1] == dim(train.data.ready)[1]) # all the cases are found in the train data of the local classifer
    { # start option 1
      
      local.case.ID.votes <- local.case.ID.votes[order(local.case.ID.votes[, 1]), ] # arange order according to case.ID
      local.case.ID.votes <- local.case.ID.votes[, 2:dim(local.case.ID.votes)[2]]   # Remove the case.ID column
      prop.vote.train     <- cbind(prop.vote.train, local.case.ID.votes)            # Add the votes to Data Collect 
      
      # remove all the local data to ensure next i start from clear
      rm(list = c("local.RF.obj",
                  "local.RF",
                  "local.train.case.ID",
                  "local.votes"))
    } # End option 1
    
    ##########################################################
    
    # option 2: not all the cells of train.data.ready are found in the local training data
    if(dim(local.case.ID.votes)[1]<dim(train.data.ready)[1]) # some cases are found in train.data.ready but not in the local data
    { # Start option 2
      
      # subset from train.data.ready all the cases that are not in the local.train.data
      not.local.train  <- subset(train.data.ready, !(prop.vote.train[, 1] %in% local.case.ID.votes[, 1]))
      
      
      # Run the not.local.train cases down the local classifer using predict.randomForest #
      predict.local.RF <- predict(object     = local.RF,
                                  newdata    = not.local.train[, exp.var],  # the explanatory variables for the local train data
                                  type       = "vote",
                                  norm.votes = TRUE)
      
      # extract the case.IDs for the not.local.train and cbind with the votes
      not.local.case.ID                    <- as.data.frame(not.local.train[, case.ID])
      not.local.case.ID.votes              <- cbind(not.local.case.ID, predict.local.RF)
      colnames(not.local.case.ID.votes)[1] <- colnames(train.data.ready)[case.ID]
      
      # bind the votes for the local train data and not local train data
      all.votes       <- rbind(local.case.ID.votes, not.local.case.ID.votes)
      all.votes       <- all.votes[order(all.votes[, 1]), ]   # arange order according to case.ID
      all.votes       <- all.votes[, 2:dim(all.votes)[2]]     # Remove the case.ID column  
      prop.vote.train <- cbind(prop.vote.train, all.votes)    # Add the votes to Data Collect 
      
      # remove all the local data to ensure next i start from clear
      rm(list = c("local.RF.obj",
                  "local.RF",
                  "local.train.case.ID",
                  "local.votes",
                  "not.local.train",
                  "predict.local.RF",
                  "not.local.case.ID",
                  "not.local.case.ID.votes",
                  "all.votes"))
    } # End option 2
    
    ########################################################## 
    
    # option 3: an error message when there are more cases in the local TRain data than in train.data.ready
    if(dim(local.case.ID.votes)[1] > dim(train.data.ready)[1]) # some cases are found in train.data.ready but not in the local.data
    { # Start option 3
      stop(paste("\n", "function: GetMultPropVotes", 
                 "\n", 
                 "In local classifier: ",
                 lRF.info[i, 1],
                 "\n",
                 " There are less cases in the overall training data than in the local training data",
                 "\n",
                 sep=""))
    } # End option 3
    rm(local.case.ID.votes)
    cat(paste("end votes extraction (training data) for local classifer= ", lRF.info[i, 1], "\n", sep=""))
  } # end the 'i' loop
  
  
  
  ################################################################################
  ### add nodes with no siblings - their parents are internal with frequancy 1 ###
  ### add also a node with tree.root and end.path.name                         ###
  ### Assign these node a proportion of votes=1                                ###
  ### cbind with prop.vote.train                                                ###
  ################################################################################
  
  
  
  nodes.as.1   <- subset(nodes.info, nodes.info$lev.above.clas.in != 1)
  nodes.as.1   <- subset(nodes.as.1 , !is.na(nodes.as.1$lev.above.clas.in))
  nodes.add    <- as.data.frame(setNames(replicate(nrow(nodes.as.1), numeric(0), simplify = F), 
                                         nodes.as.1$node.name))
  
  nodes.add[1:nrow(train.data.ready), ]                  <- 1
  nodes.add[, levels(unique.path[1, 1])]                 <- 1
  nodes.add[, levels(unique.path[1, ncol(unique.path)])] <- 1
  
  
  prop.vote.train              <- cbind(prop.vote.train,nodes.add)
  colnames(prop.vote.train)[1] <- colnames(train.data.ready)[case.ID]
  
  # add the train.or.test column
  train.cat                                           <- data.frame(train.or.test="train")
  train.cat[1:nrow(prop.vote.train), "train.or.test"] <- "train"
  prop.vote.train                                     <- cbind(train.cat,prop.vote.train)
  
  # remove some objects
  rm(list = c("nodes.as.1",
              "nodes.add",
              "train.cat"))
  
  cat(paste("\n", "     --> End votes extraction for the Training data", "\n", sep=""))
  
  } # end the if(train.predict) condition
  
  
  if(!is.null(new.data)) # there is an input of new.data
  { # start the if(!is.null(new.data)) condition
   
    cat(paste("\n", "     --> Start votes extraction for new.data", "\n", sep=""))
    # order new.data according to case.ID 
    new.data    <- new.data[order(new.data[, new.data.case.ID]), ] 
    
    # start the data frame to collect the votes
    prop.vote.new              <- as.data.frame(new.data[, new.data.case.ID])             
    colnames(prop.vote.new)[1] <- colnames(train.data.ready)[case.ID]
    
    for (i in 1:dim(lRF.info)[1]) # loop that runs on each local classifier
    { # start the 'i' loop
      
      cat(paste("\n", "Start votes extraction (new.data) for local classifer= ", lRF.info[i, 1], sep=""))
      
      local.RF.obj  <- all.local.RF[[i]]        # a list with local.data and local.RF
      local.RF      <- local.RF.obj$local.RF    # object of class RandomForest for local classifer i
      
      predict.local.RF <- predict(object     = local.RF,
                                  newdata    = new.data[,new.data.exp.var], # the explanatory variables for the local train data
                                  type       = "vote",
                                  norm.votes = TRUE)
     
     prop.vote.new <- cbind(prop.vote.new, predict.local.RF)
     cat(paste("\n", "End votes extraction (new.data) for local classifer= ", lRF.info[i, 1], "\n", sep=""))
    } # end the 'i' loop
    
    
    nodes.as.1   <- subset(nodes.info, nodes.info$lev.above.clas.in != 1)
    nodes.as.1   <- subset(nodes.as.1, !is.na(nodes.as.1$lev.above.clas.in))
    nodes.add    <- as.data.frame(setNames(replicate(nrow(nodes.as.1), numeric(0), simplify = F), 
                                           nodes.as.1$node.name))
    
    nodes.add[1:nrow(new.data), ]                          <- 1
    nodes.add[, levels(unique.path[1, 1])]                 <- 1
    nodes.add[, levels(unique.path[1, ncol(unique.path)])] <- 1
    
    
    prop.vote.new              <- cbind(prop.vote.new, nodes.add)
    colnames(prop.vote.new)[1] <- colnames(train.data.ready)[case.ID]
    
    # add the train.or.test column
    train.cat.new                                          <- data.frame(train.or.test = "test")
    train.cat.new[1:nrow(prop.vote.new), "train.or.test"]  <- "test"
    prop.vote.new                                          <- cbind(train.cat.new, prop.vote.new)
    
    cat(paste("\n", "     --> End votes extraction for new.data", "\n", sep=""))
    
  } # end the if(!is.null(new.data)) condition
  
  if(is.null(new.data))
  {return.list <- list(prop.vote.train = prop.vote.train)}
  
  if(!train.predict)
  {return.list <- list(prop.vote.new = prop.vote.new)}
  
  
  if(!bind.train.new && train.predict && !is.null(new.data))
  {return.list <- list(prop.vote.train = prop.vote.train,
                       prop.vote.new   = prop.vote.new)}
  
  if(bind.train.new && train.predict && !is.null(new.data))
  { 
    prop.vote.full <- rbind(prop.vote.train, prop.vote.new)
    return.list    <- list(prop.vote.full  = prop.vote.full,
                           prop.vote.train = prop.vote.train,
                           prop.vote.new   = prop.vote.new)
  }
  
  return.list
} # end function