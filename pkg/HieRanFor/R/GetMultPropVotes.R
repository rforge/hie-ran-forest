#' For each case, the multiplicative proportion of votes
#' 
#' This function takes as input the proportion of votes that each case received 
#' in each local classifier (See: \code{\link{predict.HRF}}) and returns the
#' multiplication of votes, along each path down the class hierarchy.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#'   
#' @param prop.vote      Data frame, the proportion of votes that each case
#'   received in each local classifier. The output of \code{\link{predict.HRF}}.
#'   
#' @param unique.path    Data frame, one of the output data frames of 
#'   \code{RunHRF}. Contains information on each path from the \code{tree.root}
#'   to each of the terminal nodes.
#'   
#' @param all.levels     Logical, if \code{TRUE}, a data frame with the
#'   predicted probabilities is returned for each level of the class hierarchy.
#'   If \code{FALSE} (default), multiplicative proportion of votes are evaluated
#'   only for the entire class hierarchy.
#'   
#' @param \dots     Optional parameters to be passed to low level functions.
#' 
#'   
#' 
#' @details The function prints an error message for cases whose sum over all
#'   terminal nodes for a given hierarchy level is different from 1 (up to 10
#'   digits accuracy). Setting \code{all.levels = TRUE} enables exploration of
#'   proportion of votes, crisp classes and accuracy at specific tree depth.
#'   
#'  @return The function returns a list with a single data frame if
#'    \code{all.levels}=\code{FALSE} or a list with length equalling the class
#'    hierarchy tree depth if \code{all.levels}=\code{TRUE}. 
#'    \describe{Each object within the list contains a data frame with the
#'    following columns:
#'    
#'      {\tabular{lll}{
#'       \code{"train.or.test"} \tab \tab whether the case was from the training
#'       dataset or from \code{new.data}. \cr
#'       
#'       \code{"case.ID"}       \tab \tab The case.ID of the case. \cr
#'       
#'       \code{other columns}     \tab \tab a column for each terminal node for
#'       the given level, containing the multiplicative proportion of votes.
#'       Values over all nodes should sum to 1. \cr }}}
#'       
#' @examples
#' set.seed(354)
#' # create a random training dataset
#' random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data <- random.hRF$train.data
#' # run HRF and predict
#' hie.RF.random <- RunHRF(train.data = train.data, 
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)),
#'                         mtry="tuneRF2")
#' prop.votes.lRF.train <- predict(hie.RF.random)$prop.vote.train
#' 
#' # multiply path only until the deepest level of the class hierarchy
#' multi.prop.votes.full <- GetMultPropVotes(
#'                          prop.vote   = prop.votes.lRF.train, 
#'                          unique.path = hie.RF.random$hier.struc$unique.path,  
#'                          all.levels  = FALSE)
#' multi.prop.votes.L4 <- multi.prop.votes.full[[1]]
#' 
#' #######
#' data(OliveOilHie)
#'
#' hie.RF.OO <- RunHRF(train.data = OliveOilHie, 
#'                     case.ID    = "case.ID", 
#'                     hie.levels = c(2:4),
#'                     internal.end.path = TRUE,
#'                     mtry= "tuneRF2")
#' 
#' prop.votes.OO <- predict(hie.RF.OO)$prop.vote.train
#' mult.prop.OO  <- GetMultPropVotes(prop.vote   = prop.votes.OO,            
#'                            unique.path = hie.RF.OO$hier.struc$unique.path,      
#'                            all.levels  = TRUE)
#' plot(hie.RF.OO)
#' multi.prop.votes.L1 <-mult.prop.OO[["prop.multiplicative.votes.L1"]]
#' names(multi.prop.votes.L1)[3:ncol(multi.prop.votes.L1)]
#' 
#' multi.prop.votes.L2 <- mult.prop.OO[["prop.multiplicative.votes.L2"]]
#' names(multi.prop.votes.L2)[3:ncol(multi.prop.votes.L2)]
#' 
#' # note that terminal nodes from level 2 appears in L3 as well. 
#' multi.prop.votes.L3 <- mult.prop.OO[["prop.multiplicative.votes.L3"]]
#' names(multi.prop.votes.L3)[3:ncol(multi.prop.votes.L3)]
#' 
#' @seealso 
#' \code{\link{RunHRF}} for running HRF, 
#' \code{\link{predict.HRF}} for predicting the proportion of votes, 
#' \code{\link{PerformanceHRF}} for various performance measures.
#'  
#' @export
#' 






# returns for each case the multiplicative proportion of votes down the hierarchical tree. Can also be used
# to return the multiplicative proportion of votes up to a certain level of the hierarhcy
# requires as input the proportion of votes that each case received in each local classifer -the output of predict.HRF
GetMultPropVotes = function(prop.vote,           # data frame, the proportion of votes that each case received for each node in each local classifier, One of the output data frames of predict.HRF
                            unique.path,         # data frame, the unique.path data frame from RunHRF
                            all.levels = FALSE,  # logical, if TRUE, a data frame with the predicted probabilities is returned for each level, if FALSE, only for the deepest level.
                            ...)
{ # Start function
  
  # start the multiplicative.prop.full list that collects the proprtion of votes for each level 
  if(all.levels)
  {multiplicative.prop.full       <- vector("list", (ncol(unique.path) - 2))
  names(multiplicative.prop.full) <- paste("prop.multiplicative.votes", colnames(unique.path)[2:(ncol(unique.path) - 1)], sep=".")
  start.col <- 1
  }
  
  if(!all.levels)
  {multiplicative.prop.full        <- vector("list", 1)
   names(multiplicative.prop.full) <- paste ("prop.multiplicative.votes", colnames(unique.path)[(ncol(unique.path) - 1)], sep=".")
   start.col <- ncol(unique.path) - 2
  }
  
  
  for (count.levels in start.col:(ncol(unique.path) - 2)) # loops that runs on each level
  { # start the count.levels loop
    
    # Get the terminal node for each path
  cat(paste("\n Estimating multiplicative proportion of votes until level: ", colnames(unique.path)[1 + count.levels], "\n", sep="" ))

  unique.path.term  <- GetTerminalNode(end.path.name = unique.path[1, ncol(unique.path)],
                                       unique.path   = unique.path,
                                       level.depth   = count.levels)
  
  
  # create the multiplicative.prop data frame
  
  all.cases.multiplicative.prop                      <- as.data.frame(setNames(replicate(nrow(unique.path.term), 
                                                                                         numeric(0), 
                                                                                         simplify = F), 
                                                                               unique.path.term$term.node.name))
  all.cases.multiplicative.prop[1:nrow(prop.vote), ] <- NA
  all.cases.multiplicative.prop                      <- cbind(prop.vote[, c(1, 2)],
                                                              all.cases.multiplicative.prop)
  
  # start a loop that runs on each case
  for (count.case in 1:nrow(all.cases.multiplicative.prop))
  { # start the count.case loop
    
    # get the Multiplicative probabilities
    case.prob <-  GetCaseMultiProb(case.props       = prop.vote[count.case, ],
                                   unique.path.term = unique.path.term)
   
  if(round(sum(case.prob$multiplicative.prob), 10) != 1) # check that the sum is correct
  { # start the if round statment
    cat(paste("\n", "Error in Function: GetCaseMultiProb", "\n", 
              "For case.ID= ", prop.vote[count.case, 2], " in depth level= ", 
              count.levels, " the sum over all terminal nodes differs from 1 (10 digits accuracy)",  "\n", sep=""))
  } # end the if round statment
   
    for (count.term in 1:nrow(unique.path.term)) # loop that runs on all terminal nodes for the case and level
    {
      focal.term.col                                            <- match(case.prob$term.node.name[count.term], 
                                                                        colnames(all.cases.multiplicative.prop)[])  # th column of the focal terminal node in all.cases.multiplicative.prop
      all.cases.multiplicative.prop[count.case, focal.term.col] <- case.prob$multiplicative.prob[count.term]
    }
  
  
  } # end the count.case loop
  
  
  if(all.levels){
  multiplicative.prop.full[[count.levels]] <- all.cases.multiplicative.prop}
  
  if(!all.levels){
    multiplicative.prop.full[[1]] <- all.cases.multiplicative.prop}
  
  rm(list=c("all.cases.multiplicative.prop",
            "unique.path.term",
            "case.prob",
            "focal.term.col"))
  

  
  
  } # end the count.levels loop
  
  if(all.levels){
  cat(paste("\n", "Multiplicative votes for level:", colnames(unique.path)[2:(ncol(unique.path) - 1)], " are found in:", names(multiplicative.prop.full), sep=" "))
  cat(paste("\n", "##--##--##--##", "\n", sep=""))}
  
  if(!all.levels){
  cat(paste("\n", "Multiplicative votes for level:", colnames(unique.path)[(ncol(unique.path) - 1)], " are found in:", names(multiplicative.prop.full), sep=" "))
  cat(paste("\n", "##--##--##--##", "\n", sep=""))}
  
  
  multiplicative.prop.full
} # End function




