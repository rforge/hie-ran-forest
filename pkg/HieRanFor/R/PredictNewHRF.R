#' Predict crisp class for \code{new.data}
#' 
#' This function takes as input a \code{new.data} data frame with the same 
#' explanatory variables as those used in \code{hie.RF}. Next, the 
#' \code{\link{predict.HRF}} function is applied to extract the proportion of 
#' votes that each case in \code{new.data} received for each node in each local 
#' classifier. Finally, up to three methods of selecting a single terminal node 
#' (as given by \code{crisp.rule} and described in \code{\link{PerformanceHRF}})
#' are applied for each case.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#'  
#' @param hie.RF              Object of class \code{"HRF"} - the output of
#'   \code{RunHRF}.
#'   
#' @param new.data            Data frame containing additional cases that were
#'   note a part of the original training set.
#'   
#' @param new.data.case.ID    Integer, specifying the column number with the 
#'   \code{case.ID} in the \code{new.data} data frame. The \code{case.ID} values
#'   should be unique and different from those in the training data.
#'   
#' @param new.data.exp.var    Vector of integers, specifying the columns of 
#'   \code{new.data} that contains the same set of explanatory variables as used
#'   in the training of \code{hie.RF}.
#'   
#' @param crisp.rule          The method of selecting a single crisp class from
#'   the proportion of votes. See details in \code{\link{PerformanceHRF}}.
#'   
#' @param perm.num            Integer, number of random permutations for each
#'   case if \code{'multiplicative.permutation'} is applied.
#'   
#' @param div.logical         Logical, if \code{TRUE} progress when 
#'   \code{'multiplicative.permutation'} is applied will be printed every
#'   \code{div.print} permutations.
#'   
#' @param div.print            See above.
#' 
#' @param \dots  Optional parameters to be passed to low level functions. 
#' 
#' @details If the observed class of \code{new.data} are known, the function 
#'   \code{\link{PerformanceNewHRF}} will perform all the steps in this function
#'   and will add the estimation of performance measures.\cr Inherited from
#'   \code{\link{randomForest}}, predictions for \code{new.data} cannot be made
#'   if the \code{new.data} contains factor levels (both for classes and for 
#'   categorical explanatory variables) that were not represented in the 
#'   training data. Before running \code{RunHRF} we recommend either sub-setting
#'   the training and new data from one general data frame or running the 
#'   \code{\link{JoinLevels}} function on each categorical variable.
#'  
#' @return A list with the following components:
#'    {\tabular{lll}{
#'      \code{"raw.vote"}              \tab \tab Data frame containing for each
#'      case, the proportion of votes for each node in each local classifier
#'      (the output of \code{\link{predict.HRF}}). \cr
#'      
#'      \code{"crisp.case.class"}      \tab \tab Data frame containing the crisp
#'      class for each case based on all options defined by
#'      \code{crisp.rule}.\cr
#'      
#'      \code{"multiplicative.prop"}   \tab \tab Optional data frame with the
#'      multiplicative proportion of votes (the output of
#'      \code{\link{GetMultPropVotes}}). Will be returned if
#'      \code{"multiplicative.majority"} or \code{"multiplicative.permutation"}
#'      were selected.\cr
#'      
#'      \code{"call"}                  \tab \tab The call to function
#'      \code{PredictNewHRF}. \cr
#'      
#'    }}  
#' 
#' @examples
#' # create a random HRF dataset and RunHRF
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data <- random.hRF$train.data
#' new.data   <- random.hRF$new.data
#' hie.RF.random <- RunHRF(train.data = train.data, 
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#' 
#' # predict for new.data
#' pred.new.hRF <- PredictNewHRF(hie.RF     = hie.RF.random,
#'                               new.data   = new.data,
#'                               crisp.rule = c("stepwise.majority",
#'                                              "multiplicative.majority" , 
#'                                              "multiplicative.permutation"),
#'                               perm.num   = 10,
#'                               div.print  = 2)
#' 
#' # extract values
#' names(pred.new.hRF)
#' pred.new.votes       <- pred.new.hRF$raw.votes
#' pred.new.mult.prop   <- pred.new.hRF$multiplicative.prop
#' pred.new.crisp.class <- pred.new.hRF$crisp.case.class
#' pred.new.call        <- pred.new.hRF$call
#'            
#' @seealso
#' \code{\link{RunHRF}} for running a hierarchical randomForest analysis, 
#' \code{\link{PerformanceHRF}} for performance analysis, 
#' \code{\link{PerformanceNewHRF}} for performance analysis on \code{new.data}
#' with observed terminal node,
#' 
#' @export
#' 


# Predict new data according to an existing HRF object

PredictNewHRF = function(hie.RF,                                  # object of class HRF - the output of RunHRF
                         new.data,                                #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                         new.data.case.ID = 1,                    # Integer, specifying the column number that contains the case.ID in the new.data data frame
                         new.data.exp.var = c(2:ncol(new.data)),  # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the hie.RF
                         crisp.rule       = c("multiplicative.majority" , "multiplicative.permutation" , "stepwise.majority"),
                         perm.num         = 500,
                         div.logical      = TRUE,                      # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                         div.print        = 25,
                         ...
                         )
  
{
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  #require(randomForest)
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call", "\n", sep=""))
  
  #####
  # check class of hie.RF
  if(class(hie.RF)!="HRF")
  {stop(paste("\n", "In PredictNewHRF:  hie.RF should be of class HRF", "\n", sep=""))}
  
  #####
  # check perm.num
  if(!is.numeric(perm.num))
  {stop(paste("\n", "In PredictNewHRF, perm.num should be a positive integer", "\n", sep=""))}
  
  if(perm.num<1)
  {stop(paste("\n","In PredictNewHRF, perm.num should be a positive integer", "\n", sep=""))}
  
  if(round(perm.num,0)!=perm.num)
  {stop(paste("\n", "In PredictNewHRF, perm.num should be a positive integer", "\n", sep=""))}
  
  #####
  # Check div.logical
  if(!is.logical(div.logical))
  {cat(paste("\n", "In PredictNewHRF, div.logical should be Logical. default of div.logical=TRUE is used", "\n", sep=""))
   div.logical <- TRUE}
  
  #####
  # check div.print
  if(!is.numeric(div.print))
  {cat(paste("\n", "In PredictNewHRF, div.print should be a positive integer", "\n" ,
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  if(div.print<1)
  {cat(paste("\n", "In PredictNewHRF, div.print should be a positive integer", "\n" ,
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  if(round(div.print,0) != div.print)
  {cat(paste("\n", "In PredictNewHRF, div.print should be a positive integer", "\n" ,
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  #####
  # check crisp.rule
  
  temp.vec <-c("multiplicative.majority", "multiplicative.permutation", "stepwise.majority") 
  
  if(length(intersect(crisp.rule,temp.vec)) < 1)
  {stop(paste("\n", "In PredictNewHRF, at least one valid option for crisp.rule is required", "\n", sep=""))}
  
  if(length(intersect(crisp.rule,temp.vec)) > 0)
  {cat(paste("\n", "Estimating crisp classification based on: ", crisp.rule, "\n", sep=""))}
  
  
  ###############################
  # Check new.data              #
  ###############################
  
  if(missing(new.data))
  {stop("\n No new.data was specified")}
  
 
  #####
  # Check NA and missing values #
   
  work.data <- new.data[, c(new.data.case.ID, new.data.exp.var)]  
  
  if(length(which(work.data[] == "" | work.data[] == " " | is.na(work.data))) != 0)
  {stop("\n At least one columns contains missing values or NA. \n Please remove missing values from  new.data columns of new.data.case.ID, hie.levels and new.data.exp.var")}
  
  rm(work.data)
  
  #####
  # check if the same explanatory variables are called 
  
  CheckNewExpVar(hie.RF           = hie.RF,
                 new.data         = new.data,
                 new.data.exp.var = new.data.exp.var)
  
  #####
  # Check if the new.data.case.ID are different from those used in the hie.RF
  CheckNewID(hie.RF = hie.RF,
             new.ID = new.data[, new.data.case.ID])
  
  #####
  # checks if the column specified as case.ID contains a single entery for each ID
  CheckCaseID(new.data[, new.data.case.ID])
  
  cat(paste("\n", "-->  Call Verified", "\n", sep=""))
  
  ######################################################################################
  ### STEP 2 - CREATE LOGICAL ARGUMENTS FOR EACH OPTION OF crisp.rule                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # Multiplicative Majority rule
  if(!is.na(match("multiplicative.majority", crisp.rule))) # the multiplicative.majority rule was called by the user
  { mult.maj.rule.logical <- TRUE  }
  if(is.na(match("multiplicative.majority", crisp.rule))) # the multiplicative.majority rule was not called by the user
  { mult.maj.rule.logical <- FALSE } 
  
  
  # multiplicative.permutation
  if(!is.na(match("multiplicative.permutation", crisp.rule))) # the multiplicative.permutation was called by the user
  { mult.perm.logical     <- TRUE  } 
  if(is.na(match("multiplicative.permutation", crisp.rule))) # the multiplicative.permutation was not called by the user
  { mult.perm.logical     <- FALSE }  
  
  # stepwise.majority
  if(!is.na(match("stepwise.majority", crisp.rule))) # the stepwise.majority was called by the user
  { step.maj.rule.logical <- TRUE  } 
  if(is.na(match("stepwise.majority", crisp.rule))) # the stepwise.majority was not called by the user
  { step.maj.rule.logical <- FALSE }
  
  ######################################################################################
  ### STEP 3 - Extract the raw votes                                                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n","-->  Preparing the predicted terminal nodes from the hierarchical RandomForest ","\n",sep=""))
  
  votes.new <- predict.HRF(object           = hie.RF,            # object of class HRF - the output of RunHRF
                           train.predict    = FALSE,             # logical, if true, the OOB votes that each case received for each local classifier are returned
                           new.data         = new.data,          # Optional data frame containing additional cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                           new.data.case.ID = new.data.case.ID,  # Integer, specifying the column number that contains the case.ID in the new.data data frame
                           new.data.exp.var = new.data.exp.var,  # vector of integers, specifying the columns of new.data that contains the same set of explanatory variables as used in the training of the hierarchical Random Forest. Note about the levels
                           bind.train.new   = FALSE)
  
  focal.votes <- votes.new$prop.vote.new
  
  ######################################################################################
  ### STEP 4 - estimate crisp classifcation                                          ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # the data frame that collects all the crisp rules
  crisp.case.class <- focal.votes[, c(1, 2)] 
  unique.path      <- hie.RF$hier.struc$unique.path
  
  # Treat the multiplicative majority and/or permutations
  if(mult.maj.rule.logical || mult.perm.logical)
  { # start the crisp rules that relay on multiplicative probabilites
    cat(paste("\n", "Estimating the Multiplicative probabilities down the hierarchical tree ", "\n", sep=""))
    
    
    multiplicative.prop.last.level <- GetMultPropVotes(prop.vote   = focal.votes,
                                                       unique.path = unique.path,
                                                       all.levels  = FALSE)
    
    multiplicative.prop <- multiplicative.prop.last.level[[1]] # the deepest level of the hierarchical nodes tree
    
    # if the user called the multiplicative majority rule
    if(mult.maj.rule.logical)
    {
      cat(paste("\n", "Adding the multiplicative majority Rule", "\n", sep=""))
      mult.maj.rule <- GetMultMajRule(prop.multiplicative.votes = multiplicative.prop,     # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is train.or.test and the second is case.ID
                                      bind.prop.mul.maj         = FALSE)
      
      crisp.case.class <- cbind(crisp.case.class, mult.maj.rule[3])
      cat(paste("\n", "Multiplicative majority Rule added", "\n", sep=""))
    }
    
    # if the user called the multiplicative permutations rule
    if(mult.perm.logical)
    {
      cat(paste("\n", " Adding: ", perm.num, " permutations", "\n", sep=""))
      mult.perm.rule <- GetPermMultTermNode(multiplicative.prop.votes = multiplicative.prop,       # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. onr of the outputs of GetMultMajRule function. First column is the train.or.test, second column is the case.ID, 
                                            perm.num                  = perm.num,          # Integer, number of random votes to take for each case 
                                            div.logical               = div.logical,       # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                                            div.print                 = div.print,
                                            bind.prop.perm            = FALSE)              # logical, if true, the permutated terminal nodes are added at the end of the multiplicative.prop.votes. If FALSE, a seperate data frame is returned, cointating only the case.ID and the permuted terminal nodes
      crisp.case.class <- cbind(crisp.case.class, mult.perm.rule[3:ncol(mult.perm.rule)])
      
      cat(paste("\n", perm.num, " Permutations added", "\n", sep=""))
    } 
    
    multiplicative.prop <- multiplicative.prop[order(multiplicative.prop[, 2]), ]
  } # end the crisp rules that relay on multiplicative probabilites
  
  
  # Treat the stepwise majority rule
  if(step.maj.rule.logical)
  {
    cat(paste("\n", "Adding the stepwise majority Rule", "\n", sep=""))
    step.maj.rule <- GetStepMajRule(hie.RF             = hie.RF,
                                    prop.vote          = focal.votes,
                                    bind.prop.step.maj = FALSE) 
    
    crisp.case.class <- cbind(crisp.case.class, step.maj.rule[3])  
    cat(paste("\n", "Stepwise majority Rule added", "\n", sep=""))
  }
  
  
  # Order according to case.ID
  cat(paste("\n", "Ordering according to case.ID ", "\n", sep=""))
  
  crisp.case.class    <- crisp.case.class[order(crisp.case.class[, 2]), ]
  
  cat(paste("\n", "-->  Predicted terminal nodes ready ", "\n", sep=""))
  
  
  
  ######################################################################################
  ### STEP 5 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  if(mult.maj.rule.logical || mult.perm.logical){
  return.list <- list(raw.votes           = focal.votes,          # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
                      multiplicative.prop = multiplicative.prop,  # data frame, containing the muitplication of votes down each path from tree root to terminal node for each case
                      crisp.case.class    = crisp.case.class,     # Data frame, containing all the crisp classification of each case (as row) 
                      call                = match.call()
  )} else{
  return.list <- list(raw.votes           = focal.votes,          # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
                      crisp.case.class    = crisp.case.class,     # Data frame, containing all the crisp classification of each case (as row) 
                      call                = match.call())}
  
  
  return(return.list)
} # end function