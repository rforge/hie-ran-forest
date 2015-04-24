#' Runs a flat classification on the same data as \code{hie.RF} and assess
#' performance.
#' 
#' This function takes as input an object of class \code{HRF}, identifies the 
#' observed terminal node for each case of the original training data, and runs 
#' a regular flat classification on all terminal nodes. The function then 
#' applies two different methods for selecting a single terminal node (given by 
#' \code{crisp.rule} and described in \code{\link{PerformanceHRF}}). Finally, 
#' the performance is explored in relation to the observed class using various
#' performance measures (given by \code{per.index})
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param hie.RF             Object of class \code{"HRF"} - the output of
#'   \code{RunHRF}.
#'    
#' @param mtry               Number of variables randomly sampled as candidates 
#'   at each split. Default is to use the same method used in \code{hie.RF}.
#'   
#' @param ntree              Number of trees to grow in the flat classifier. See
#'   \code{\link{randomForest}} for additional details. Default is the same
#'   \code{ntree} used in each local classifier of \code{hie.RF}.
#'   
#' @param importance         Logical, if \code{TRUE}, importance of variables
#'   will be assessed. See \code{\link{randomForest}} for additional details.
#'   Default is the same as \code{hie.RF}.
#'   
#' @param proximity          Logical, If \code{TRUE}, proximity will be
#'   calculated. See \code{\link{randomForest}} for additional details. Default
#'   is the same as \code{hie.RF}.
#'   
#' @param keep.forest        Logical, if \code{TRUE} (recommended) the forest
#'   will be retained. Default is the same as \code{hie.RF}.
#'   
#' @param keep.inbag         Logical, if \code{TRUE} an \emph{n} by \code{ntree}
#'   matrix be returned that keeps track of which cases are out-of-bag in which
#'   trees. \emph{n} being the number of cases in the training set. Required for
#'   votes extraction. Default is the same as \code{hie.RF}.
#'   
#' @param per.index          The performance and accuracy indices to compute. 
#'   See details in \code{\link{PerformanceHRF}}.
#'   
#' @param crisp.rule         The method of selecting a single crisp class from 
#'   the proportion of votes. See details in \code{\link{PerformanceHRF}}.
#'   
#' @param perm.num           Integer, number of random permutations for each 
#'   case if \code{'multiplicative.permutation'} is applied. See details in 
#'   \code{\link{PerformanceHRF}}.
#'   
#' @param by.node            Logical, if \code{TRUE}, performance measures are 
#'   estimated for each terminal node as well.
#'   
#' @param div.logical        Logical, if \code{TRUE} progress when 
#'   \code{'multiplicative.permutation'} is applied will be printed every
#'   \code{div.print} permutations.
#'   
#' @param div.print          See above.
#' 
#' @param beta.h.F           Numeric in the range \code{beta.h.F} => 0. Controls
#'   weights in the hierarchical F measure index. See \code{\link{HieFMeasure}}
#'   for details.
#'   
#' @param \dots       Optional parameters to be passed to low level functions. 
#'   
#' @details When running a flat classification, a single randomForest algorithm 
#' is used to distinguish between all terminal nodes. As no information on the
#' proportion of votes along the class hierarchy is produced, the 
#' \code{"stepwise.majority"} option for \code{crisp.rule} cannot be 
#' calculated.\cr
#' This function allows comparison of the performance of the hierarchical and
#' flat randomForest. In addition, the proportion of votes produced by this
#' function (can be extracted from the\code{randomForest} object) is
#' comparable to the proportion of votes returned by
#' \code{\link{PerformanceHRF}} in the \code{'multiplicative.prop'} data
#' frame.
#'   
#' @return List with the following components: 
#'  {\tabular{lll}{ 
#'   \code{'flat.RF'}                \tab \tab An object of class 
#'   \code{randomForest} for the flat classification  \cr
#'  
#'   \code{'crisp.case.class'}       \tab \tab Data frame containing the crisp 
#'   class for each case based on all options defined by \code{crisp.rule}. The 
#'   observed class (terminal node) is given at the last column under 
#'   \code{obs.term.node}. \cr
#'   
#'   \code{'hie.performance'}        \tab \tab Data frame summarizing all the
#'   performance measures, starting with the overall performance indices and
#'   followed by all the \code{by.node} measures. See details in
#'   \code{\link{PerformanceHRF}}. \cr
#'    
#'   \code{'nodes.measures.columns'} \tab \tab Optional, if \code{by.node=TRUE},
#'   data frame with three columns including the name of the terminal node, the 
#'   performance index and the name of the column in \code{hie.performance} that
#'   holds the output. \cr
#'   
#'   \code{'call'}                   \tab \tab The call to function
#'   \code{PerformanceFlatHRF}. \cr}}
#'   
#' @examples   
#' # analyse the OliveOilHie dataset
#' data(OliveOilHie)
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE)
#' 
#' # run and assess performance as flat classification
#' flat.RF.OO <- PerformanceFlatRF(hie.RF     = hie.RF.OO,                                   
#'                                 per.index  = c("flat.measures", 
#'                                                "hie.F.measure"),                         
#'                                 crisp.rule = c("multiplicative.majority", 
#'                                                "multiplicative.permutation"),   
#'                                 perm.num   = 10,         
#'                                 div.print  = 2)
#' # extract values
#' names(flat.RF.OO)
#' flat.RF.OO.RF        <- flat.RF.OO$flat.RF # object of class randomForest
#' votes.flat           <- flat.RF.OO$flat.RF$votes 
#' flat.RF.OO.crisp     <- flat.RF.OO$crisp.case.class
#' hie.perf.flat        <- flat.RF.OO$hie.performance
#' flat.RF.OO.nodes.mea <- flat.RF.OO$nodes.measures.columns
#' flat.RF.OO.call      <- flat.RF.OO$call
#' 
#' # compare the hie.perf.flat to the HRF analysis
#' # Performance of the hierarchical randomForest
#' perf.hRF.OO <- PerformanceHRF(hie.RF     = hie.RF.OO,
#'                               crisp.rule  = c("multiplicative.majority", 
#'                                               "multiplicative.permutation"),
#'                               perm.num    = 10,
#'                               div.print   = 2)
#' 
#' hie.perf.HRF   <- perf.hRF.OO$hie.performance
#' 
#' # Despite the overall high performance, HRF is slightly better...
#' comp.perf <- data.frame(model = c("Flat","HRF"))
#' join.perf <- rbind(hie.perf.flat[1, c("Accuracy","Kappa","h.F.measure")],
#'                    hie.perf.HRF[1, c("Accuracy","Kappa","h.F.measure")])
#' comp.perf <- cbind(data.frame(model = c("Flat","HRF")),
#'                    join.perf)
#' comp.perf
#'               
#' @seealso
#' \code{\link{RunHRF}} for running a hierarchical randomForest analysis,
#' \code{\link{PerformanceHRF}} for performance analysis,
#' \code{\link{HieFMeasure}} for additional information on the hierarchical
#' performance measures.
#' 
#' @importFrom caret confusionMatrix
#' @importFrom reshape melt
#' @export
#' 


PerformanceFlatRF = function(hie.RF,                                   
                             mtry         = hie.RF$call$mtry,          
                             ntree        = hie.RF$call$ntree,         
                             importance   = hie.RF$call$importance,    # Should importance of predictors be assessed?
                             proximity    = hie.RF$call$proximity,     # should the proximity be calcualted? 
                             keep.forest  = hie.RF$call$keep.forest,   
                             keep.inbag   = hie.RF$call$keep.inbag,    
                             per.index    = c("flat.measures", "hie.F.measure"),                          # the accurcary index to use. 
                             crisp.rule   = c("multiplicative.majority", "multiplicative.permutation"),   # Wether the multiplicative majority rule should be used to convert proportion of votes to classifed node or wether permutation based accuracy  should be estimated.
                             perm.num     = 500,         # Integer, number of random votes to take for each case
                             by.node      = TRUE,        # logical, indicating whether several indices for each node should be returned, if TRUE, the indices will appear in hie.performance after the overall performance measures 
                             div.logical  = TRUE,        # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                             div.print    = 25,
                             beta.h.F     = 1,
                             ...)
{ # Start function
  

  # required packages
  # require(randomForest)
  # require(e1071)
  # require(caret)
  # require(reshape)
  
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # treat the missing call parameters if needed:
  if(is.null(mtry))        {mtry        <- "tuneRF"}
  if(is.null(ntree))       {ntree       <- 500} 
  if(is.null(importance))  {importance  <- TRUE} 
  if(is.null(proximity))   {proximity   <- TRUE} 
  if(is.null(keep.forest)) {keep.forest <- TRUE}
  if(is.null(keep.inbag))  {keep.inbag  <- TRUE}
          
  
  
  
  # additinal checks are performed within RunFlatHRF
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call", "\n", sep=""))
  
  # check class of hie.RF
  if(class(hie.RF)!="HRF")
  {stop(paste("\n","PerformanceFlatRF:  hie.RF should be of class HRF","\n",sep=""))}
  
  unique.path  <- hie.RF$hier.struc$unique.path
  
  # check per.index
  temp.vec <-c("flat.measures", "hie.F.measure") 
  
  if(length(intersect(per.index, temp.vec)) < 1)
  {stop(paste("\n", "In PerformanceFlatRF, atleast one valid option for per.index is required", "\n", sep=""))}
  
  if(length(intersect(per.index, temp.vec)) > 0)
  {cat(paste("\n", "Performance will be evaluated using: ", per.index, "\n", sep=""))}
  
  rm(temp.vec)
  # check crisp.rule
  
  temp.vec <-c("multiplicative.majority", "multiplicative.permutation") 
  
  if(length(intersect(crisp.rule, temp.vec)) < 1)
  {stop(paste("\n", "In PerformanceFlatRF, atleast one valid option for crisp.rule is required", "\n", sep=""))}
  
  if(length(intersect(crisp.rule, temp.vec)) > 0)
  {cat(paste("\n", "Performance will be evaluated using crisp classification based on: ", crisp.rule, "\n", sep=""))}
  
  # check perm.num
  if(!is.numeric(perm.num))
  {stop(paste("\n", "In PerformanceFlatRF, perm.num should be a positive integer", "\n", sep=""))}
  
  if(perm.num < 1)
  {stop(paste("\n", "In PerformanceFlatRF, perm.num should be a positive integer", "\n", sep=""))}
  
  if(round(perm.num, 0) != perm.num)
  {stop(paste("\n", "In PerformanceFlatRF, perm.num should be a positive integer", "\n", sep=""))}
  
  # Check by.node
  if(!is.logical(by.node))
  {cat(paste("\n", "In PerformanceFlatRF, by.node should be Logical. default of by.node=TRUE is used", "\n", sep=""))
   by.node <- TRUE}
  
  # Check div.logical
  if(!is.logical(div.logical))
  {cat(paste("\n", "In PerformanceFlatRF, div.logical should be Logical. default of div.logical=TRUE is used", "\n", sep=""))
   div.logical <- TRUE}
  
  # check div.print
  if(!is.numeric(div.print))
  {cat(paste("\n", "In PerformanceFlatRF, div.print should be a positive integer", "\n",
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  if(div.print < 1)
  {cat(paste("\n", "In PerformanceFlatRF, div.print should be a positive integer", "\n",
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  if(round(div.print, 0) != div.print)
  {cat(paste("\n", "In PerformanceFlatRF, div.print should be a positive integer", "\n",
             "Default of div.print=25 is used", "\n", sep=""))
   div.print <- 25 }
  
  # check beta.h.F
  if(!is.numeric(beta.h.F))
  {cat(paste("\n", "In PerformanceFlatRF, beta.h.F should be a non-negative number", "\n",
             "Default of beta.h.F=1 is used","\n",sep=""))
   beta.h.F <- 1    }
  
  if(beta.h.F<0)
  {cat(paste("\n","In PerformanceFlatRF, beta.h.F should be a non-negative number", "\n",
             "Default of beta.h.F=1 is used", "\n", sep=""))
   beta.h.F <- 1    }
  
  cat(paste("\n", "-->  Call Verified", "\n", sep=""))
  
  
  ######################################################################################
  ### STEP 2 - CREATE LOGICAL ARGUMENTS FOR EACH OPTION OF per.index and crisp.rule    ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # Multiplicative Majority rule
  if(!is.na(match("multiplicative.majority", crisp.rule))) # the multiplicative.majority rule was called by the user
  { mult.maj.rule.logical <- TRUE  }
  if(is.na(match("multiplicative.majority", crisp.rule)))  # the multiplicative.majority rule was not called by the user
  { mult.maj.rule.logical <- FALSE }  
  
  # multiplicative.permutation
  if(!is.na(match("multiplicative.permutation", crisp.rule))) # the multiplicative.permutation was called by the user
  { mult.perm.logical     <- TRUE  } 
  if(is.na(match("multiplicative.permutation", crisp.rule)))  # the multiplicative.permutation was not called by the user
  { mult.perm.logical     <- FALSE }  
  
  # flat.measures
  if(!is.na(match("flat.measures", per.index))) # the flat.measures was called by the user
  { flat.measures.logical <- TRUE  }
  if(is.na(match("flat.measures", per.index)))  # the flat.measures was not called by the user
  { flat.measures.logical <- FALSE }  
  
  # hie.F.measure
  if(!is.na(match("hie.F.measure", per.index))) # the hie.F.measure was called by the user
  { hie.F.measure.logical <- TRUE  }
  if(is.na(match("hie.F.measure", per.index)))  # the hie.F.measure was not called by the user
  { hie.F.measure.logical <- FALSE }  
  
  # add additional logical variables here if new crisp rules and/or measurees are added
  
  ######################################################################################
  ### STEP 3 - Run the flat RandomForest                                             ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # run the flat RF
  run.flat <- RunFlatHRF(hie.RF       = hie.RF,        # object of class HRF - the output of RunHRF
                         mtry         = mtry,          # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier
                         ntree        = ntree,         # number of trees to grow in each local classifier
                         importance   = importance,    # Should importance of predictors be assessed?
                         proximity    = proximity,     # should the proximity be calcualted? 
                         keep.forest  = keep.forest,   
                         keep.inbag   = keep.inbag    
  )
  
  # extract the output
  flat.RF         <- run.flat$flat.RF
  train.flat.case <- run.flat$train.flat.case
  
  
  
  # all the terminal nodes found in the train.flat.case
  unique.nodes   <- unique(train.flat.case$obs.term.node)
  
  ######################################################################################
  ### STEP 4 - CREATE THE RETURN DATA FRAME                                          ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n", "-->  Createing the crisp.rule by per.index data frames", "\n", sep=""))
  
  cat(paste("\n", "Identifying indices called by user", "\n", sep=""))
  ## Add the relevant measures columns                        ##
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  hie.performance <- data.frame(crisp.rule = c(NA))
  
  if(flat.measures.logical)
  { ### the columns from confusionMatrix$overall
    hie.performance$Accuracy       <- NA
    hie.performance$Kappa          <- NA 
    hie.performance$AccuracyLower  <- NA 
    hie.performance$AccuracyUpper  <- NA 
    hie.performance$AccuracyNull   <- NA 
    hie.performance$AccuracyPValue <- NA 
    hie.performance$McnemarPValue  <- NA    }
  
  if(hie.F.measure.logical)
  { # the columns for the overall hierarchical F measure
    hie.performance$h.precision  <- NA
    hie.performance$h.recall     <- NA
    hie.performance$h.F.measure  <- NA      }
  
  ### add additional overall measures here ###
  
  
  
  if(by.node)
  { # Start the by.node condition
    if(flat.measures.logical) 
    { # the columns for the individual nodes measures of Accuracy
      
      # The columns names according to the output of confusionMatrix$byClass
      nodes.acc.ind <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy")
      
      # create a data frame with all pairs of nodes and indices
      nodes.measures                 <- expand.grid(unique.nodes, nodes.acc.ind)
      colnames(nodes.measures)       <- c("term.node", "index")
      nodes.measures$term.node.index <- paste(nodes.measures$term.node,
                                              nodes.measures$index,
                                              sep=";")
      
      hie.performance[, nodes.measures$term.node.index] <- NA
      # update the return data frame
      nodes.measures.columns <- nodes.measures    }
    
    #
    
    if(hie.F.measure.logical)
    { # The columns names for the hierarchical F measures
      
      
      # create a data frame with all pairs of nodes and indices
      nodes.h.measures                 <- expand.grid(unique.nodes, c("n.h.precision", "n.h.recall", "n.h.F.measure"))
      colnames(nodes.h.measures)       <- c("term.node", "index")
      nodes.h.measures$term.node.index <- paste(nodes.h.measures$term.node,
                                                nodes.h.measures$index,
                                                sep=";")
      
      hie.performance[, nodes.h.measures$term.node.index] <- NA
      # update the return data frame
      nodes.measures.columns <- nodes.h.measures    }
    
    #  add additional by.node measures here if needed
    
    
    # update the return data frame
    if(flat.measures.logical && hie.F.measure.logical)
    {nodes.measures.columns <- rbind(nodes.measures, nodes.h.measures)}
    
  } # End the by.node condition
  
  
  
  ## start the data frame for each crisp rule                 ##
  ##############################################################
  
  cat(paste("\n", "Starting output data frames for each crisp rule", "\n", sep=""))
  
  if(mult.maj.rule.logical)
  { hie.perf.mult.maj                  <- hie.performance
    hie.perf.mult.maj[1, "crisp.rule"] <- "multiplicative.majority.rule" }
  
  if(mult.perm.logical)
  { hie.perf.mult.perm                              <- hie.performance
    hie.perf.mult.perm[c(1:perm.num), "crisp.rule"] <- c(paste("perm.", 1:perm.num, sep=""))  }
  
  
  ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
  #    Add here addtional hie.performance data frames if new methods are added     #
  ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
  
  
  cat(paste("\n", "-->  crisp.rule by per.index data frames created", "\n", sep=""))
  cat("######################")
  
  
  ######################################################################################
  ### STEP 5 - PREPARING THE TERMINAL NODES AS PREDICTED BY THE HRF                  ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n", "-->  Preparing the predicted terminal nodes from the flat RandomForest ", 
            "\n", sep = ""))
  
  # for each case (rows) the proportion of OOB votes for each node
  # according to the flat classification
  focal.votes <- as.data.frame(flat.RF$votes)
  
  # check row sums = 1
  if (!all(abs(rowSums(focal.votes) - 1) <= 10^-8)) {
    stop(paste("\n", "In PerformanceFlatRF, Row sums of proportion of votes do not sum to 1", 
               "\n", sep = ""))
  }
  
  # add to focal.votes the train.or.test column and the case.ID column
  focal.votes                                       <- cbind(train.flat.case[1], focal.votes)
  temp.vec                                          <- data.frame(train.or.test = c(NA))
  temp.vec[c(1:nrow(focal.votes)), "train.or.test"] <- "train"
  focal.votes                                       <- cbind(temp.vec, focal.votes)
  rm(temp.vec)
  
  # reorder  focal votes and 
  focal.votes <- focal.votes[order(focal.votes[, 2]), ]
  
  # re-order the train.flat.case according to case.ID
  train.flat.case <- train.flat.case[order(train.flat.case[, 1]), ]
  
  # check consistancy of case.ID
  if(!all(focal.votes[, 2] == train.flat.case[, 1]))
  {stop(paste("\n", "In PerformanceFlatRF, inconsistent case.ID between observed and predicted porportion of votes", "\n", sep=""))}
  

  
  # the data frame that collects all the crisp rules
  crisp.case.class <- focal.votes[, c(1, 2)] 
  
  # if the user called the multiplicative majority rule
  if(mult.maj.rule.logical)
  {
    cat(paste("\n", "Adding the multiplicative majority Rule", "\n", sep=""))
    mult.maj.rule <- GetMultMajRule(prop.multiplicative.votes = focal.votes,     # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is train.or.test and the second is case.ID
                                    bind.prop.mul.maj         = FALSE)
    
    crisp.case.class <- cbind(crisp.case.class,mult.maj.rule[3])
    cat(paste("\n", "Multiplicative majority Rule added", "\n", sep=""))
  }
  
  # if the user called the multiplicative permutations rule
  if(mult.perm.logical)
  {
    cat(paste("\n", " Adding: ", perm.num, " permutations", "\n", sep=""))
    mult.perm.rule <- GetPermMultTermNode(multiplicative.prop.votes = focal.votes,       # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. one of the outputs of GetMultMajRule function. First column is the train.or.test, second column is the case.ID, 
                                          perm.num                  = perm.num,          # Integer, number of random votes to take for each case 
                                          div.logical               = div.logical,       # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                                          div.print                 = div.print,
                                          bind.prop.perm            = FALSE)             # logical, if true, the permutated terminal nodes are added at the end of the multiplicative.prop.votes. If FALSE, a seperate data frame is returned, cointating only the case.ID and the permuted terminal nodes
    crisp.case.class <- cbind(crisp.case.class, mult.perm.rule[3:ncol(mult.perm.rule)])
    
    cat(paste("\n", perm.num," Permutations added", "\n", sep=""))
  } 
  
  cat(paste("\n", "-->  Predicted terminal nodes ready ", "\n", sep=""))
  
  
  ######################################################################################
  ### STEP 6 - ESTIMATING ACCURACY FOR EACH ROW IN hie.performance                   ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  cat("######################")
  cat(paste("\n", "-->  Start estimating performance measures ", "\n", sep=""))
  
  ######################################################
  ####    treat the Multiplicative Majority Rule     ###
  ######################################################
  
  if(mult.maj.rule.logical)
  { #### start treatment of  the Multiplicative Majority Rule
    
    cat(paste("\n", "Estimating performance measures for the multiplicative majority rule ", "\n", sep=""))
    
    
    focal.detail  <- "multiplicative.majority.rule"
    pred.nodes    <- crisp.case.class[, match(focal.detail, colnames(crisp.case.class))] 
    
    # make sure the levels of pred.nodes and obse.nodes are identical
    joined.levels <- JoinLevels(vector.1 = pred.nodes,
                                vector.2 = train.flat.case$obs.term.node)
    
    pred.nodes    <- joined.levels$vector.1
    obse.nodes    <- joined.levels$vector.2
    
    rm(joined.levels) # remove 
    
    # Use the confusionmatrix function caret package to get the confusion matrix
    conf.matr <- confusionMatrix(data      = pred.nodes,
                                 reference = obse.nodes,
                                 dnn       = c("Prediction", "Observed"))
    
    conf.matr.table   <- conf.matr$table
    conf.matr.overall <- as.data.frame(t(conf.matr$overall))
    conf.matr.byclass <- conf.matr$byClass
    
    # remove the prefix "Class: " from the row names of the byclass table
    rownames(conf.matr.byclass)<-sub("Class: ", "", rownames(conf.matr.byclass))
    
    # insert results to hie.performance if flat.measures.logical==TRUE
    if(flat.measures.logical)
    {  # start if Flat
      hie.perf.mult.maj$Accuracy[1]       <- conf.matr.overall$Accuracy
      hie.perf.mult.maj$Kappa[1]          <- conf.matr.overall$Kappa 
      hie.perf.mult.maj$AccuracyLower[1]  <- conf.matr.overall$AccuracyLower 
      hie.perf.mult.maj$AccuracyUpper[1]  <- conf.matr.overall$AccuracyUpper 
      hie.perf.mult.maj$AccuracyNull[1]   <- conf.matr.overall$AccuracyNull 
      hie.perf.mult.maj$AccuracyPValue[1] <- conf.matr.overall$AccuracyPValue 
      hie.perf.mult.maj$McnemarPValue[1]  <- conf.matr.overall$McnemarPValue
    } # End if Flat
    
    
    if(flat.measures.logical && by.node)
    {  # start if Flat and by.node
      melt.byclass                 <- melt(conf.matr.byclass)
      colnames(melt.byclass)       <- c("term.node", "index", "value")
      melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                            melt.byclass$index,
                                            sep=";")
      for (i in 1:nrow(melt.byclass))
      { col.num                       <- match(melt.byclass$term.node.index[i],
                                               colnames(hie.perf.mult.maj))
        hie.perf.mult.maj[1, col.num] <- melt.byclass$value[i]       }  
    } # end if Flat and by.node
    
    if(hie.F.measure.logical)
    { # start if Hie measures
      # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
      results.hie.F.measure <- HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                           unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                           beta.h.F    = beta.h.F,
                                           by.node     = by.node)
      
      # Store each result in the correct place in hie.performance
      for(count.measure in 1: nrow(results.hie.F.measure))
      { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                           colnames(hie.perf.mult.maj))
        hie.perf.mult.maj[1, col.num.2] <- results.hie.F.measure[count.measure, "values"] }
      
    } # end if Hie measures
  }  #### End treatment of the Multiplicative Majority Rule
  
  #########################################################
  ####    treat the Multiplicative Permutation Rule     ###
  #########################################################
  
  if(mult.perm.logical)
  { #### Start treatment of the multiplicative permutation Rule
    cat(paste("\n", "Estimating performance measures for the multiplicative permutations rule ", "\n", sep=""))
    
    for (count.perm in 1:nrow(hie.perf.mult.perm))
    { # Start the count.perm for loop
      
      # print the progress
      if(div.logical &&  round(count.perm / div.print, 0) == count.perm / div.print)
      {cat(paste("\n", "  Estimating performance measures for permutation number: ", count.perm , sep=""))}
      
      # the focal pair of observed and expected to work on
      focal.detail  <- hie.perf.mult.perm[count.perm, "crisp.rule"] 
      pred.nodes    <- crisp.case.class[, match(focal.detail, colnames(crisp.case.class))] 
      
      
      # make sure the levels of pred.nodes and obse.nodes are identical
      joined.levels <- JoinLevels(vector.1 = pred.nodes,
                                  vector.2 = train.flat.case$obs.term.node)
      
      pred.nodes    <- joined.levels$vector.1
      obse.nodes    <- joined.levels$vector.2
      rm(joined.levels) # remove 
      
      
      # Use the confusionmatrix function caret package to get the confusion matrix
      conf.matr <- confusionMatrix(data      = pred.nodes,
                                   reference = obse.nodes,
                                   dnn       = c("Prediction", "Observed"))
      
      conf.matr.table   <- conf.matr$table
      conf.matr.overall <- as.data.frame(t(conf.matr$overall))
      conf.matr.byclass <- conf.matr$byClass
      
      # remove the prefix "Class: " from the row names of the byclass table
      rownames(conf.matr.byclass) <- sub("Class: ", "", rownames(conf.matr.byclass))
      
      # insert results to hie.performance if flat.measures.logical==TRUE
      if(flat.measures.logical)
      { # start if Flat
        hie.perf.mult.perm$Accuracy[count.perm]       <- conf.matr.overall$Accuracy
        hie.perf.mult.perm$Kappa[count.perm]          <- conf.matr.overall$Kappa 
        hie.perf.mult.perm$AccuracyLower[count.perm]  <- conf.matr.overall$AccuracyLower 
        hie.perf.mult.perm$AccuracyUpper[count.perm]  <- conf.matr.overall$AccuracyUpper 
        hie.perf.mult.perm$AccuracyNull[count.perm]   <- conf.matr.overall$AccuracyNull 
        hie.perf.mult.perm$AccuracyPValue[count.perm] <- conf.matr.overall$AccuracyPValue 
        hie.perf.mult.perm$McnemarPValue[count.perm]  <- conf.matr.overall$McnemarPValue
        
      } # end if Flat
      
      
      # insert results to hie.performance if flat.measures.logical==TRUE for each node
      if(flat.measures.logical && by.node)
      {  # start if Flat by.node
        melt.byclass                 <- melt(conf.matr.byclass)
        colnames(melt.byclass)       <- c("term.node", "index", "value")
        melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                              melt.byclass$index,
                                              sep=";")
        for (i in 1:nrow(melt.byclass))
        { col.num                                 <- match(melt.byclass$term.node.index[i],
                                                           colnames(hie.perf.mult.perm))
          hie.perf.mult.perm[count.perm, col.num] <- melt.byclass$value[i]       }  
      } # end if Flat by.node
      
      if(hie.F.measure.logical)
      { # Start if Hie measures
        # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
        results.hie.F.measure <- HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                             unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                             beta.h.F    = beta.h.F,
                                             by.node     = by.node)
        
        # Store each result in the correct place in hie.performance
        for(count.measure in 1: nrow(results.hie.F.measure))
        { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                             colnames(hie.perf.mult.perm))
          hie.perf.mult.perm[count.perm, col.num.2] <- results.hie.F.measure[count.measure, "values"] }
      } # end if Hie measures
    } # End the count.perm for loop
  } #### End treatment of the multiplicative permutation Rule
  
  
  
  cat(paste("\n", "-->  Performance measures estimated  ", "\n", sep=""))
  cat("######################")
  
  
  # Add the obs.term.node to crisp.case.class
  for (i in 1:nrow(crisp.case.class)){
    crisp.case.class[i,"obs.term.node"] <- train.flat.case[match(train.flat.case[i,1],
                                                                 crisp.case.class[,2]), 
                                                           "obs.term.node"]
  }
  
  
  ######################################################################################
  ### STEP 7 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  # create the return data frame - bind the varioues crisp rules
  hie.performance  = rbind(if(mult.maj.rule.logical)hie.perf.mult.maj,
                           if(mult.perm.logical)hie.perf.mult.perm)
  
  return.list <- list(flat.RF           = flat.RF,       # Object of class randomForest, the result of flat classification
                      #train.flat.case  = train.flat.case,
                      crisp.case.class  = crisp.case.class,    # Data frame, containing all the crisp classification of each case (as row) + the observed case at the last column
                      hie.performance   = hie.performance)     # all the performance measure for all crisp.rule
 
  
  #if(mult.maj.rule.logical)
  #{ new.list    <- list(hie.perf.mult.maj = hie.perf.mult.maj)
   # return.list <- c(return.list, new.list)}
  
  #if(mult.perm.logical)
  #{ new.list    <- list(hie.perf.mult.perm = hie.perf.mult.perm)
  #  return.list <- c(return.list, new.list)}
  
  
  if(by.node)
  { new.list    <- list(nodes.measures.columns = nodes.measures.columns)
    return.list <- c(return.list, new.list)}
   
  # add the call
  new.list    <- list(call = match.call())
  return.list <- c(return.list, new.list)
  
  # set the new class
  #class(return.list) <- "Hier.RF.Performance"
  
  return(return.list)
  
  
  
} # End function
                              