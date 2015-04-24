#' Predict and assess performance of \code{new.data}, for which the 'true' class
#' is known.
#' 
#' This function takes as input a \code{new.data} data frame with the same 
#' explanatory variables as those used in \code{hie.RF}. Next, the 
#' \code{\link{predict.HRF}} function is applied to extract the proportion of 
#' votes that each case in \code{new.data} received for each node in each local 
#' classifier of \code{hie.RF}. The function then applies up to three different
#' methods for selecting a single terminal node (given by \code{crisp.rule} and
#' described in \code{\link{PerformanceHRF}}). Finally, the performance is
#' explored in relation to the observed class using various performance measures
#' (given by \code{per.index} and described in \code{\link{PerformanceHRF}}).
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param hie.RF              Object of class \code{HRF} - the output of
#'   \code{RunHRF}.
#'   
#' @param new.data            Data frame containing additional cases that were
#'   note a part of the original training set.
#'   
#' @param new.data.case.id    Integer, specifying the column number with the 
#'   \code{case.id} in the \code{new.data} data frame. The \code{case.id} values
#'   should be unique and different from those in the training data.
#'   
#' @param new.data.exp.var    Vector of integers, specifying the columns of 
#'   \code{new.data} that contains the same set of explanatory variables as used
#'   in the training of \code{hie.RF}. Default is all columns except
#'   \code{new.data.case.id} and \code{new.data.hie}.
#'   
#' @param new.data.hie        Vector of character or integers, containing the 
#'   names or column numbers of the hierarchical levels in \code{new.data}.
#'   Order of columns should be from the \code{tree.root} to the terminal nodes.
#'   If a single column is provided, it should contain only terminal nodes.
#'   
#' @param crisp.rule         The method of selecting a single crisp class from
#'   the proportion of votes. See details in \code{\link{PerformanceHRF}}.
#'   
#' @param perm.num           Integer, number of random permutations for each 
#'   case if \code{'multiplicative.permutation'} is applied. See details in 
#'   \code{\link{PerformanceHRF}}.
#'   
#' @param div.logical        Logical, if \code{TRUE} progress when 
#'   \code{'multiplicative.permutation'} is applied will be printed every
#'   \code{div.print} permutations
#'   
#' @param div.print         See above.  
#' 
#' @param per.index         The performance and accuracy indices to compute. See
#'   details in \code{\link{PerformanceHRF}}.
#'   
#' @param by.node           Logical, if \code{TRUE} performances indices will be
#'   estimated for each terminal node as well as for the overall confusion 
#'   matrix.
#'   
#' @param beta.h.F          Numeric in the range \code{beta.h.F} >= 0. Controls
#'   weights in the hierarchical F measure index. See \code{\link{HieFMeasure}}
#'   for details.
#'   
#' @param \dots     Optional parameters to be passed to low level functions. 
#' 
#'  @details See details on the various \code{crisp.rule}, \code{per.index} and 
#'  on the structure of the output data frames in \code{\link{PerformanceHRF}}.
#'    
#' @return A list with the following components:
#' {\tabular{lll}{
#' \code{"raw.vote"}               \tab \tab Data frame containing for each
#' case, the proportion of votes for each node in each local classifier (the
#' output of \code{\link{predict.HRF}}). \cr
#' 
#' \code{"crisp.case.class"}       \tab \tab Data frame containing the crisp
#' class for each case based on all options defined by \code{crisp.rule}. The
#' observed class (terminal node) is given at the last column under
#' \code{obs.term.node}. \cr
#' 
#' \code{"hie.performance"}        \tab \tab Data frame summarizing all the
#' performance measures, starting with the overall performance indices and
#' followed by all the \code{by.node} measures. See details in
#' \code{\link{PerformanceHRF}}. \cr
#' 
#' \code{"multiplicative.prop"}    \tab \tab Optional data frame with the
#' multiplicative proportion of votes (the output of
#' \code{\link{GetMultPropVotes}}). Will be returned if
#' \code{"multiplicative.majority"} or \code{"multiplicative.permutation"} were
#' selected.\cr
#' 
#' \code{"nodes.measures.columns"} \tab \tab Optional, if \code{by.node=TRUE},
#' data frame with three columns including the name of the terminal node, the
#' performance index and the name of the column in \code{hie.performance} that
#' holds the output. \cr
#' 
#' \code{"call"}                   \tab \tab The call to function
#' \code{PerformanceNewHRF}. \cr}}
#' 
#'   
#' @examples   
#' # create a random HRF dataset and RunHRF
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20, 
#'                         tree.depth = 4, 
#'                         new.data.observed = TRUE)
#' train.data <- random.hRF$train.data
#' new.data   <- random.hRF$new.data
#' hie.RF.random <- RunHRF(train.data = train.data,
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#' 
#' # assess performance for the new.data
#' perf.new.data <- PerformanceNewHRF(hie.RF           = hie.RF.random,
#'                  new.data         = new.data ,
#'                  new.data.case.id = 1,
#'                  new.data.hie     = c(2:(random.hRF$call$tree.depth + 1)),
#'                  crisp.rule       = c("stepwise.majority",
#'                                       "multiplicative.majority", 
#'                                       "multiplicative.permutation"),
#'                  perm.num         = 10,
#'                  div.print        = 2,
#'                  per.index        = c("flat.measures", "hie.F.measure"),
#'                  by.node          = TRUE)
#' 
#' # extract the data
#' names(perf.new.data)
#' perf.new.votes      <- perf.new.data$raw.votes
#' perf.new.crisp      <- perf.new.data$crisp.case.class  
#' perf.new.hie.perf   <- perf.new.data$hie.performance
#' perf.new.mult.prop  <- perf.new.data$multiplicative.prop
#' perf.new.nodes.meas <- perf.new.data$nodes.measures.columns
#' perf.new.call       <- perf.new.data$call 
#'                
#'  @seealso
#' \code{\link{RunHRF}} for running a hierarchical randomForest analysis, 
#' \code{\link{PredictNewHRF}} for predicting crisp class for each case of
#' \code{new.data}, \code{\link{PerformanceHRF}} for performance analysis, 
#' \code{\link{HieFMeasure}} for additional information on the hierarchical
#' performance measures.
#' 
#' @importFrom caret confusionMatrix
#' @importFrom reshape melt
#' @export
#'     


# Function - predict and asses performance of new.data, for which the 'true' class is known

PerformanceNewHRF = function(hie.RF,
                             new.data,
                             new.data.case.id = 1,
                             new.data.exp.var = NA,
                             new.data.hie,
                             crisp.rule       = c("stepwise.majority","multiplicative.m ajority", "multiplicative.permutation"),
                             perm.num         = 500,
                             div.logical      = TRUE,                          
                             div.print        = 25,
                             per.index        = c("flat.measures", "hie.F.measure"),
                             by.node          = TRUE,
                             beta.h.F         = 1,
                             ...
                                    )
  
{
 
  
  # required packages
    # require(randomForest)
    # require(e1071)
    # require(caret)
    # require(reshape)
   
  ######################################################################################
  ### STEP 1 - PERFORM CHECKS                                                        ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  cat("######################")
  cat(paste("\n", "-->  Veryfing Call", "\n", sep=""))
  
  # Most of the checks are performed within 'PredictNewHRF'
  
  
  if(missing(new.data.exp.var))
  {cat("Note! all columns other than those specified by new.data.case.id and new.data.hie will be used as input variables")}
  
  
  #####
  # check per.index
  temp.vec <-c("flat.measures", "hie.F.measure") 
  
  if(length(intersect(per.index,temp.vec)) < 1)
  {stop(paste("\n", "In PerformanceNewHRF, atleast one valid option for per.index is required", "\n", sep=""))}
  
  if(length(intersect(per.index,temp.vec)) > 0)
  {cat(paste("\n", "Performance will be evaluated using: ", per.index, "\n", sep=""))}
  
  rm(temp.vec)
  
  #####
  # Check by.node
  if(!is.logical(by.node))
  {cat(paste("\n", "In PerformanceNewHRF, by.node should be Logical. default of by.node=TRUE is used", "\n", sep=""))
   by.node <- TRUE}
  
  #####
  # check beta.h.F
  if(!is.numeric(beta.h.F))
  {cat(paste("\n", "In PerformanceNewHRF, beta.h.F should be a non-negative number", "\n",
             "Default of beta.h.F=1 is used", "\n", sep=""))
   beta.h.F <- 1    }
  
  if(beta.h.F < 0)
  {cat(paste("\n", "In PerformanceNewHRF, beta.h.F should be a non-negative number", "\n",
             "Default of beta.h.F=1 is used", "\n", sep=""))
   beta.h.F <- 1    }
  
  
  #####
  # check new.data.hie
  
  # Change from a character to numeric column number 
  if(is.character(new.data.hie))
  {new.data.hie  <- match(new.data.hie, names(new.data))}
  
  # checks if the columns specified in hie.levels contains factors
  CheckHieLevels(new.data[, new.data.hie])
  
  ######################################################################################
  ### STEP 2 - Extract values from hie.RF                                            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  unique.path       <- hie.RF$hier.struc$unique.path
  end.path.name     <- hie.RF$call$end.path.name
  if(is.null(end.path.name)){end.path.name <- "END.PATH"}
  
  
  ######################################################################################
  ### STEP 3 - Arrange new.data.exp.var + extract values from hie.RF                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  if(is.character(new.data.exp.var))
  {new.data.exp.var  <- match(new.data.exp.var, names(new.data))}
  
  # when new.data.exp.var is not given by the user 
  if(length(new.data.exp.var) == 1){
    if(is.na(new.data.exp.var))
    { new.data.exp.var <- 1:length(names(new.data)) 
      new.data.exp.var <- new.data.exp.var[new.data.exp.var != new.data.case.id]       # Remove case.id
      new.data.exp.var <- new.data.exp.var[!new.data.exp.var %in% new.data.hie]        # Remove hie.levels 
    }}
  
 
  ######################################################################################
  ### STEP 4 - Apply the Predict function                                            ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  
  
  #### Predict the raw proportion of votes, the multiplicative proportion of votes and the crisp classifcation for each case 
  cat("######################")
  cat(paste("\n", "-->  Calling PredictNewHRF for additional checks and votes extraction", "\n", sep=""))
  
  
  predict.new.data = PredictNewHRF(hie.RF           = hie.RF,               # object of class HRF - the output of RunHRF
                                   new.data         = new.data,             #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                                   new.data.case.id = new.data.case.id,     # Integer, specifying the column number that contains the case.id in the new.data data frame
                                   new.data.exp.var = new.data.exp.var,     # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the hie.RF
                                   crisp.rule       = crisp.rule,
                                   perm.num         = perm.num,
                                   div.logical      = div.logical,
                                   div.print        = div.print
                                   )
    
  
  # Extract values from the output of PredictNewHRF
  raw.votes           <- predict.new.data$raw.votes             # data frame, the raw proportionvotes for each node (terminal and internal) and each case, according to each local classifer
  multiplicative.prop <- predict.new.data$multiplicative.prop  # data frame, containing the muitplication of votes down each path from tree root to terminal node for each case
  crisp.case.class    <- predict.new.data$crisp.case.class
  
 
  
  ######################################################################################
  ### STEP 5 - Prepare observed terminal nodes                                       ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n", "-->  Preparing the observed terminal nodes ", "\n", sep=""))
  
  # prepare the train.data.acc data frame
  train.data.acc               <- subset(new.data,select = new.data.case.id)
  train.data.acc$obs.term.node <- NA
 
  # Get the terminal node of each case
  for (K2 in 1:nrow(new.data))
  {
    focal.path      <- new.data[K2, new.data.hie] # the column with the hierarchical data for case k2
    
    if(length(which(focal.path == end.path.name)) == 0)
       {train.data.acc[K2, "obs.term.node"] <- as.character(focal.path[1, ncol(focal.path)])}  
    
    if(length(which(focal.path==end.path.name))!=0)
       {train.data.acc[K2, "obs.term.node"] <- as.character(focal.path[1, (min(which(focal.path == end.path.name)) - 1)])} 
    
    rm(focal.path)
  } 
  rm(K2)
  
  # order according to case.id and change to Factors
  train.data.acc               <- train.data.acc[order(train.data.acc[, 1]), ]
  train.data.acc$obs.term.node <- factor(train.data.acc$obs.term.node)
  
  cat(paste("\n", "-->  Observed terminal nodes ready,  ", "\n", sep=""))
  
  ######################################################################################
  ### STEP 6 - Arrange data for performance analysis                                 ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat("######################")
  cat(paste("\n", "-->  Createing the crisp.rule by per.index data frames", "\n", sep=""))
  
  cat(paste("\n", "Identifying indices called by user", "\n", sep=""))
  ## Add the relevant measures columns                        ##
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  
  ### Create logical object for each option of crisp.rule and per.index
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
  
  # flat.measures
  if(!is.na(match("flat.measures", per.index))) # the flat.measures was called by the user
  { flat.measure.logical <- TRUE  }
  if(is.na(match("flat.measures", per.index))) # the flat.measures was not called by the user
  { flat.measure.logical <- FALSE }  
  
  # hie.F.measure
  if(!is.na(match("hie.F.measure", per.index))) # the hie.F.measure was called by the user
  { hie.F.measure.logical <- TRUE  }
  if(is.na(match("hie.F.measure", per.index))) # the hie.F.measure was not called by the user
  { hie.F.measure.logical <- FALSE }  
  
  
  
  # Add column to hie.performance for each performance index
  
  hie.performance <-data.frame(crisp.rule     = c(NA))
 
  if(flat.measure.logical)
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
  
  # If performance is to be estimated for each terminal node
  if(by.node)
  { # Start the by.node condition
    
    # Extract all the terminal nodes in the original hie.RF and insert the levels to unique.nodes
    nodes.info     <- hie.RF$hier.struc$nodes.info
    nodes.info     <- subset(nodes.info, nodes.info$term.int.node == "term.node")
    unique.nodes   <- unique(nodes.info$node.name)
    rm(nodes.info)
    
    if(flat.measure.logical) 
    { # the columns for the individual nodes measures of Accuracy
      
      # The columns names according to the output of confusionMatrix$byClass
      nodes.acc.ind <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy")
      
      # create a data frame with all pairs of nodes and indices
      nodes.measures                 <- expand.grid(unique.nodes, nodes.acc.ind)
      colnames(nodes.measures)       <- c("term.node", "index")
      nodes.measures$term.node.index <- paste(nodes.measures$term.node,
                                              nodes.measures$index,
                                              sep=";")
      
      hie.performance[,nodes.measures$term.node.index] <- NA
      # update the return data frame
      nodes.measures.columns <- nodes.measures    }
    
    #
    
    if(hie.F.measure.logical)
    { # The columns names for the hierarchical F measures
      nodes.h.acc.ind <- c("n.h.precision", "n.h.recall", "n.h.F.measure")
      
      # create a data frame with all pairs of nodes and indices
      nodes.h.measures                 <- expand.grid(unique.nodes, nodes.h.acc.ind)
      colnames(nodes.h.measures)       <- c("term.node", "index")
      nodes.h.measures$term.node.index <- paste(nodes.h.measures$term.node,
                                                nodes.h.measures$index,
                                                sep=";")
      
      hie.performance[, nodes.h.measures$term.node.index] <- NA
      # update the return data frame
      nodes.measures.columns <- nodes.h.measures    }
    
    #  add additional by.node measures here if needed
    
    
    # update the return data frame
    if(flat.measure.logical && hie.F.measure.logical)
    {nodes.measures.columns <- rbind(nodes.measures, nodes.h.measures)}
    
  } # End the by.node condition
  
  
  ## start the data frame for each crisp rule                 ##
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  cat(paste("\n", "Starting output data frames for each crisp rule", "\n", sep=""))
  
  if(mult.maj.rule.logical)
  { hie.perf.mult.maj <- hie.performance
    hie.perf.mult.maj[1, "crisp.rule"] <- "multiplicative.majority.rule" }
  
  if(mult.perm.logical)
  { hie.perf.mult.perm <- hie.performance
    hie.perf.mult.perm[c(1:perm.num), "crisp.rule"] <-c(paste("perm.", 1:perm.num, sep=""))}
  
  if(step.maj.rule.logical)
  { hie.perf.step.maj <- hie.performance
    hie.perf.step.maj[1, "crisp.rule"] <- "stepwise.majority"}
  
  
  cat(paste("\n", "-->  crisp.rule by per.index data frames created", "\n", sep=""))
  cat("######################")
  
  
  ######################################################################################
  ### STEP 7 - ESTIMATING ACCURACY FOR EACH ROW IN hie.performance                           ###
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
                                vector.2 = train.data.acc$obs.term.node)
    
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
    
    # insert results to hie.performance if flat.measure.logical==TRUE
    if(flat.measure.logical)
    {  # start if Flat
      hie.perf.mult.maj$Accuracy[1]       <- conf.matr.overall$Accuracy
      hie.perf.mult.maj$Kappa[1]          <- conf.matr.overall$Kappa 
      hie.perf.mult.maj$AccuracyLower[1]  <- conf.matr.overall$AccuracyLower 
      hie.perf.mult.maj$AccuracyUpper[1]  <- conf.matr.overall$AccuracyUpper 
      hie.perf.mult.maj$AccuracyNull[1]   <- conf.matr.overall$AccuracyNull 
      hie.perf.mult.maj$AccuracyPValue[1] <- conf.matr.overall$AccuracyPValue 
      hie.perf.mult.maj$McnemarPValue[1]  <- conf.matr.overall$McnemarPValue
    } # End if Flat
    
    
    if(flat.measure.logical && by.node)
    {  # start if Flat and by.node
      melt.byclass <- melt(conf.matr.byclass)
      colnames(melt.byclass)       <- c("term.node", "index", "value")
      melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                            melt.byclass$index,
                                            sep=";")
      for (i in 1:nrow(melt.byclass))
      { col.num                        <- match(melt.byclass$term.node.index[i],
                                                colnames(hie.perf.mult.maj))
        hie.perf.mult.maj[1, col.num]  <- melt.byclass$value[i]       }  
    } # end if Flat and by.node
    
    if(hie.F.measure.logical)
    { # start if Hie measures
      # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
      results.hie.F.measure = HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                          unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                          beta.h.F    = beta.h.F,
                                          by.node     = by.node)
      
      # Store each result in the correct place in hie.performance
      for(count.measure in 1: nrow(results.hie.F.measure))
      { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                           colnames(hie.perf.mult.maj))
        hie.perf.mult.maj[1 ,col.num.2] <- results.hie.F.measure[count.measure, "values"] }
      
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
      if(div.logical &&  round(count.perm/div.print, 0) == count.perm/div.print)
      {cat(paste("\n", "  Estimating performance measures for permutation number: ", count.perm , sep=""))}
      
      # the focal pair of observed and expected to work on
      focal.detail  <- hie.perf.mult.perm[count.perm, "crisp.rule"] 
      pred.nodes    <- crisp.case.class[, match(focal.detail, colnames(crisp.case.class))] 
      
      
      # make sure the levels of pred.nodes and obse.nodes are identical
      joined.levels <- JoinLevels(vector.1 = pred.nodes,
                                  vector.2 = train.data.acc$obs.term.node)
      
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
      
      # remove the prefix "Class: " from the row names of the by.class table
      rownames(conf.matr.byclass) <- sub("Class: ", "", rownames(conf.matr.byclass))
      
      # insert results to hie.performance if flat.measure.logical==TRUE
      if(flat.measure.logical)
      { # start if Flat
        hie.perf.mult.perm$Accuracy[count.perm]       <- conf.matr.overall$Accuracy
        hie.perf.mult.perm$Kappa[count.perm]          <- conf.matr.overall$Kappa 
        hie.perf.mult.perm$AccuracyLower[count.perm]  <- conf.matr.overall$AccuracyLower 
        hie.perf.mult.perm$AccuracyUpper[count.perm]  <- conf.matr.overall$AccuracyUpper 
        hie.perf.mult.perm$AccuracyNull[count.perm]   <- conf.matr.overall$AccuracyNull 
        hie.perf.mult.perm$AccuracyPValue[count.perm] <- conf.matr.overall$AccuracyPValue 
        hie.perf.mult.perm$McnemarPValue[count.perm]  <- conf.matr.overall$McnemarPValue
        
      } # end if Flat
      
      
      # insert results to hie.performance if flat.measure.logical==TRUE for each node
      if(flat.measure.logical && by.node)
      {  # start if Flat by.node
        melt.byclass <- melt(conf.matr.byclass)
        colnames(melt.byclass)       <- c("term.node", "index", "value")
        melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                              melt.byclass$index,
                                              sep=";")
        for (i in 1:nrow(melt.byclass))
        { col.num                        <- match(melt.byclass$term.node.index[i],
                                                  colnames(hie.perf.mult.perm))
          hie.perf.mult.perm[count.perm, col.num] <- melt.byclass$value[i]       }  
      } # end if Flat by.node
      
      if(hie.F.measure.logical)
      { # Start if Hie measures
        # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
        results.hie.F.measure = HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                            unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                            beta.h.F    = beta.h.F,
                                            by.node     = by.node)
        
        # Store wach result in the correct place in hie.performance
        for(count.measure in 1: nrow(results.hie.F.measure))
        { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                             colnames(hie.perf.mult.perm))
          hie.perf.mult.perm[count.perm,col.num.2] <- results.hie.F.measure[count.measure, "values"] }
      } # end if Hie measures
    } # End the count.perm for loop
  } #### End treatment of the multiplicative permutation Rule
  
  
  #########################################################
  ####    treat the Stepwise Majority Rule              ###
  #########################################################
  
  if(step.maj.rule.logical)
  { #### start treatment of  the stepwise Majority Rule
    cat(paste("\n","Estimating performance measures for the stepwise majority rule ","\n",sep=""))
    
    focal.detail  <- "stepwise.majority.rule"
    pred.nodes    <- crisp.case.class[, match(focal.detail, colnames(crisp.case.class))] 
    
    # make sure the levels of pred.nodes and obse.nodes are identical
    joined.levels <- JoinLevels(vector.1 = pred.nodes,
                                vector.2 = train.data.acc$obs.term.node)
    
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
    
    # remove the prefix "Class: " from the row names of the by.class table
    rownames(conf.matr.byclass) <- sub("Class: ", "", rownames(conf.matr.byclass))
    
    # insert results to hie.performance if flat.measure.logical==TRUE
    if(flat.measure.logical)
    { # Start if Flat
      hie.perf.step.maj$Accuracy[1]       <- conf.matr.overall$Accuracy
      hie.perf.step.maj$Kappa[1]          <- conf.matr.overall$Kappa 
      hie.perf.step.maj$AccuracyLower[1]  <- conf.matr.overall$AccuracyLower 
      hie.perf.step.maj$AccuracyUpper[1]  <- conf.matr.overall$AccuracyUpper 
      hie.perf.step.maj$AccuracyNull[1]   <- conf.matr.overall$AccuracyNull 
      hie.perf.step.maj$AccuracyPValue[1] <- conf.matr.overall$AccuracyPValue 
      hie.perf.step.maj$McnemarPValue[1]  <- conf.matr.overall$McnemarPValue
    } # End if Flat
    
    # insert results to hie.performance if flat.measure.logical==TRUE for each node
    if(flat.measure.logical && by.node)
    { # Start if Flat and by.node
      melt.byclass <- melt(conf.matr.byclass)
      colnames(melt.byclass)       <- c("term.node", "index", "value")
      melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                            melt.byclass$index,
                                            sep=";")
      for (i in 1:nrow(melt.byclass))
      { col.num                      <- match(melt.byclass$term.node.index[i],
                                              colnames(hie.perf.step.maj))
        hie.perf.step.maj[1,col.num] <- melt.byclass$value[i]       }  
    } # End if Flat and by.node
    
    if(hie.F.measure.logical)
    { # Start if Hie measure
      # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
      results.hie.F.measure = HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                          unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                          beta.h.F    = beta.h.F,
                                          by.node     = by.node)
      
      # Store each result in the correct place in hie.performance
      for(count.measure in 1: nrow(results.hie.F.measure))
      { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                           colnames(hie.perf.step.maj))
        hie.perf.step.maj[1 ,col.num.2] <- results.hie.F.measure[count.measure, "values"] }
    } # End if Hie measure
  }  #### End treatment of the stepwise Majority Rule
  
  cat(paste("\n", "-->  Performance measures estimated  ", "\n", sep=""))
  cat("######################")
  
  # Add the obs.term.node to crisp.case.class
  for (i in 1:nrow(crisp.case.class)){
    crisp.case.class[i,"obs.term.node"] <- train.data.acc[match(train.data.acc[i,1],
                                                                crisp.case.class[,2]), 
                                                          "obs.term.node"]
  }
  
  
  
  
  ######################################################################################
  ### STEP 7 - Create the return list                                                ###
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  # create the return data frame - bind the varioues crisp rules
  hie.performance  = rbind(if(step.maj.rule.logical)hie.perf.step.maj,
                           if(mult.maj.rule.logical)hie.perf.mult.maj,
                           if(mult.perm.logical)hie.perf.mult.perm)
  
  
  return.list <- list(raw.votes        = raw.votes ,       # data frame, the raw proportion votes for each node (terminal and internal) and each case, according to each local classifer
                      #train.data.acc   = train.data.acc,     # Data frame, the case.ID and observed terminal node of each case
                      crisp.case.class = crisp.case.class,    # Data frame, containing all the crisp classification of each case (as row) 
                      hie.performance  = hie.performance)
  
  if(mult.maj.rule.logical || mult.perm.logical)
  { new.list    <- list(multiplicative.prop = multiplicative.prop)
    return.list <- c(return.list, new.list)}
  
  #if(mult.maj.rule.logical)
  #{ new.list    <- list(hie.perf.mult.maj = hie.perf.mult.maj)
  #  return.list <- c(return.list, new.list)}
  
 # if(mult.perm.logical)
 # { new.list    <- list(hie.perf.mult.perm = hie.perf.mult.perm)
  #  return.list <- c(return.list, new.list)}
  
 # if(step.maj.rule.logical)
 # { new.list    <- list(hie.perf.step.maj = hie.perf.step.maj)
 #   return.list <- c(return.list, new.list)}
  
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