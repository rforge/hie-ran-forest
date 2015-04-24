#' Flat and hierarchical performance measures.
#' 
#' This function predicts the proportion of votes for each case in each local
#' classifier and then estimates various flat and hierarchical performance
#' measures. The performance measures are based on translating the proportion of
#' votes to a crisp class (selecting a single class). The function offers three
#' different method for translating the soft proportion of votes to a crisp
#' class.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#' 
#' @param hie.RF            Object of class \code{"HRF"} - the output of
#'   \code{RunHRF}.
#'   
#' @param per.index         The performance and accuracy indices to compute. See
#'   details below.
#'   
#' @param crisp.rule        The method of selecting a single crisp class from
#'   the proportion of votes. See details below.
#'   
#' @param perm.num          Integer, number of random permutations for each case
#'   if \code{'multiplicative.permutation'} is applied.
#'   
#' @param by.node           Logical, if \code{TRUE}, performances indices will
#'   be estimated for each terminal node as well as for the overall confusion 
#'   matrix.
#'   
#' @param div.logical       Logical, if \code{TRUE} progress when 
#'   \code{'multiplicative.permutation'} is applied will be printed every
#'   \code{div.print} permutations.
#'   
#' @param div.print         See above.
#' 
#' @param beta.h.F          Numeric in the range \code{beta.h.F} >= 0. Controls
#'   weights in the hierarchical F measure index. See \code{\link{HieFMeasure}}
#'   for details.
#'   
#' @param \dots     Optional parameters to be passed to low level functions.
#'   
#' @details For a given flat classification, the output of the randomForest
#'   algorithm for each case is the proportion of out-of-bag (OOB) votes that
#'   each class (node) received. In the hierarchical version of randomForest,
#'   the proportion of OOB votes are returned separately for each local
#'   classifier (see \code{\link{predict.HRF}}). In flat classification, the
#'   proportions of votes are translated into a crisp classification by applying
#'   the majority rule, i.e., by selecting the class with the highest proportion
#'   of votes. In the hierarchical version of randomForest, there are 3
#'   different methods to select one crisp class for a given case. \cr
#'   
#' \strong{The \code{"stepwise.majority"} method: } \cr In each local
#' classifier, the flat majority rule is applied. Then starting with the
#' \code{tree.root}, a case is classified down the tree until a terminal node is
#' reached. This method gives high emphasis to local classifiers close to the
#' \code{tree.root}, since a case can only be classified to sibling classes of
#' those selected using the majority rule in classes closer to the
#' \code{tree.root}.\cr If the blue values in the figure below are the
#' proportion of votes for a given case in a given local classifier, then the
#' case would be classified to class \emph{A} in local classifier \emph{C.1},
#' and then to class \emph{A.2} in local classifier \emph{C.2}.\cr
#'   
#' \strong{The \code{"multiplicative.majority"} method:}  \cr First multiply the
#' proportion of votes along each path from the \code{tree.root} to any of the
#' terminal nodes (see: \code{\link{GetMultPropVotes}}). The result is a vector
#' of probabilities for each terminal node, similar to that returned from flat
#' classification. Next, apply the majority rule and select the terminal node
#' that received the highest multiplicative proportion of votes. This method
#' give less emphasis to local classifiers near the root of the class hierarchy,
#' but may depend on the number of siblings nodes. \cr If the red values in the
#' figure below are the multiplication of votes along each path to all 4
#' terminal nodes for a given case, then the case would be classified to class
#' \emph{B.2} \cr
#'   
#' \strong{The \code{"multiplicative.permutation"} method: }\cr Similar to
#' \code{multiplicative.majority}, but instead of applying a majority rule, 
#' the method randomly select a terminal node based on the multiplicative 
#' probabilities. The user defines the number of permutations and accuracy is 
#' assessed separately for each permutation. When most cases are classified to 
#' the same class in various classification trees (i.e., when the proportion of 
#' votes for one category is close to 1), the mean accuracy over all permutation
#' should be similar to that achieved under the \code{"multiplicative.majority"}
#' method.\cr In the example in the figure below, each permutation will choose a
#' class as random draw with probabilities equal to the values in red. \cr
#' 
#'   \if{html}{\figure{StepMultExp.jpeg}} 
#'   \if{latex}{\figure{StepMultExp.jpeg}{options: width=7cm}}
#' 
#'  \strong{The two options of \code{per.index}:} \cr
#'    {\tabular{lll}{
#'    \code{"flat.measures"} \tab \tab Accuracy measures that do not consider 
#'    the hierarchical structure of the classes. In this case, the accuracy
#'    measures returned are those returned by \code{\link{confusionMatrix}} of
#'    the \code{caret} package. \cr \cr \cr
#'    
#'    \code{"hie.F.measure"} \tab \tab Accuracy measures that account for the 
#'    hierarchical structure of the classes. See details in
#'    \code{\link{HieFMeasure}} and reference within. \cr}}
#'    
#'@return A list with the following components:
#'  
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
#' followed by all the \code{by.node} measures. See details below. \cr
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
#' \code{PerformanceHRF}. \cr}}
#' 
#' @section The hie.performance data frame: 
#' 
#' The \code{hie.performance} data frame contains the following overall
#' performance measures:
#' 
#'  {\tabular{lll}{
#'    \code{"crisp.rule"}     \tab \tab The crisp rule used to create the crisp
#'    classification. \cr
#'    
#'    \code{"Accuracy"}       \tab \tab The overall accuracy. \cr
#'    
#'    \code{"Kappa"}          \tab \tab The unweighted Kappa statistic. \cr
#'    
#'    \code{"AccuracyLower"}  \tab \tab The lower bound of the 95 confidence
#'    interval around the overall error based on \code{\link{binom.test}}. \cr
#'    
#'    \code{"AccuracyUpper"}  \tab \tab The upper bound of the 95 confidence
#'    interval. \cr
#'    
#'    \code{"AccuracyNull"}   \tab \tab The expected null accuracy. \cr
#'    
#'    \code{"AccuracyPValue"} \tab \tab One side test to see if accuracy is
#'    better than "no information rate". \cr
#'    
#'    \code{"AccuracyNull"}   \tab \tab A p-value from McNemar's test using
#'    \code{\link{mcnemar.test}} (may return \code{NA}). \cr
#'    
#'    \code{"h.precision"}    \tab \tab The hierarchical precision from
#'    \code{\link{HieFMeasure}}. \cr
#'    
#'    \code{"h.recall"}       \tab \tab The hierarchical recall from
#'    \code{\link{HieFMeasure}}. \cr
#'    
#'    \code{"h.F.measure"}    \tab \tab The hierarchical F measure from
#'    \code{\link{HieFMeasure}}. \cr }}
#'  These columns are followed by all the \code{by.node} results, with the name
#'  of each column follows the strucutre: 'nodename;measure'.
#'    
#' @examples
#' # create a random HRF dataset and run HRF analysis
#' set.seed(354)
#' random.hRF    <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data    <- random.hRF$train.data
#' hie.RF.random <- RunHRF(train.data = train.data,
#'                         case.ID    = "case.ID",
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#' 
#' # assess performance
#' perf.hRF.random <- PerformanceHRF(hie.RF     = hie.RF.random,
#'                                   perm.num   = 20,
#'                                   div.print  = 5)
#' 
#' ### Extract values ###
#' names(perf.hRF.random)
#' raw.vote.random               <- perf.hRF.random$raw.vote
#' crisp.case.class.random       <- perf.hRF.random$crisp.case.class        
#' hie.performance.random        <- perf.hRF.random$hie.performance
#' multiplicative.prop.random    <- perf.hRF.random$multiplicative.prop     
#' nodes.measures.columns.random <- perf.hRF.random$nodes.measures.columns
#' hie.perf.call.random          <- perf.hRF.random$call
#'
#' #### example with the OliveOilHie dataset
#' data(OliveOilHie)
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE)
#' 
#' perf.hRF.olive <- PerformanceHRF(hie.RF    = hie.RF.OO,
#'                                  crisp.rule  = c("multiplicative.majority"))
#' 
#' ### Extract values ###
#' crisp.case.class.olive <- perf.hRF.olive$crisp.case.class        
#' hie.performance.olive <- perf.hRF.olive$hie.performance
#'
#' ### plotting option ### 
#' # create a confusion matrix
#' conf.matr.olive <- as.data.frame(table(crisp.case.class.olive$obs.term.node,
#'                       crisp.case.class.olive$multiplicative.majority.rule))
#' # use the PlotImportanceHie to plot the confusion matrix
#' PlotImportanceHie(input.data = conf.matr.olive,
#'                   X.data     = 1,                                     
#'                   Y.data     = 2,                            
#'                   imp.data   = 3,
#'                   plot.type  = "Tile",
#'                   X.Title    = c("Observed"),
#'                   Y.Title    = c("mMultiplicative majority rule"),
#'                   imp.title  = c("frequency"),
#'                   low.col    = "darkslategray4",
#'                   high.col   = "red",
#'                   geom.tile.bor.col = "gray20")
#'                   
#' @importFrom caret confusionMatrix
#' @importFrom reshape melt
#' @seealso
#' \code{\link{RunHRF}} for running a hierarchical randomForest analysis, 
#' \code{\link{PerformanceNewHRF}} for performance analysis on \code{new.data} 
#' with observed terminal node, \code{\link{PerformanceFlatRF}} for hierarchical
#' and flat performance analyses for a flat randomForest, 
#' \code{\link{HieFMeasure}} for additional information on the hierarchical
#' performance measures.
#' @export




# function, returns mutltiple flat and hierarchical performance measures for a crisp classifcation. Crispt classifcation may be based on multiplicative majority, stepwise majority or multiplicative permutations 
PerformanceHRF = function(hie.RF,                                         
                          per.index   = c("flat.measures", "hie.F.measure"),      
                          crisp.rule  = c("stepwise.majority", "multiplicative.majority", "multiplicative.permutation"),   # Wether the multiplicative majority rule shoulf be used to convert proportion of votes to classifed node or wether permutation based accuracy  should be estimated.
                          perm.num    = 500,      # Integer, number of random votes to take for each case
                          by.node     = TRUE,     # logical, indicating whether several indices for each node should be returned, if TRUE, the indices will appear after the overall performance measures 
                          div.logical = TRUE,     # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                          div.print   = 25,
                          beta.h.F    = 1,
                           ...)
   
 { # Start function
   
   # required packages
   #require(randomForest)
   #require(e1071)
   #require(caret)
   #require(reshape)
   
   ######################################################################################
   ### STEP 1 - PERFORM CHECKS                                                        ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n", "-->  Veryfing Call", "\n", sep=""))
   
   # check class of hie.RF
   if(class(hie.RF) != "HRF")
   {stop(paste("\n" ,"In PerformanceHRF:  hie.RF should be of class HRF", "\n", sep=""))}
   
   # check per.index
   temp.vec <- c("flat.measures", "hie.F.measure") 
   
   if(length(intersect(per.index, temp.vec)) < 1)
   {stop(paste("\n", "In PerformanceHRF, at least one valid option for per.index is required", "\n", sep=""))}
   
   if(length(intersect(per.index, temp.vec)) > 0)
   {cat(paste("\n", "Performance will be evaluated using: ", per.index, "\n", sep=""))}
   
   rm(temp.vec)
  
   # check crisp.rule
   
   temp.vec <- c("multiplicative.majority", "multiplicative.permutation", "stepwise.majority") 
   
   if(length(intersect(crisp.rule, temp.vec)) < 1)
   {stop(paste("\n", "In PerformanceHRF, at least one valid option for crisp.rule is required", "\n", sep=""))}
   
   if(length(intersect(crisp.rule, temp.vec)) > 0)
   {cat(paste("\n", "Performance will be evaluated using crisp classification based on: ",crisp.rule,"\n", sep=""))}
   
   # check perm.num
   if(!is.numeric(perm.num))
   {stop(paste("\n", "In PerformanceHRF, perm.num should be a positive integer", "\n", sep=""))}
   
   if(perm.num < 1)
   {stop(paste("\n", "In PerformanceHRF, perm.num should be a positive integer", "\n", sep=""))}
   
   if(round(perm.num, 0) != perm.num)
   {stop(paste("\n", "In PerformanceHRF, perm.num should be a positive integer", "\n", sep=""))}
   
   # Check by.node
   if(!is.logical(by.node))
   {cat(paste("\n", "In PerformanceHRF, by.node should be Logical. default of by.node=TRUE is used", "\n", sep=""))
    by.node <- TRUE}
   
   # Check div.logical
   if(!is.logical(div.logical))
   {cat(paste("\n", "In PerformanceHRF, div.logical should be Logical. default of div.logical=TRUE is used", "\n", sep=""))
    div.logical <- TRUE}
   
   # check div.print
   if(!is.numeric(div.print))
   {cat(paste("\n", "In PerformanceHRF, div.print should be a positive integer", "\n",
              "Default of div.print=25 is used", "\n", sep=""))
    div.print <- 25 }
   
   if(div.print < 1)
   {cat(paste("\n", "In PerformanceHRF, div.print should be a positive integer", "\n",
              "Default of div.print=25 is used", "\n", sep=""))
    div.print <- 25 }
   
   if(round(div.print, 0) != div.print)
   {cat(paste("\n", "In PerformanceHRF, div.print should be a positive integer", "\n",
              "Default of div.print=25 is used", "\n", sep=""))
    div.print <- 25 }
   
   # check beta.h.F
   if(!is.numeric(beta.h.F))
   {cat(paste("\n", "In PerformanceHRF, beta.h.F should be a non-negative number", "\n",
              "Default of beta.h.F=1 is used", "\n", sep=""))
    beta.h.F <- 1    }
   
   if(beta.h.F < 0)
   {cat(paste("\n", "In PerformanceHRF, beta.h.F should be a non-negative number", "\n",
              "Default of beta.h.F=1 is used", "\n", sep=""))
    beta.h.F <- 1    }
   
   cat(paste("\n", "-->  Call Verified", "\n", sep=""))
  
   ######################################################################################
   ### STEP 2 - CREATE LOGICAL ARGUMENTS FOR EACH OPTION OF per.index and crisp.rule    ###
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
   
      # flat.measures
   if(!is.na(match("flat.measures", per.index))) # the flat.measures was called by the user
   { flat.measures.logical <- TRUE  }
   if(is.na(match("flat.measures", per.index))) # the flat.measures was not called by the user
   { flat.measures.logical <- FALSE }  
   
      # hie.F.measure
   if(!is.na(match("hie.F.measure", per.index))) # the hie.F.measure was called by the user
   { hie.F.measure.logical <- TRUE  }
   if(is.na(match("hie.F.measure", per.index))) # the hie.F.measure was not called by the user
   { hie.F.measure.logical <- FALSE }  
   
   # add additional logical variables here if new crisp rules and/or measurees are added
   
   
   ######################################################################################
   ### STEP 3 - ARRANGE TERMINAL NODES FOR THE TRAINING DATA                          ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n", "-->  Preparing the observed terminal nodes ", "\n", sep=""))
   
   # extract info from the hie.RF object
   cat(paste("\n", "Extracting info from hie.RF ", "\n", sep=""))
   train.data.ready  <- hie.RF$train.data.ready
   case.ID           <- hie.RF$case.ID
   hie.levels        <- hie.RF$hie.levels
   end.path.name     <- hie.RF$call$end.path.name
   if(is.null(end.path.name)){end.path.name <- "END.PATH"}
   unique.path       <- hie.RF$hier.struc$unique.path
   
   # prepare the train.data.acc data frame
   train.data.acc               <- subset(train.data.ready,select = case.ID)
   train.data.acc$obs.term.node <- NA 
   
   cat(paste("\n", "Evaluating terminal node for each case ", "\n", sep=""))
   
   # Get the terminal node of each case
   for (K2 in 1:nrow(train.data.ready))
   {focal.path      <- train.data.ready[K2, hie.levels] # the column with the hierarchical data for case k2
    focal.term.node <- GetTerminalNode(end.path.name = end.path.name,         #  Character, the name used to represent the END.PATH
                                       unique.path   = focal.path,            #  data frame, specifying all the pathes from the tree root to terminal nodes. with each path from tree root as a row,and each column as a depth in the hierarchy
                                       level.depth   = ncol(focal.path) - 2)
    
    train.data.acc[K2, "obs.term.node"] <- focal.term.node$term.node.name 
   }
   
   # remove objects used in the k2 loop
   rm(list=c("K2", "focal.path", "focal.term.node"))
   
   # order according to
   cat(paste("\n", "Ordering according to case.ID ", "\n", sep=""))
   
   train.data.acc <- train.data.acc[order(train.data.acc[, 1]), ]
   
   # all the terminal nodes found in the train.data.acc
   unique.nodes   <- unique(train.data.acc$obs.term.node)
   
   cat(paste("\n", "-->  Observed terminal nodes ready,  ", "\n", sep=""))
      
   ######################################################################################
   ### STEP 4 - CREATE THE RETURN DATA FRAME                                          ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat("######################")
   cat(paste("\n", "-->  Createing the crisp.rule by per.index data frames", "\n", sep=""))
   
   cat(paste("\n", "Identifying indices called by user", "\n", sep=""))
   ## Add the relevant measures columns                        ##
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
    
   hie.performance <-data.frame(crisp.rule     = c(NA))
   
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
    if(flat.measures.logical && hie.F.measure.logical)
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
     hie.perf.mult.perm[c(1:perm.num), "crisp.rule"] <- c(paste("perm.", 1:perm.num, sep=""))  }
   
   if(step.maj.rule.logical)
   { hie.perf.step.maj <- hie.performance
     hie.perf.step.maj[1, "crisp.rule"] <- "stepwise.majority"  }
   
   ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
   ####                                                                   ####
   ##                                                                       ##
   #    Add here addtional hie.performance data frames if new methods are added     #
   ##                                                                       ##
   ####                                                                   ####         
   ######   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ######
   
   cat(paste("\n", "-->  crisp.rule by per.index data frames created","\n", sep=""))
   cat("######################")
   
   ######################################################################################
   ### STEP 5 - PREPARING THE TERMINAL NODES AS PREDICTED BY THE HRF                  ###
   ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
   
   cat(paste("\n", "-->  Preparing the predicted terminal nodes from the hierarchical RandomForest ", "\n", sep=""))
   
   # Extract the votes, defaults are used for all other arguments 
   raw.votes   <- predict.HRF(object = hie.RF) 
   focal.votes <- raw.votes$prop.vote.train
   
   # the data frame that collects all the crisp rules
   crisp.case.class <- focal.votes[, c(1, 2)] 
   
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
       mult.perm.rule <- GetPermMultTermNode(multiplicative.prop.votes = multiplicative.prop,  # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. one of the outputs of GetMultMajRulee function. First column is the train.or.test, second column is the case.ID, 
                                             perm.num                  = perm.num,             # Integer, number of random votes to take for each case 
                                             div.logical               = div.logical,          # Logical, if TRUE progress when permutating the proportion of votes will be printed every div.print permutations
                                             div.print                 = div.print,
                                             bind.prop.perm            = FALSE)                # logical, if true, the permutated terminal nodes are added at the end of the multiplicative.prop.votes. If FALSE, a seperate data frame is returned, cointating only the case.ID and the permuted terminal nodes
       crisp.case.class <- cbind(crisp.case.class, mult.perm.rule[3:ncol(mult.perm.rule)])
       
       cat(paste("\n", perm.num," Permutations added", "\n", sep=""))
     } 
     
     multiplicative.prop <- multiplicative.prop[order(multiplicative.prop[, 2]), ]
   } # end the crisp rules that relay on multiplicative probabilites
   
   
   # Treat the stepwise majority rule
   if(step.maj.rule.logical)
   {
     cat(paste("\n", "Adding the stepwise majority Rule", "\n", sep=""))
     step.maj.rule <-  GetStepMajRule(hie.RF             = hie.RF,
                                      prop.vote          = focal.votes,
                                      bind.prop.step.maj = FALSE) 
     
     crisp.case.class <- cbind(crisp.case.class,  step.maj.rule[3])  
     cat(paste("\n", "Stepwise majority Rule added", "\n", sep=""))
   }
   
   
   # Order according to case.ID
   cat(paste("\n", "Ordering according to case.ID ", "\n", sep=""))
   
   crisp.case.class    <- crisp.case.class[order(crisp.case.class[, 2]), ]
   
   # check if case.ID of the training and predicted data match perfectly
   
   if(!all(train.data.acc[, 1] == crisp.case.class[, 2]))
   { stop(paste("\n", "Error in function: PerformanceHRF", "\n",
                "case.ID names (rows) of the training data and predicted data do not match perfectly", "\n",
                sep=""))  }
 
   cat(paste("\n", "-->  Predicted terminal nodes ready ", "\n", sep=""))
   
   
   
   
   ######################################################################################
   ### STEP 6 - ESTIMATING ACCURACY FOR EACH ROW IN hie.performance                           ###
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
     
     # remove the prefix "Class: " from the row names of the by.class table
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
       melt.byclass <- melt(conf.matr.byclass)
       colnames(melt.byclass)       <- c("term.node", "index", "value")
       melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                             melt.byclass$index,
                                             sep=";")
       for (i in 1:nrow(melt.byclass))
       { col.num                       <- match(melt.byclass$term.node.index[i], colnames(hie.perf.mult.maj))
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
         hie.perf.mult.maj[1, col.num.2] <-results.hie.F.measure[count.measure, "values"] }
       
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
       {cat(paste("\n","  Estimating performance measures for permutation number: ", count.perm , sep=""))}
       
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
       rownames(conf.matr.byclass)<-sub("Class: ", "", rownames(conf.matr.byclass))
       
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
         results.hie.F.measure <- HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                              unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                              beta.h.F    = beta.h.F,
                                              by.node     = by.node)
         
         # Store wach result in the correct place in hie.performance
         for(count.measure in 1: nrow(results.hie.F.measure))
         { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"], colnames(hie.perf.mult.perm))
           hie.perf.mult.perm[count.perm,col.num.2] <- results.hie.F.measure[count.measure, "values"] }
       } # end if Hie measures
     } # End the count.perm for loop
   } #### End treatment of the multiplicative permutation Rule
   
   
   #########################################################
   ####    treat the Stepwise Majority Rule              ###
   #########################################################
   
   if(step.maj.rule.logical)
   { #### start treatment of  the stepwise Majority Rule
     cat(paste("\n", "Estimating performance measures for the stepwise majority rule ", "\n", sep=""))
     
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
     
     # insert results to hie.performance if flat.measures.logical==TRUE
     if(flat.measures.logical)
     { # Start if Flat
       hie.perf.step.maj$Accuracy[1]       <- conf.matr.overall$Accuracy
       hie.perf.step.maj$Kappa[1]          <- conf.matr.overall$Kappa 
       hie.perf.step.maj$AccuracyLower[1]  <- conf.matr.overall$AccuracyLower 
       hie.perf.step.maj$AccuracyUpper[1]  <- conf.matr.overall$AccuracyUpper 
       hie.perf.step.maj$AccuracyNull[1]   <- conf.matr.overall$AccuracyNull 
       hie.perf.step.maj$AccuracyPValue[1] <- conf.matr.overall$AccuracyPValue 
       hie.perf.step.maj$McnemarPValue[1]  <- conf.matr.overall$McnemarPValue
     } # End if Flat
     
     # insert results to hie.performance if flat.measures.logical==TRUE for each node
     if(flat.measures.logical && by.node)
     { # Start if Flat and by.node
       melt.byclass <- melt(conf.matr.byclass)
       colnames(melt.byclass)       <- c("term.node", "index", "value")
       melt.byclass$term.node.index <- paste(melt.byclass$term.node,
                                             melt.byclass$index,
                                             sep=";")
       for (i in 1:nrow(melt.byclass))
       { col.num                       <- match(melt.byclass$term.node.index[i], colnames(hie.perf.step.maj))
         hie.perf.step.maj[1, col.num] <- melt.byclass$value[i]       }  
     } # End if Flat and by.node
     
     if(hie.F.measure.logical)
     { # Start if Hie measure
       # returns the hierarchical Precision recall and F measure for the entire confusion matrix and if by.node=TRUE, to each node as well
       results.hie.F.measure <- HieFMeasure(conf.matr   = conf.matr,       # Object of class confusion matrix, as generated by confusionMatrix of the caret package
                                            unique.path = unique.path,     # Data frame, the unique.path data frame from RunHRF
                                            beta.h.F    = beta.h.F,
                                            by.node     = by.node)
       
       # Store each result in the correct place in hie.performance
       for(count.measure in 1: nrow(results.hie.F.measure))
       { col.num.2 <- match(results.hie.F.measure[count.measure, "measure"],
                            colnames(hie.perf.step.maj))
         hie.perf.step.maj[1,col.num.2] <- results.hie.F.measure[count.measure, "values"] }
     } # End if Hie measure
   }  #### End treatment of the stepwise Majority Rule
   
   cat(paste("\n", "-->  Performance measures estimated  ", "\n", sep=""))
   cat("######################")
   
   ############################
   
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
  
   
   return.list <- list(raw.votes        = focal.votes ,       # data frame, the raw proportion votes for each node (terminal and internal) and each case, according to each local classifer
                       #train.data.acc   = train.data.acc,     # Data frame, the case.ID and observed terminal node of each case
                       crisp.case.class = crisp.case.class,    # Data frame, containing all the crisp classification of each case (as row) 
                       hie.performance  = hie.performance)
   
   if(mult.maj.rule.logical || mult.perm.logical)
   { new.list    <- list(multiplicative.prop = multiplicative.prop) # add the multiplicative proprotion of votes if requested bu any of multiplicative majority or multiplicative permutation
     return.list <- c(return.list, new.list)}
   
   #if(mult.maj.rule.logical)
   # {new.list    <- list(hie.perf.mult.maj = hie.perf.mult.maj)    # the results of the multiplicative majority performance
   #  return.list <- c(return.list, new.list)}
     
   # if(mult.perm.logical)
   # {new.list    <- list(hie.perf.mult.perm = hie.perf.mult.perm)  # the results of the multiplicative permutation performance
   #  return.list <- c(return.list, new.list)}
   
   # if(step.maj.rule.logical)
   #  {new.list    <- list(hie.perf.step.maj = hie.perf.step.maj)    # the results of the stepwise majotiy rule
   #   return.list <- c(return.list, new.list)}
   
   if(by.node)
   { new.list    <- list(nodes.measures.columns = nodes.measures.columns)  # if by.node==TRUE, additional data frame with key to columns name
     return.list <- c(return.list, new.list)}
   
   
   # add the call
   new.list    <- list(call = match.call())  # add the call
   return.list <- c(return.list, new.list)
   
   # set the new class
   # class(return.list) <- "Hier.RF.Performance"
   return(return.list)
  
  
 } # End function 

 