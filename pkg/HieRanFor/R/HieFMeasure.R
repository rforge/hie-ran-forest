#'  Hierarchical precision, hierarchical recall and hierarchical F measure
#'  
#'  This function estimate 3 measures of accuracy that accounts for the
#'  hierarchical structure of the class hierarchy. 
#'  
#'  @param conf.matr      Object of class \code{"confusionMatrix"}, as generated
#'    by \code{\link{confusionMatrix}} function of the '\code{caret}' package.
#'    
#'  @param unique.path    Data frame, the \code{unique.path} data frame from 
#'    \code{RunHRF}. Can be found in an object of class \code{"HRF"} at 
#'    \code{$hier.struc$unique.path}.
#'    
#'  @param beta.h.F       Numeric in the range \code{beta.h.F}>=0. Controls the 
#'    relative weight of Hierarchical precision and hierarchical recall in the 
#'    estimation of hierarchical F measure. Values close to 0 will give higher 
#'    weight to Hierarchical precision. Equal weights are for \code{beta.h.F}=1,
#'    while values above 1 will give higher weight to hierarchical recall. See
#'    details in \emph{Kiritchenko et al. (2005)}.
#'    
#'  @param by.node        Logical, if \code{TRUE}, the hierarchical precision,
#'    recall and F measure are estimated for each class as well.
#'    
#'  @param \dots Optional parameters to be passed to low level functions.
#'  
#'  @author Yoni Gavish <gavishyoni@@gmail.com>
#'  
#'  @details To create a confusion matric in with the \code{caret} package, the
#'    levels of the observed and predicted vectors should be identical. 
#'    Therefore, we advise using \code{\link{JoinLevels}}.  \emph{Kiritchenko
#'    et al. (2005)} estimated the hierarchical precision, recall and F
#'    measure by summing over all cases. Here, we estimate the same values
#'    directly from the confusion matrix. In addition, \emph{Kiritchenko et al.
#'    (2005)} do not provide a class specific measure of hierarchical accuracy.
#'    Here we developed one such measure. \cr 
#'    In the hierarchical measures of accuracy, not all misclassifications carry
#'    the same weight. For example, in the simple hierarchy below, correct 
#'    classification of a case from class \emph{A.1} will contribute most to the
#'    overall accuracy (2 in this case). However, misclassification of the 
#'    \emph{A.1} case to class \emph{A.2} will still contribute to the overall 
#'    accuracy since \emph{A.1} and \emph{A.2} share one node in their path 
#'    (\emph{A}). On the other hand, misclassification of the \emph{A.1} case to
#'    class \emph{B.2} will not contribute anything to the overall accuracy, 
#'    since the two terminal nodes only meet at the tree root of the class
#'    hierarchy.
#'    
#'  \if{html}{\figure{SimpleHie.jpeg}}
#'  \if{latex}{\figure{SimpleHie.jpeg}{options: width=7cm}}
#'    
#'  @references
#'   Kiritchenko, S., S. Matwin, and F. Famili. 2005. Functional annotation of 
#'    genes using hierarchical text categorization. In: \emph{Proc. of the ACL 
#'    Workshop on Linking Biological Literature, Ontologies and Databases: 
#'    Mining Biological Semantics}.
#'    
#'    @return a data frame with the name of the measure in the first column and
#'      the value in the second.
#'      
#'  @section hP, hR and hF:
#'  The hierarchical precision (\emph{hP}), recall (\emph{hR}) and F measure 
#'  (\emph{hF}) are estimated from a confusion matrix (with \emph{j} being the 
#'  observed class and \emph{k} being the predicted class) using the formulas 
#'  given below. Similarly, the hierarchical precision for predicted class 
#'  \emph{k} (\emph{hP [k]}), the hierarchical recall for observed class
#'  \emph{j} (\emph{hR [j]}), and the hierarchical F measure for class
#'  \emph{j=k} (\emph{hF [j=k]}) can also be estimated from the confusion
#'  matrix. The \code{by.node} specific measures converge to the regular flat
#'  precision and recall (respectively) by setting \emph{S [j,k] = 1} if \emph{j
#'  = k} and \emph{S [j,k] = 0} if \emph{j} differs from \emph{k}.
#'  
#'  \if{html}{\figure{Fmeasures.jpg}}
#'  \if{latex}{\figure{Fmeasures.jpg}{options: width=7cm}}
#'  
#'
#'  
#'  @examples
#' data(OliveOilHie)
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE)
#'
#' # extract the crisp class using PerformanceHRF
#' perf.hRF.OO <- PerformanceHRF(hie.RF     = hie.RF.OO,
#'                               per.index  = c("hie.F.measure"),      
#'                               crisp.rule = c("stepwise.majority"))   
#' crisp.OO <- perf.hRF.OO$crisp.case.class
#'
#' # Join factor levels of the observed and predicted 
#' joined.levels <- JoinLevels(vector.1 = crisp.OO$stepwise.majority.rule,
#'                             vector.2 = crisp.OO$obs.term.node)
#' 
#' # create the confusionMatrix object
#' library(caret)
#' conf.matr <- confusionMatrix(data      = joined.levels$vector.1,
#'                              reference = joined.levels$vector.2,
#'                              dnn       = c("Prediction", "Observed"))
#' 
#' # the HieFMeasure
#' hie.F.value <- HieFMeasure(conf.matr   = conf.matr,       
#'                            unique.path = hie.RF.OO$hier.struc$unique.path,     
#'                            beta.h.F    = 1,    
#'                            by.node     = TRUE)
#'
#' # compare to the output of PerformanceHRF
#' hie.performance <- perf.hRF.OO$hie.performance
#' 
#' @export
#'        



#  \deqn{hP =\frac{\sum _{j}\sum _{k}[{S_{j,k}\cdot F_{j,k}}]}{\sum _{k}[S_{k,k}\cdot F_{+,k}]}}{hP = sum[j] sum[k] ( S[j,k]) * f[j,k] ) / sum[k] ( S[k,k] * F[+,k] )}

 





HieFMeasure = function(conf.matr,       
                       unique.path,     
                       beta.h.F = 1,    
                       by.node  = FALSE,   
                       ...)
{ # Start function
  
 
  
  # Perform some checks
  if(!is.numeric(beta.h.F))
  {cat(paste("\n", "beta.h.F should be numeric. Defalut value of beta.h.F = 1 is used", "\n", sep=""))
   beta.h.F <- 1}
  
  if(beta.h.F < 0)
  {cat(paste("\n", "beta.h.F should be larger than 0. Defalut value of beta.h.F = 1 is used", "\n", sep=""))
   beta.h.F <- 1}
  
  if(class(conf.matr) != "confusionMatrix")
  {stop(paste("\n", "conf.matr should be of class", "\n", sep=""))}
  
  if(!is.logical(by.node))
  {cat(paste("\n", "by.node should be logical. Defalut value of by.node=FALSE is used", "\n", sep=""))
   by.node <- FALSE}
  
  
  ###############################################
  ### extract info from the confusion matrix  ###
  ###############################################
  
  conf.matr.table   <- conf.matr$table                   # the confusion table
  
  conf.matr.J.sums  <- colSums(conf.matr.table)   # The number of cases observed in each node
  conf.matr.K.sums  <- rowSums(conf.matr.table)   # the number of cases classifed as each node
  conf.matr.J.names <- colnames(conf.matr.table)         # The names of the nodes in the columns
  conf.matr.K.names <- rownames(conf.matr.table)         # the name of the nodes in the rows
  self.J <- as.data.frame(cbind(conf.matr.J.names, conf.matr.J.sums))
  self.K <- as.data.frame(cbind(conf.matr.K.names, conf.matr.K.sums))
  
  self.J$depth <- NA
  self.K$depth <- NA
  colnames(self.J)[1] <-  "node.name"
  colnames(self.J)[2] <-  "node.freq"
  colnames(self.K)[1] <-  "node.name"
  colnames(self.K)[2] <-  "node.freq"
  self.J[, 2] <- as.numeric(as.character(self.J[, 2]))
  self.K[, 2] <- as.numeric(as.character(self.K[, 2]))
  
  
  # check if the nodes in the rows and columns are the same and in the same order
  if(!all(conf.matr.J.names == conf.matr.K.names))
  {stop(paste("\n", " the names of the rows and columns in the confusion matrix do not match", "\n", sep=""))}
  
  ###############################################
  ### Estimate the shared.nodes matrix        ###
  ###############################################
  
  
  shared.nodes <- GetNumSharedNodes(unique.path       = unique.path,
                                    ordered.term.node = colnames(conf.matr.table))
  
  if(!all(colnames(shared.nodes) == colnames(conf.matr.table)) || !all(rownames(shared.nodes) == rownames(conf.matr.table)))
  {stop(paste("\n", "Cannot match the names of the conf.matr with terminal nodes in unique.path", "\n", sep=""))}
  
  
  
  
  ###############################################
  ### Arrange the return data frame           ###
  ###############################################
  
  
  hie.F.results <- data.frame(measure     = c(NA),
                              values      = c(NA))
  
  
  hie.F.results[1, 1] <- "h.precision"
  hie.F.results[2, 1] <- "h.recall"
  hie.F.results[3, 1] <- "h.F.measure" 
  
  if(by.node)
  {
    
    nodes.h.measures                 <- expand.grid(colnames(conf.matr.table), c("n.h.precision", "n.h.recall", "n.h.F.measure"))
    colnames(nodes.h.measures)       <- c("term.node", "index")
    nodes.h.measures$measure <- paste(nodes.h.measures$term.node,
                                      nodes.h.measures$index,
                                      sep=";")
    nodes.h.measures$values <- NA
    
   
    hie.F.results <- rbind(hie.F.results, nodes.h.measures[, c(3, 4)])
    #rm(nodes.h.measures)
  }
  
  
  ###############################################
  ### Calcualte hP, hR and hF                 ###
  ###############################################
  
  # multiply element by element the confusion matrix and the sahred nodes matrix
  matrix.fre.shared <- conf.matr.table * shared.nodes
  
  for (count.nodes in 1:nrow(self.J))
  {
    num.col.run <- match(self.J[count.nodes, "node.name"], colnames(shared.nodes))
    
    self.J[count.nodes, "depth"] <- shared.nodes[num.col.run, num.col.run]
    self.K[count.nodes, "depth"] <- shared.nodes[num.col.run, num.col.run]
  }
  
    
  self.J[, "F.j.all.times.S.j.j"] <- self.J[, 2] * self.J[, 3]
  self.K[, "F.all.k.times.S.k.k"] <- self.K[, 2] * self.K[, 3]
  
  sum.K.J <- sum(matrix.fre.shared)  # the numerator of the hP and hR
  sum.K   <- sum(self.K[, "F.all.k.times.S.k.k"]) 
  sum.J   <- sum(self.J[, "F.j.all.times.S.j.j"]) 
  
  hP <- sum.K.J / sum.K
  hR <- sum.K.J / sum.J
  hF <- ((beta.h.F * beta.h.F + 1) * hP * hR)/( beta.h.F * beta.h.F * hP + hR)
    
  hie.F.results[1, 2] <- hP
  hie.F.results[2, 2] <- hR
  hie.F.results[3, 2] <- hF 
  
  if(by.node)
  {
    for (count.node in 1: nrow(conf.matr.table))
    {
      focal.node <- colnames(conf.matr.table)[count.node]
      shared.row <- match(focal.node, rownames(matrix.fre.shared))
      shared.col <- match(focal.node, colnames(matrix.fre.shared))
      
      node.sum.shared.predicted <- sum(matrix.fre.shared[shared.row, ])  
      node.sum.predicted        <- self.K[shared.row, "F.all.k.times.S.k.k"]
        
      node.sum.shared.observed  <- sum(matrix.fre.shared[, shared.col]) 
      node.sum.observed         <- self.J[shared.col, "F.j.all.times.S.j.j"]
      
      NhP <- node.sum.shared.predicted / node.sum.predicted
      NhR <- node.sum.shared.observed  / node.sum.observed
      NhF <- ((beta.h.F * beta.h.F + 1) * NhP * NhR)/(beta.h.F * beta.h.F * NhP + NhR)
      
      node.prec <- paste(focal.node,
                         "n.h.precision",
                         sep=";")
      
      node.reca <- paste(focal.node,
                         "n.h.recall",
                         sep=";")
      
      node.F    <- paste(focal.node,
                         "n.h.F.measure",
                         sep=";")
      
      hie.F.results[match(node.prec, hie.F.results$measure), 2] <- NhP 
      hie.F.results[match(node.reca, hie.F.results$measure), 2] <- NhR
      hie.F.results[match(node.F, hie.F.results$measure)   , 2] <- NhF
      
    }
    
  }
    
    
  hie.F.results
} # End function