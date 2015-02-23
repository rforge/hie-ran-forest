#' Create random class hierarchy with random training and \code{new.data} cases.
#' 
#' This function uses the \code{\link{random.hierarchical.data}} function of the
#' package \code{"treemap"} to create a tree hierarchy with a given number of
#' terminal nodes and with a given number of hierarchical levels. Next, random
#' quantitative and categorical explanatory variables are added to a
#' user-defined number of training and \code{new.data} cases of each terminal
#' node.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#'  
#' @note Currently, the function can only return hierarchies for which all
#'   terminal nodes occur at the lowest level. Future versions may allow trimmed
#'   hierarchy trees.
#' @param num.term.nodes          Integer, the number of terminal nodes at the
#'   lowest level.
#'   
#' @param tree.depth              Integer, the number of levels in the class
#'   hierarchy.
#'   
#' @param cases.t.node.train      Integer, the number of training cases to
#'   create for each terminal node in the training data.
#'   
#' @param cases.t.node.new        Integer, the number of training cases to
#'   create for each terminal node in \code{new.data}.
#'   
#' @param new.data.observed       Logical, if \code{FALSE} (default)- the
#'   \code{new.data} will not contain an observed class hierarchy for each case.
#'   If \code{TRUE} an observed class hierarchy will be added to each case of
#'   \code{new.data}.
#'   
#' @param numer.exp.var           Integer, number of numeric explanatory
#'   variables.
#'   
#' @param categ.exp.var           Integer, number of categorical explanatory
#'   variables.
#'   
#' @param case.ID                 Character, the name of the column that will
#'   contain the \code{case.IDs}.
#'   
#' @param \dots  Optional parameters to be passed to low level functions.
#' 
#' @details The numeric explanatory variables are based on sampling with 
#'   replacement from a vector of integers starting with 1 and ending with a 
#'   random number between 1 and 50. The categorical explanatory variables are 
#'   based on sampling with replacement of the first 10 letters. Future version
#'   may allow greater control on the creation of explanatory variables. \cr
#'   Setting \code{cases.t.node.train = 0} or \code{cases.t.node.new = 0} will
#'   result with \code{NA} for \code{train.data} or \code{new.data},
#'   respectively.
#'   
#'   
#' @return  A list consisting of:
#' \item{train.data}{Data frame with the training dataset}
#' \item{new.data}{Data frame with the \code{new.data} dataset}
#' \item{call}{The call to \code{RandomHRF}}
#'  
#' @examples
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20,
#'                         tree.depth     = 4)
#'
#' train.data <- random.hRF$train.data
#' new.data   <- random.hRF$new.data
#' random.hRF$call$tree.depth 
#' #  example with new.data.observed =TRUE
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 15,
#'                         tree.depth     = 6,
#'                         new.data.observed = TRUE)
#' train.data <- random.hRF$train.data
#' new.data   <- random.hRF$new.data   
#'        
#'    
#' @importFrom  treemap random.hierarchical.data
#' 
#' @export
#' 
#'     


RandomHRF = function(num.term.nodes     = 25,          # the number of terminal nodes at the lowest level
                     tree.depth         = 5,           # the number of levels in the tree
                     cases.t.node.train = 15,          # Integer, number of training cases to create for each terminal node
                     cases.t.node.new   = 25,          # Integer, Number of cases per terminal node in the new.data data frame
                     new.data.observed  = FALSE,       # LOgical, if FALSE , the new dat will not contain an observed class hiearchy. If TRUE an observed clas hierarchy will be added to each case of new data
                     numer.exp.var      = 10,          # Integer, Number of numeric explanatory variables 
                     categ.exp.var      = 10,          # Integer, Number of categorical explanatory variables
                     case.ID            = "case.ID",   # character, the column name for the case.ID
                     ...)
{
  
  # require("treemap")
  
  # create the random hierarchy
  unique.path <- random.hierarchical.data(n     = num.term.nodes, 
                                          depth = tree.depth) 
  unique.path <- unique.path[ , 1:dim(unique.path)[2] - 1]  #remove numerical random number
  
  ########################################################
  ## Create random training data sets for exploring HRF ##
  ########################################################
  
  
  unique.path         <- unique.path[!duplicated(unique.path) , ]  # remove duplicates from clipboard input
  num.terminal.nodes  <- dim(unique.path)[1]                       # number of termimal nodes
  
  if(cases.t.node.train > 0){
  # number of training cases
  num.train.cells     <- num.terminal.nodes * cases.t.node.train
  
  # create random numeric and categorical data  
  train.data <- cbind(data.frame(replicate(numer.exp.var, 
                                           sample(0:round(runif(n = 1 , min = 1 , max = 50)), 
                                                  num.train.cells, 
                                                  replace = TRUE))),
                      data.frame(replicate(categ.exp.var,
                                           sample(letters[1:10],
                                                  num.train.cells,
                                                  replace = TRUE))))
  
  # arrange the column names for the variables
  colnames(train.data) <- c(paste("var" , 1:(dim(train.data)[2]) , sep=""))
  
  
  # repeat the categories cases.t.node.train times and rbind them 
  a7 <- do.call("rbind" , replicate(cases.t.node.train , unique.path , simplify = FALSE))
  
  # bind for each row of random variables a category in a similar structure as clipboard 
  train.data <- cbind(a7 , train.data)
  
  # Add a case.ID number
  train.data              <- cbind(1:dim(train.data)[1] , train.data)
  colnames(train.data)[1] <- case.ID
  } else{train.data = NA}
  
  
  ##########################################################
  ## Create random new.data for exploring HRF ##
  ##########################################################
  
  if(cases.t.node.new > 0){
  num.predict.cells     <- num.terminal.nodes * cases.t.node.new
  
  
    # new.data
  new.data <- cbind(data.frame(replicate(numer.exp.var,
                                         sample(0:round(runif(n = 1 , min = 1 , max = 50)),
                                                num.predict.cells,
                                                replace = TRUE))),
                    data.frame(replicate(categ.exp.var,
                                         sample(letters[1:10],
                                                num.predict.cells,
                                                replace = TRUE))))
  
  # arrange the column names for the variables
  colnames(new.data) <- c(paste("var" , 1:(dim(new.data)[2]) , sep=""))
  
 start.cell.ID <- dim(train.data)[1]+1
 if(is.null(dim(train.data))){start.cell.ID <- 1}
 
 if(!new.data.observed){
   
  new.data              <- cbind(start.cell.ID:(num.predict.cells + start.cell.ID - 1),
                                 new.data)
  colnames(new.data)[1] <- case.ID
  }
  
 
 if(new.data.observed){
   # new.data
   a8 <- do.call("rbind" , replicate(cases.t.node.new , unique.path , simplify = FALSE))
   
   # bind for each row of random variables a category in a similar structure as clipboard 
   new.data <- cbind(a8 , new.data)
   
   # Add a case.ID number
   new.data              <- cbind(start.cell.ID:(num.predict.cells + start.cell.ID - 1),
                                  new.data)
   colnames(new.data)[1] <- case.ID
 }
 
 } else {new.data = NA}
 
 
  ################
  
  # Return
  list(train.data = train.data,    # data frame, training data dset       
       new.data   = new.data,
       call = match.call())      # data frame, the new data set
 
} # end funtion