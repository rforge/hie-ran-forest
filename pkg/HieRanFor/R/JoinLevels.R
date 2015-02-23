#' Combines the factor levels of two input vectors
#' 
#' This function takes two vectors, combines their factor levels, and return a 
#' list with the same vectors, only with identical and full factor levels. The 
#' order of the factors is first all the factors of vector.1 then the added 
#' factor levels of vector.2. As \code{randomForest} cannot predict if any input
#' variables of a \code{new.data} contains factor levels that were not included
#' in the training data, we recommend running this function on each categorical
#' input variable of the training and \code{new.data} before running the
#' hierarchical randomForest with \code{\link{RunHRF}}.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#'   
#' @param vector.1     The first vector.
#' @param vector.2     The second vector.
#' @param \dots  Optional parameters to be passed to low level functions.
#'   
#' @return A list with two vectors, arranged according to the input order
#' @examples
#' # create two vectors
#' vec.1 <- as.factor(rep(c("a","b","c"),10))
#' vec.2 <- as.factor(rep(c("g","a","f","b"),20))
#' 
#' # run the function
#' new.Vec <- JoinLevels(vec.1,vec.2)
#' 
#' # Re-assign to the original vectors
#' vec.1 <- new.Vec[[1]]
#' vec.2 <- new.Vec[[2]]
#' 
#' # note the levels
#' levels(vec.1)
#' levels(vec.2)
#' 
#' @export
#' 
#' 

# takes as input two vecotrs, turn them into factors, add to each one the levels that are not found in the other and sort the levels to be in the same order. Returns the two modified vectors
JoinLevels = function(vector.1,  # the first vector,  order will be kept in the return list
                      vector.2,  # the second vector, order will be kept in the return list
                      ...)       # not currenly used
{ # Start function
  # change to factors
  work.1 <- as.factor(vector.1)
  work.2 <- as.factor(vector.2)
  
  # Extract the levels
  work.1.levels <- attributes(work.1)$levels
  work.2.levels <- attributes(work.2)$levels
  
  # the levels to add to each vector
  work.1.add <- work.2.levels[!(work.2.levels %in% work.1.levels)]
  work.2.add <- work.1.levels[!(work.1.levels %in% work.2.levels)]
  
  # add the levels to working vectors
  work.1 <- factor(work.1, levels=c(levels(work.1), work.1.add))
  work.2 <- factor(work.2, levels=c(levels(work.2), work.2.add))
  
  # sort the levels on both vectors
  work.1 <- factor(work.1, levels=sort(levels(work.1)))
  work.2 <- factor(work.2, levels=sort(levels(work.2)))
  
  # Check work.1
  if(!all(as.character(work.1) == as.character(vector.1)))
  {  stop(paste("\n", "Error in JoinLevels:", "\n",
                "Unable to join the levels of the two vecotrs without changing the orignial data of vector.1", "\n",
                sep=""))  }
  
  # Check work.2
  if(!all(as.character(work.2) == as.character(vector.2)))
  {  stop(paste("\n", "Error in JoinLevels:", "\n",
                "Unable to join the levels of the two vecotrs without changing the orignial data of vector.2", "\n",
                sep=""))  }
  
  # return the vectors
  return.list <- list(vector.1 = work.1,
                      vector.2 = work.2)
  
} # End function