#' Combines the factor levels of two input vectors
#' 
#' This function takes two vectors, combines thier factor levels, and return a 
#' list with the same vectors, only with identical and full factor levels. The 
#' order of the factors is first all the factors of Vector_1 then the added 
#' factor levels of Vector_2. As randomForest canot predict if any input
#' variables of a new data contains factor levels that were not included in the
#' training data, we recomend running this function on each categorical input
#' variable of the training and New_Data before running the hierarchical
#' randomForest.
#' 
#' @author Yoni Gavish <gavishyoni@@gmail.com>
#'   
#' @param Vector_1     The first vector.
#' @param Vector_2     The second vector.
#'   
#' @return A list with two vectors, aranged according to the input order
#' @examples
#' # create two vectors
#' vec.1 <- as.factor(rep(c("a","b","c"),10))
#' vec.2 <- as.factor(rep(c("g","a","f","b"),20))
#' 
#' # run the function
#' new.Vec <- Join_Levels(vec.1,vec.2)
#' 
#' # Re-assign to the original vecotrs
#' vec.1 <- new.Vec[[1]]
#' vec.2 <- new.Vec[[2]]
#' 
#' # note the levels
#' levels(vec.1)
#' levels(vec.2)
#' 
#' 

# takes as input two vecotrs, turn them into factors, add to each one the levels that are not found in the other and sort the levels to be in the same order. Returns the two modified vectors
Join_Levels = function(Vector_1,  # the first vector,  order will be kept in the return list
                       Vector_2,  # the second vector, order will be kept in the return list
                       ...)       # not currenly used
{ # Start function
  # change to factors
  Work_1 <- as.factor(Vector_1)
  Work_2 <- as.factor(Vector_2)
  
  # Extract the levels
  Work_1_Levels <- attributes(Work_1)$levels
  Work_2_Levels <- attributes(Work_2)$levels
  
  # the levels to add to each vector
  Work_1_Add <- Work_2_Levels[!(Work_2_Levels %in% Work_1_Levels)]
  Work_2_Add <- Work_1_Levels[!(Work_1_Levels %in% Work_2_Levels)]
  
  # add the levels to working vectors
  Work_1 <- factor(Work_1, levels=c(levels(Work_1), Work_1_Add))
  Work_2 <- factor(Work_2, levels=c(levels(Work_2), Work_2_Add))
  
  # sort the levels on both vectors
  Work_1 <- factor(Work_1, levels=sort(levels(Work_1)))
  Work_2 <- factor(Work_2, levels=sort(levels(Work_2)))
  
  # Check Work_1
  if(!all(as.character(Work_1)==as.character(Vector_1)))
  {  stop(paste("\n","Error in Join_Levels:", "\n",
                "Unable to join the levels of the two vecotrs without changing the orignial data of Vector_1", "\n",
                sep=""))  }
  
  # Check Work_2
  if(!all(as.character(Work_2)==as.character(Vector_2)))
  {  stop(paste("\n","Error in Join_Levels:", "\n",
                "Unable to join the levels of the two vecotrs without changing the orignial data of Vector_2", "\n",
                sep=""))  }
  
  # return the vectors
  Return_List <- list(Vector_1 = Work_1,
                      Vector_2 = Work_2)
  
} # End function