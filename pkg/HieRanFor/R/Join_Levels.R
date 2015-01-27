
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