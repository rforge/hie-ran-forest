

# for a single case, returns the Multiplicative probabilities for each path from the tree root to eterminal nodes
# returns the multiplication of all proportion of votes for Tree_Root to End_Path
Case_Multiplicative_Prob = function(Case_Props,
                                    Unique_Path_Term,
                                    ...)
{ # start function
   
 # initialize the data collect data frame
  #Unique_Path_Term <- Unique_Path_Term[!duplicated(Unique_Path_Term),]
  Unique_Path                       <- Unique_Path_Term[,1:(ncol(Unique_Path_Term)-1)]
  Case_Path_Multiplicative_Prob             <- Unique_Path_Term
  Case_Path_Multiplicative_Prob$Multiplicative_Prob <- NA
 
  for (count_1 in 1:nrow(Unique_Path)) # loop that runs on each path of Unique_Path
  { # start count_1 loop
    # identify for each path the terminal node
    # Local_Path  <- as.matrix(Unique_Path[count_1,])
    # term_node   <- Unique_Path[count_1, match(End_Path_Name, Local_Path)-1]
    
    # initialize the Multiplicative probabilities
    Multiplicative_Prob <- 1
    
    # estimate the Multiplicative probabilities
    for(count_2 in 1:ncol(Unique_Path)) # loop that runs on each node in the path 
    { # start count_2 loop
      
      # extract the local probabilites for the node
      local_prob <- Case_Props[1,as.character(Unique_Path[count_1,count_2])]
      
      # update the Multiplicative probabilites
      Multiplicative_Prob <- Multiplicative_Prob*local_prob 
    } # end count_2 loop
    # insert the name of the terminal node and the Multiplicative probabilities
    
    #Case_Path_Multiplicative_Prob$Term_Node_Name[count_1] <- as.character(term_node)
    Case_Path_Multiplicative_Prob$Multiplicative_Prob[count_1]    <- Multiplicative_Prob
    
  } # send count_1 loop
  
 if(round(sum(Case_Path_Multiplicative_Prob$Multiplicative_Prob),digits=10)!=1)
 {stop(paste("\n",
            "Error in Function: Case_Multiplicative_Prob", "\n",
            "For Case_ID= ",
            Case_Props[1,1],
            " , the sum of Multiplicative probabilities along the hierarchical tree differs from 1",
            sep=""))}

 rm(list=c("count_1",
           "count_2",
           "Multiplicative_Prob"))
 
 

 
  return(Case_Path_Multiplicative_Prob=Case_Path_Multiplicative_Prob)
} # end function: Estimate_Vote_Path