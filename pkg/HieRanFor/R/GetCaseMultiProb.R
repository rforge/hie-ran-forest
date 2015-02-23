

# for a single case, returns the Multiplicative probabilities for each path from the tree root to terminal nodes
# returns the multiplication of all proportion of votes for TREE.ROOT to END.PATH
GetCaseMultiProb = function(case.props,
                            unique.path.term,
                            ...)
{ # start function
   
 # initialize the data collect data frame
  #unique.path.term <- unique.path.term[!duplicated(unique.path.term),]
  unique.path                                       <- unique.path.term[, 1:(ncol(unique.path.term) - 1)]
  case.path.multiplicative.prob                     <- unique.path.term
  case.path.multiplicative.prob$multiplicative.prob <- NA
 
  for (count.1 in 1:nrow(unique.path)) # loop that runs on each path of unique.path
  { # start count.1 loop
    # identify for each path the terminal node
    # local.path  <- as.matrix(unique.path[count.1, ])
    # term.node   <- unique.path[count.1, match(end.path.name, local.path) - 1]
    
    # initialize the Multiplicative probabilities
    multiplicative.prob <- 1
    
    # estimate the Multiplicative probabilities
    for(count.2 in 1:ncol(unique.path)) # loop that runs on each node in the path 
    { # start count.2 loop
      
      # extract the local probabilites for the node
      local.prob <- case.props[1, as.character(unique.path[count.1, count.2])]
      
      # update the Multiplicative probabilites
      multiplicative.prob <- multiplicative.prob * local.prob 
    } # end count.2 loop
    # insert the name of the terminal node and the Multiplicative probabilities
    
    #case.path.multiplicative.prob$term.node.name[count.1] <- as.character(term.node)
    case.path.multiplicative.prob$multiplicative.prob[count.1] <- multiplicative.prob
    
  } # end count.1 loop
  
 if(round(sum(case.path.multiplicative.prob$multiplicative.prob), digits = 10) != 1)
 {stop(paste("\n",
            "Error in Function: GetCaseMultiProb", "\n",
            "For case.ID= ",
            case.props[1, 1],
            " , the sum of Multiplicative probabilities along the hierarchical tree differs from 1",
            sep=""))}

 rm(list = c("count.1",
             "count.2",
             "multiplicative.prob"))
 
 

 
  return(case.path.multiplicative.prob = case.path.multiplicative.prob)
} # end function