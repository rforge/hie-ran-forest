

# function, takes as input the Multiplicative chain probabilities and return for each case (row) a random node as terminal node. 

GetPermMultTermNode = function(multiplicative.prop.votes,   # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. First column is the train.or.test, second column is the case.ID, 
                               perm.num       = 500,        # Integer, number of random votes to take for each case 
                               bind.prop.perm = TRUE,       # logical, if true, the permutated terminal nodes are added at the end of the multiplicative.prop.votes. If FALSE, a seperate data frame is returned, cointating only the train.or.test, the case.ID and the permuted terminal nodes
                               div.logical    = TRUE,       # Logical, if TRUE progress of the loop will be printed every div.print permutations
                               div.print      = 25,         # Integer, see above
                               ...)
{ # Start function
  
  # Arrange output data frame
  perm.data           <- as.data.frame(matrix(nrow = nrow(multiplicative.prop.votes), ncol = perm.num))
  colnames(perm.data) <- c(paste("perm.", 1:perm.num, sep=""))
  
  # idenitfy columns with terminal nodes
  
  nodes.columns = c(3:ncol(multiplicative.prop.votes)) 
  
  cat(paste("\n", "Starting permutations", "\n", sep=""))
  
  # start a loop that runs on all permutations
  for(count.perm in 1:perm.num)
  { # start the count.perm loop
    if(div.logical && round(count.perm / div.print, 0) == count.perm / div.print)
    {cat(paste("\n", "   permutation number: ", count.perm, " out of: ", perm.num, sep=""))}
      
   
    # a loop that runs on each case
  for (count.case in 1:nrow(multiplicative.prop.votes))
  { # start the count.case loop
    focal.case                        <- multiplicative.prop.votes[count.case, ]
    focal.case.nodes                  <- colnames(focal.case)[nodes.columns]
    focal.case.probs                  <- focal.case[1, nodes.columns]
    perm.data[count.case, count.perm] <- sample(focal.case.nodes, 
                                                size    = 1, 
                                                replace = FALSE, 
                                                prob    = focal.case.probs)
    
  } # end the count.case loop
  } # End the count.perm loop
  
  
  if(bind.prop.perm){perm.data <- cbind(multiplicative.prop.votes, perm.data)}
  
  if(!bind.prop.perm)
    {perm.data           <- cbind(multiplicative.prop.votes[, c(1, 2)], perm.data)
     names(perm.data)[1] <- names(multiplicative.prop.votes)[1]
     names(perm.data)[2] <- names(multiplicative.prop.votes)[2]
    }
  
  # return
  perm.data
} # End function