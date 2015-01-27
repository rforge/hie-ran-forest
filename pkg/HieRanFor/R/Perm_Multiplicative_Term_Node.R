

# function, takes as input the Multiplicative chain probabilities and return for each case (row) a random node as terminal node. 

Perm_Multiplicative_Term_Node = function(Multiplicative_Prop_Votes,      # Data frame, containing the Multiplicative proportion of votes for each terminal node and for each case. First column is the Train_or_test, second column is the Case_ID, 
                                         Perm_Num = 500,         # Integer, number of random votes to take for each case 
                                         Bind_Prop_Perm = TRUE,     # logical, if true, the permutated terminal nodes are added at the end of the Multiplicative_Prop_Votes. If FALSE, a seperate data frame is returned, cointating only the Train_or_test, the Case_ID and the permuted terminal nodes
                                         Div_Logical = TRUE,      # Logical, if TRUE progress of the loop will be printed every Div_Print permutations
                                         Div_Print = 25,         # integer, the
                                         ...)
{ # Start function
  
  # Arrange output data frame
  Perm_Data <- as.data.frame(matrix(nrow=nrow(Multiplicative_Prop_Votes),ncol=Perm_Num))
  colnames( Perm_Data) <- c(paste("Perm_",1:Perm_Num,sep=""))
  
  # idenitfy columns with terminal nodes
  
  Nodes_Columns = c(3:ncol(Multiplicative_Prop_Votes)) 
  
  cat(paste("\n","Starting permutations","\n",sep=""))
  # start a loop that runs on all permutations
  for(count_perm in 1:Perm_Num)
  { # start the count_perm loop
    if(Div_Logical && round(count_perm/Div_Print,0)==count_perm/Div_Print)
    {cat(paste("\n","   permutation number: ",count_perm," out of: ",Perm_Num,sep=""))}
      
   
    # a loop that runs on each case
  for (count_case in 1:nrow(Multiplicative_Prop_Votes))
  { # start the count_case loop
    Focal_Case <- Multiplicative_Prop_Votes[count_case,]
    Focal_Case_Nodes <- colnames(Focal_Case)[Nodes_Columns]
    Focal_Case_Probs <- Focal_Case[1,Nodes_Columns]
    Perm_Data[count_case,count_perm] <- sample(Focal_Case_Nodes, size=1, replace = FALSE, prob = Focal_Case_Probs)
    
  } # end the count_case loop
  } # End the count_perm loop
  
  
  if(Bind_Prop_Perm)
  {  Perm_Data <- cbind(Multiplicative_Prop_Votes,Perm_Data) }
  if(!Bind_Prop_Perm)
  {  Perm_Data <- cbind(Multiplicative_Prop_Votes[,c(1,2)],Perm_Data)
     names(Perm_Data)[1] <- names(Multiplicative_Prop_Votes)[1]
     names(Perm_Data)[2] <- names(Multiplicative_Prop_Votes)[2]
  }
  
  # return
  Perm_Data
} # End function