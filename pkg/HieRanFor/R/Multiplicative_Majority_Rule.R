

# Takes as input the proportion of votes for each node and for each case returns the noide that received the maximal proportion fo votes.
Multiplicative_Majority_Rule = function(Prop_Multiplicative_Votes,     # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is Train_or_Test and the second is Case_ID
                                        Rule_Treshold=0,               # Numeric, the minimum distance betweeen the class that recevied the hiest proptoion of votes and the class that came second, if thedistance is lower than the treshold, the case will be classifed as Class_If_Under 
                                        Class_If_Under="Unclassified", # the name of the class to use if the difference is lower than the treshold
                                        Bind_Prop_Mul_Maj=TRUE,        # Logical, if TRUE the multiplicative majority rule is added to Prop_Multiplicative_Votes. If FALSE, a data frame is returned containing the first to columns of Prop_Multiplicative_Votes (Train_or_Test and Case_ID) and the multiplicative majority rule 
                                        ...)
{
  Prop_Multiplicative_Votes_Maj <- Prop_Multiplicative_Votes
  Prop_Multiplicative_Votes_Maj$Multiplicative_Majority_Rule <- NA
  
  
  for (Count_Case in 1:nrow(Prop_Multiplicative_Votes_Maj))
  {
    Local_case      <- Prop_Multiplicative_Votes_Maj[Count_Case,] # The local case
    Sort_Local_Case <- sort(Local_case[,3:(ncol(Local_case)-1)],
                            decreasing = TRUE)
    Delta_1_2       <- Sort_Local_Case[1] - Sort_Local_Case[2]
    
    if(Delta_1_2>=Rule_Treshold)
    { Prop_Multiplicative_Votes_Maj$Multiplicative_Majority_Rule[Count_Case] <- colnames(Local_case)[match(Sort_Local_Case[1],Local_case[1,3:(ncol(Local_case)-1)])+2]}
    
    if(Delta_1_2<Rule_Treshold)
    {Prop_Multiplicative_Votes_Maj$Multiplicative_Majority_Rule[Count_Case] <- Class_If_Under }
  }
 
  if(!Bind_Prop_Mul_Maj)
  {
    Col_Add <- match("Multiplicative_Majority_Rule",colnames(Prop_Multiplicative_Votes_Maj))
    Prop_Multiplicative_Votes_Maj <- Prop_Multiplicative_Votes_Maj[,c(1,2,Col_Add)]
  }
  
  Prop_Multiplicative_Votes_Maj
}