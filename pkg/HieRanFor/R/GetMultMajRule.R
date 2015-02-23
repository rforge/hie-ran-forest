

# Takes as input the proportion of votes for each node and for each case returns the noide that received the maximal proportion fo votes.
GetMultMajRule = function(prop.multiplicative.votes,           # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is train.or.test and the second is case.ID
                          rule.treshold     = 0,               # Numeric, the minimum distance betweeen the class that recevied the hiest proptoion of votes and the class that came second, if thedistance is lower than the treshold, the case will be classifed as class.if.under 
                          class.if.under    = "unclassified",  # the name of the class to use if the difference is lower than the treshold
                          bind.prop.mul.maj = TRUE,            # Logical, if TRUE the multiplicative majority rule is added to prop.multiplicative.votes. If FALSE, a data frame is returned containing the first to columns of prop.multiplicative.votes (train.or.test and case.ID) and the multiplicative majority rule 
                          ...)
{
  prop.multiplicative.votes.maj                              <- prop.multiplicative.votes
  prop.multiplicative.votes.maj$multiplicative.majority.rule <- NA
  
  
  for (count.case in 1:nrow(prop.multiplicative.votes.maj))
  {
    local.case      <- prop.multiplicative.votes.maj[count.case, ] # The local case
    sort.local.case <- sort(local.case[, 3:(ncol(local.case) - 1)],
                            decreasing = TRUE)
    delta.1.2       <- sort.local.case[1] - sort.local.case[2]
    
    if(delta.1.2 >= rule.treshold)
    { prop.multiplicative.votes.maj$multiplicative.majority.rule[count.case] <- colnames(local.case)[match(sort.local.case[1],
                                                                                                           local.case[1, 3:(ncol(local.case) - 1)]) +2 ]}
    
    if(delta.1.2 < rule.treshold)
    {prop.multiplicative.votes.maj$multiplicative.majority.rule[count.case] <- class.if.under }
  }
 
  if(!bind.prop.mul.maj)
  {
    col.add <- match("multiplicative.majority.rule", colnames(prop.multiplicative.votes.maj))
    prop.multiplicative.votes.maj <- prop.multiplicative.votes.maj[, c(1, 2, col.add)]
  }
  
  prop.multiplicative.votes.maj
}