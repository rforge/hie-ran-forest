


Multiplicative_Prop_Votes = function(Prop_Vote,     # data frame, the proportion of votes that each case received for each node in each local classifier,One of the output data frames of Extract_Votes
                             Unique_Path,   # data frame, the Unique_Path data frame from Run_HRF
                             All_Levels = FALSE, # logical, if TRUE, a data frame with the predicted probabilities is returned for each level, if FALSE, only for the deepest level.
                             ...)
{ # Start function
  
  # start the Multiplicative_Prop_Full list that collects the proprtion of votes for each level 
  if(All_Levels)
  {Multiplicative_Prop_Full        <- vector("list", (ncol(Unique_Path)-2))
  names(Multiplicative_Prop_Full) <- paste ("Prop_Multiplicative_Votes", colnames(Unique_Path)[2:(ncol(Unique_Path)-1)], sep="_")
  Start_Col <- 1
  }
  
  if(!All_Levels)
  {Multiplicative_Prop_Full        <- vector("list", 1)
   names(Multiplicative_Prop_Full) <- paste ("Prop_Multiplicative_Votes", colnames(Unique_Path)[(ncol(Unique_Path)-1)], sep="_")
   Start_Col <- ncol(Unique_Path)-2
  }
  
  
  for (count_levels in Start_Col:(ncol(Unique_Path)-2)) # loops that runs on each level
  { # start the count_levels loop
    
    # Get the terminal node for each path
  cat(paste("\n Estimating multiplicative proportion of votes until level: ",colnames(Unique_Path)[1+count_levels],"\n",sep="" ))
    
  Unique_Path_Term  <- Get_Terminal_Node(End_Path_Name=Unique_Path[1,ncol(Unique_Path)],
                                         Unique_Path=Unique_Path,
                                         Level_Depth=count_levels)
  
  
  # create the Multiplicative_Prop data frame
  
  All_Cases_Multiplicative_Prop <- as.data.frame(setNames(replicate(nrow(Unique_Path_Term),numeric(0), simplify = F), Unique_Path_Term$Term_Node_Name))
  All_Cases_Multiplicative_Prop[1:nrow(Prop_Vote),] <- NA
  All_Cases_Multiplicative_Prop              <- cbind(Prop_Vote[,c(1,2)],
                                              All_Cases_Multiplicative_Prop)
  
  # start a loop that runs on each case
  for (Count_Case in 1:nrow(All_Cases_Multiplicative_Prop))
  { # start the count_case loop
    
    # get the Multiplicative probabilities
    Case_Prob <-  Case_Multiplicative_Prob(Case_Props       = Prop_Vote[Count_Case,],
                                   Unique_Path_Term = Unique_Path_Term)
  
  if(round(sum(Case_Prob$Multiplicative_Prob),10)!=1) # check that the sum is correct
  { # start the if round statment
    cat(paste("\n","Error in Function: Case_Multiplicative_Prob","\n",
              "For Case_ID= ",Prop_Vote[count_case_1,2]," in depth level= ", 
              count_levels," the sum over all terminal nodes differs from 1 (10 digits accuracy)", "\n",sep=""))
  } # end the if round statment
   
    for (Count_Term in 1:nrow(Unique_Path_Term)) # loop that runs on all terminal nodes for the case and level
    {
      Focal_Term_Col <- match(Case_Prob$Term_Node_Name[Count_Term],colnames(All_Cases_Multiplicative_Prop)[])  # th column of the focal terminal node in All_Cases_Multiplicative_Prop
      All_Cases_Multiplicative_Prop[Count_Case,Focal_Term_Col] <- Case_Prob$Multiplicative_Prob[Count_Term]
    }
  
  
  } # end the count_case loop
  
  
  if(All_Levels){
  Multiplicative_Prop_Full[[count_levels]] <-All_Cases_Multiplicative_Prop}
  
  if(!All_Levels){
    Multiplicative_Prop_Full[[1]] <-All_Cases_Multiplicative_Prop}
  
  rm(list=c("All_Cases_Multiplicative_Prop",
            "Unique_Path_Term",
            "Case_Prob",
            "Focal_Term_Col"))
  

  
  
  } # end the count_levels loop
  
  if(All_Levels){
  cat(paste("\n","Multiplicative votes for level:",colnames(Unique_Path)[2:(ncol(Unique_Path)-1)] ," are found in:",names(Multiplicative_Prop_Full),sep=" "))
  cat(paste("\n","##--##--##--##","\n",sep=""))}
  
  if(!All_Levels){
    cat(paste("\n","Multiplicative votes for level:",colnames(Unique_Path)[(ncol(Unique_Path)-1)] ," are found in:",names(Multiplicative_Prop_Full),sep=" "))
    cat(paste("\n","##--##--##--##","\n",sep=""))}
  
  
  
  Multiplicative_Prop_Full
} # End function




