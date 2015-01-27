

# function, takes the output of Extract_Votes and the Hier.Random.Forest object and estimates for each case the stepwise majority vote
# 

Stepwise_Majority_Rule = function(Hie_RF, # object of class Hier.Random.Forest - the output of Run_HRF
                                  Prop_Vote,    # data frame, one of the data frame in the list returned from the function Extract_Votes. the first column (Train_or_Test) specifies wether the case (rows) was in the training data or in the test data. The second column is the Case_ID. the other columns are the proportion of votes for each node (terminal or internal)
                                  Bind_Prop_Step_Maj = TRUE,  # Logical, if TRUE, the Stepwise majortiy rule is added to the Prop_Vote data frame. if FALSE the Stepwise majority rule is addedonly to the first two columns of Prop_Votes.
                                  ...)
  
{ # Start function
  
  
  # check class of Hie_RF
  if(class(Hie_RF)!="Hier.Random.Forest")
  {stop(paste("\n","In Stepwise_Majority_Rule:  Hie_RF should be of class Hier.Random.Forest","\n",sep=""))}
  
  
  # check class of Bind_Prop_Step_Maj
  if(!is.logical(Bind_Prop_Step_Maj))
  {cat(paste("\n","In Stepwise_Majority_Rule:  Bind_Prop_Step_Maj should be logical. Default or TRUE is used","\n",sep=""))}
  
  
  # Extract info from Hie_RF  
  Unique_Path   <- Hie_RF$Hier_Struc$Unique_Path
  Nodes_Info    <- Hie_RF$Hier_Struc$Nodes_Info
  LRF_Info      <- Hie_RF$Hier_Struc$LRF_Info
  End_Path_Name <- Hie_RF$call$End_Path_Name
  Root_Name     <- Hie_RF$call$Root_Name
  
  # Explore if Prop_Vote contains all nodes
  Nodes_Name      <- c(as.vector(as.character(Nodes_Info$Node_Name)),End_Path_Name)
  Prop_Vote_Names <- colnames(Prop_Vote)[3:ncol(Prop_Vote)]
  Num_intersect   <- length(intersect(Nodes_Name,Prop_Vote_Names))
  if (Num_intersect!=length(Prop_Vote_Names) ||Num_intersect!=length(Nodes_Name) )
  {stop(paste("\n","In Stepwise_Majority_Rule:  nodes in Prop_vote differ from those in Hie_RF ","\n",sep=""))}
  
  # start the tepwise_Majority_Rule data frame in which the classification will be collected
  Stepwise_Maj <- data.frame(Stepwise_Majority_Rule = c(NA))
  Stepwise_Maj[1:nrow(Prop_Vote),1] <- NA
  
  # start a loop that runs on each case
  for(count_case in 1: nrow(Prop_Vote))
  { # Start the count_case loop
    Par_Name <-Root_Name
    
    for(count_level in 2:ncol(Unique_Path))
    { # Start the count_level loop
      
      # the parent and child columns from Unique_Path
      Child_and_Par <- Unique_Path[,c(count_level-1,count_level)]
      
      # subseting the child according to Par_Name
      Focal_Nodes   <- subset(Child_and_Par,Child_and_Par[,1]==Par_Name)
      Focal_Nodes   <- unique(Focal_Nodes[,2])
      
      # retreiving the proportion of votes for the relevant child node and case
      Focal_Prop    <- Prop_Vote[count_case,match(Focal_Nodes,colnames(Prop_Vote)),drop=FALSE]
      
      # selecting the node with highest proportion of votes
      Par_Name_Temp <- colnames(Focal_Prop)[match(max(Focal_Prop),Focal_Prop)]
      
      # stop if the child node is End_Path_Name, elseupdate the parent node
      if(Par_Name_Temp==End_Path_Name)
      {break}else(Par_Name<-Par_Name_Temp)
    } # End the count_level loop
    
    # insert th eclassifed node into the correct place and remove objects for this case
    Stepwise_Maj[count_case,1] <- Par_Name
    rm(list=c("count_level",
              "Child_and_Par",
              "Focal_Nodes",
              "Focal_Prop",
              "Par_Name_Temp")) 
  } # End the count_case loop
    
# to bind or not to bind  
if(Bind_Prop_Step_Maj)
{Stepwise_Maj <- cbind(Prop_Vote,Stepwise_Maj)}

if(!Bind_Prop_Step_Maj)
{Stepwise_Maj <- cbind(Prop_Vote[,c(1,2)],Stepwise_Maj)}

# return oobject
Stepwise_Maj
  
} # End function