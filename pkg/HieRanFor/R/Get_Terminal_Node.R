
# adds a the terminal node name for each path of Unique_Path
# as a new column under the colname 'Term_Node_Name'
# works also 

Get_Terminal_Node = function(End_Path_Name,                  #  Character, the name used to represent the End_Path
                             Unique_Path,                    #  data frame, specifying all the pathes from the tree root to terminal nodes. with each path from tree root as a row,and each column as a depth in the hierarchy
                             Level_Depth=ncol(Unique_Path)-2,  #  integer, specifying the first level_depth columns of Unique_Path that will be used when searching for terminal nodes. the defalut is all columns. Usefull for finding the terminal node of a path when the tree is not to be followed all the way to the root
                             ...)
{
  if(Level_Depth>ncol(Unique_Path)-2)
  {cat(paste("Error in Function: Get_Terminal_node", "\n",
             "Level_Depth is larger than the depth of the tree", "\n",
             "Maximum tree depth is used","\n", sep=""))
   Level_Depth <- ncol(Unique_Path)-2}
  
  Unique_Path <- Unique_Path[,1:(Level_Depth+1)]
  Unique_Path <- Unique_Path[!duplicated(Unique_Path),]
  Unique_Path_Term <- Unique_Path
  Unique_Path_Term$Term_Node_Name <-NA 
  for (count_1 in 1:nrow(Unique_Path)) # loop that runs on each path of Unique_Path
  { # start count_1 loop
    # identify for each path the terminal node
    Local_Path  <- as.matrix(Unique_Path[count_1,])
    
    if(!is.na(match(End_Path_Name, Local_Path)) )
      {term_node   <- Unique_Path[count_1, match(End_Path_Name, Local_Path)-1]
       Unique_Path_Term$Term_Node_Name[count_1] <- as.character(term_node)}
     
    if(is.na(match(End_Path_Name, Local_Path)))
    { term_node   <-Unique_Path[count_1, ncol(Unique_Path)]
      Unique_Path_Term$Term_Node_Name[count_1] <- as.character(term_node)}
   } 
  Unique_Path_Term 
}