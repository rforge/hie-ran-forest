

# function, takes the Unique_Path data frame as input and returns a square matrix, with every terminal node as a column and row. The entries in the table are the number of shared nodes in the two pathes from the two two nodes to the tree root, excluding the tree root.
Num_Shared_Nodes = function(Unique_Path, # Data frame, the Unique_Path data frame from Run_HRF
                            Ordered_Term_Node = NULL, # Logical if NULL, th order of terminal nodes in the return table will be as in Unique_Path. Else, the function expect a vector containing all terminal nodes for which paired shared nodes are to be calcualted
                            ...)
{ # start function
  
  
  Tree_Root_Name <- Unique_Path[1,1]  
  End_Path_Name  <- Unique_Path[1,ncol(Unique_Path)]
  
 
  Unique_Path_Term <- Get_Terminal_Node(End_Path_Name = End_Path_Name,                  #  Character, the name used to represent the End_Path
                               Unique_Path = Unique_Path,                    #  data frame, specifying all the pathes from the tree root to terminal nodes. with each path from tree root as a row,and each column as a depth in the hierarchy
                               Level_Depth = ncol(Unique_Path)-2)  #  integer, specifying the first level_depth columns of Unique_Path that will be used when searching for terminal nodes. the defalut is all columns. Usefull for finding the terminal node of a path when the tree is not to be followed all the way to the root
                              
 # check if the vector of nodes contains only terminal nodes
  if(!is.null(Ordered_Term_Node))
  { 
    Num_Shared <- length(intersect(Ordered_Term_Node,Unique_Path_Term$Term_Node_Name))
    if (Num_Shared!=length(Ordered_Term_Node))
    {  stop(paste("\n","At leaste some elements of Ordered_Term_Node are not terminal nodes in Unique_Path","\n",sep=""))   }
    Work_Nodes <- Ordered_Term_Node
  }
 
 if(is.null(Ordered_Term_Node))
 { Work_Nodes <- Unique_Path_Term$Term_Node_Name  }
  
Work_Table <- table(colnames=Work_Nodes, rownames=Work_Nodes)
Work_Table <- Work_Table[,ordered(Work_Nodes)]
Work_Table <- Work_Table[ordered(Work_Nodes),]


# Two nested loops that cover any combination of two nodes
for (Col_Count in 1:ncol(Work_Table))
{  # start  the Col_Count loop
  

  Focal_Col <-colnames(Work_Table)[Col_Count]
  
  Col_path_row <- match(Focal_Col,Unique_Path_Term$Term_Node_Name)
  Col_Path <- t(Unique_Path[Col_path_row,])
  
  Col_Path <- subset(Col_Path,Col_Path[]!=End_Path_Name)
  Col_Path <- subset(Col_Path,Col_Path[]!=Tree_Root_Name)
  
  for (Row_Count in 1:nrow(Work_Table))
  {  # Start the Row_count loop
    Focal_Row <-rownames(Work_Table)[Row_Count]
    Row_path_row <- match( Focal_Row,Unique_Path_Term$Term_Node_Name)
    Row_Path <- t(Unique_Path[Row_path_row,])
    
    Row_Path <- subset(Row_Path,Row_Path[]!=End_Path_Name)
    Row_Path <- subset(Row_Path,Row_Path[]!=Tree_Root_Name)
    
    Work_Table[Col_Count,Row_Count] <- length(intersect(Row_Path,Col_Path))
    
    
    
  }  # End the Row_count loop
  
} # End the Col_Count loop
  
  
Work_Table
} # End_Vinction