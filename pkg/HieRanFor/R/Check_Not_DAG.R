

#check if any of the nodes has more than one parent

Check_Not_DAG = function(Nodes_Info,
                         ...)
{
  
  if(max(table(Nodes_Info$Node_Name))!=1)
  {stop("\n The input to Unique_Path results in a Directed Acyclic Graph (DAG). \n That is, some nodes have more than one parent")}
  
}


