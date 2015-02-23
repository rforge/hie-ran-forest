

#check if any of the nodes has more than one parent

CheckNotDAG = function(nodes.info,
                       ...)
{
  
  if(max(table(nodes.info$node.name)) != 1)
  {stop("\n The input to Unique_Path results in a Directed Acyclic Graph (DAG). \n That is, some nodes have more than one parent")}
  
}


