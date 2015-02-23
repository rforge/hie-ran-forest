

# function, takes the unique.path data frame as input and returns a square matrix, with every terminal node as a column and row. The entries in the table are the number of shared nodes in the two pathes from the two two nodes to the tree root, excluding the tree root.
GetNumSharedNodes = function(unique.path,              # Data frame, the unique.path data frame from RunHRF
                             ordered.term.node = NULL, # Logical if NULL, th order of terminal nodes in the return table will be as in unique.path. Else, the function expect a vector containing all terminal nodes for which paired shared nodes are to be calcualted
                             ...)
{ # start function
  
  
  tree.root.name <- unique.path[1, 1]  
  end.path.name  <- unique.path[1, ncol(unique.path)]
  
 
  unique.path.term<- GetTerminalNode(end.path.name = end.path.name,                  #  Character, the name used to represent the END.PATH
                                     unique.path   = unique.path,                    #  data frame, specifying all the pathes from the tree root to terminal nodes. with each path from tree root as a row,and each column as a depth in the hierarchy
                                     level.depth   = ncol(unique.path) - 2)  #  integer, specifying the first level.depth columns of unique.path that will be used when searching for terminal nodes. the defalut is all columns. Usefull for finding the terminal node of a path when the tree is not to be followed all the way to the root
                              
 # check if the vector of nodes contains only terminal nodes
  if(!is.null(ordered.term.node))
  { 
    num.shared <- length(intersect(ordered.term.node, unique.path.term$term.node.name))
    
    if (num.shared != length(ordered.term.node))
    {  stop(paste("\n", "At least some elements of ordered.term.node are not terminal nodes in unique.path", "\n", sep=""))   }
    work.nodes <- ordered.term.node
  }
 
 if(is.null(ordered.term.node))
 { work.nodes <- unique.path.term$term.node.name  }
  
work.table <- table(colnames = work.nodes, rownames = work.nodes)
work.table <- work.table[, ordered(work.nodes)]
work.table <- work.table[ordered(work.nodes), ]


# Two nested loops that cover any combination of two nodes
for (col.count in 1:ncol(work.table))
{  # start  the col.count loop
  
  focal.col    <- colnames(work.table)[col.count]
  col.path.row <- match(focal.col, unique.path.term$term.node.name)
  col.path     <- t(unique.path[col.path.row, ])
  col.path     <- subset(col.path, col.path[] != end.path.name)
  col.path     <- subset(col.path, col.path[] != tree.root.name)
  
  for (row.count in 1:nrow(work.table))
  {  # Start the row.count loop
    focal.row    <-rownames(work.table)[row.count]
    row.path.row <- match(focal.row, unique.path.term$term.node.name)
    row.path     <- t(unique.path[row.path.row, ])
    row.path     <- subset(row.path, row.path[] != end.path.name)
    row.path     <- subset(row.path, row.path[] != tree.root.name)
    
    work.table[col.count, row.count] <- length(intersect(row.path, col.path))
     
  }  # End the row.count loop 
} # End the col.count loop
  
  
work.table
} # End.Function