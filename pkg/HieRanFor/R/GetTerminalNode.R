
# adds a the terminal node name for each path of unique.path
# as a new column under the colname 'term.node.name'
# works also 

GetTerminalNode = function(end.path.name,                        #  Character, the name used to represent the end.path
                           unique.path,                          #  data frame, specifying all the pathes from the tree.root to terminal nodes. with each path from tree root as a row, and each column as a depth in the hierarchy
                           level.depth = ncol(unique.path) - 2,  #  integer, specifying the first level.depth columns of unique.path that will be used when searching for terminal nodes. the defalut is all columns. Usefull for finding the terminal node of a path when the tree is not to be followed all the way to the root
                           ...)
{
  if(level.depth > ncol(unique.path) - 2)
  {cat(paste("Error in Function: GetTerminalNode", "\n",
             "level.depth is larger than the depth of the tree", "\n",
             "Maximum tree depth is used", "\n", sep=""))
   level.depth <- ncol(unique.path) - 2}
  
  unique.path                     <- unique.path[, 1:(level.depth + 1)]
  unique.path                     <- unique.path[!duplicated(unique.path), ]
  unique.path.term                <- unique.path
  unique.path.term$term.node.name <- NA 
  
  for (count.1 in 1:nrow(unique.path)) # loop that runs on each path of unique.path
  { # start count.1 loop
    # identify for each path the terminal node
    local.path  <- as.matrix(unique.path[count.1, ])
    
    if(!is.na(match(end.path.name, local.path)) )
      {term.node                                <- unique.path[count.1, match(end.path.name, local.path) - 1]
       unique.path.term$term.node.name[count.1] <- as.character(term.node)}
     
    if(is.na(match(end.path.name, local.path)))
      {term.node                                <- unique.path[count.1, ncol(unique.path)]
       unique.path.term$term.node.name[count.1] <- as.character(term.node)}
   } 
  unique.path.term 
}