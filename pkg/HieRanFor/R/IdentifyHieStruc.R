

IdentifyHieStruc = function(unique.path,                     # the data frame containing all the pathes. Each row is a path, number of column is the number of levels
                            internal.end.path = FALSE,       # Logical (TRUE/FALSE)- are there terminal nodes ending in levels lower than the number of columns?
                            end.path.name     = "END.PATH",  # Character - the name used in level i+1 for terminal nodes ending in level i.
                            root.include      = FALSE,       # Logical (TRUE/FALSE)is the first column the tree root?
                            root.name         = "TREE.ROOT", # Character - name to use for the tree root
                            ...) 
  {
  
  
  ##############################################################
  ###  Performs checks for errors                            ###
  ##############################################################
  
  # Check hierrarchy
  if(dim(unique.path)[2] <= 1)
  {stop("\n unique.path contains a single level when multiple levels are needed \n")}
  
  # check set 1:  check if input is in correct class
  
  if(!is.data.frame(unique.path))
  {stop("\n unique.path should be a data frame \n")}
  
  if(!is.logical(internal.end.path))
  {stop("\n internal.end.path should be TRUE/FALSE \n")}
  
  if(!is.character(end.path.name))
  {stop("\n end.path.name should be a character \n")}
  
  if(!is.logical(root.include))
  {stop("\n root.include should be TRUE/FALSE \n")}
  
  if(!is.character(root.name))
  {stop("\n root.name should be a character \n")}
  
  # check set 2:  checks related to internal.end.path and end.path.name
  
  if (internal.end.path == TRUE  && length(unique.path[unique.path == end.path.name]) == 0)
  {stop(paste("\n could not find end.path.name='", end.path.name, "' in unique.path, despite internal.end.path==TRUE \n", sep=""))}
  
  if (internal.end.path == FALSE  && length(unique.path[unique.path == end.path.name]) != 0)  
  {stop(paste("\n Some enteries within unique.path are end.path.name='", end.path.name, "', despite internal.end.path==FALSE \n", sep=""))}
  
  # check set 3:  checks related to root.include and root.name
  
  num.root = dim(unique.path[unique.path[, 1] == root.name, ])[1]
  
  if (root.include == TRUE && num.root != dim(unique.path)[1])
  {stop(paste("\n if root.include==TRUE, the first column should contain only root.name='", root.name, "' \n" , sep=""))}
  
  if (root.include == FALSE && num.root == dim(unique.path)[1])
  {stop(paste("\n the first column contains only root.name='", root.name, "', despite root.include==FALSE \n", sep=""))}
  
  
  ##############################################################
  ###  Arrange the level/path data frame                     ###
  ##############################################################
  
  ### add a column for the root if it is not included in the data frame
  ### use "TREE.ROOT" as the defalut name for the root, or the user defined name
  
  if (!root.include)  
  {unique.path <- cbind(rep(root.name, dim(unique.path)[1]),
                        unique.path)}
  
  ### add a column for the end path 
  ### use "END.PATH" as the defalut name, or the user defined name
  
  if(dim(unique.path)[1] == dim(unique.path[unique.path[, dim(unique.path)[2]] == end.path.name, ])[1])
  {cat(paste("\n the last coulmn do not contain any category other than end.path.name='", end.path.name, "', and is not counted as a hierarchical level \n", sep=""))} else 
  {unique.path <-cbind(unique.path, rep(end.path.name, dim(unique.path)[1]))}
  
  ### arrange Col names as "L0", "L1", "L2"....
  colnames(unique.path) <- c(paste("L", 0:(dim(unique.path)[2] - 1), sep=""))
  
  # remove duplicate pathes if any exist
  unique.path <- unique.path[!duplicated(unique.path), ]
  
  # checks if any node names is used in more than one level and stops the function if true 
  CheckMultiLevels(unique.path, end.path.name = end.path.name)
  
  # sort unique.path
  unique.path <- SortUniquePath(unique.path)
  
  # number of rows and columns in unique.path
  r.unique.path <- dim(unique.path)[1]
  c.unique.path <- dim(unique.path)[2]
    
  ################################################################
  ### Initialize the lRF.info and the nodes.info data frames   ###
  ################################################################
  
  # initialize the lRF.info data frame
  lRF.info <- data.frame(classifier.ID = c(NA),    # the ID of the local RF classifier
                         par.level     = c(NA),    # The level of the parent node in the cklassification
                         par.name      = c(NA),    # The name of the node in the parent node
                         par.clas.ID   = c(NA),    # The classifier.ID that separate the parent node from its siblings
                         num.sib.tot   = c(NA),    # number of categories in the current RF classifier
                         num.sib.ter   = c(NA),    # number of sibling that are terminal nodes
                         num.sib.int   = c(NA)     # Number of siblings that are internal nodes
                        )
  
  # initialize the nodes.info data frame
  nodes.info <- data.frame(node.name         = c(NA),  # the name of the node
                           node.level        = c(NA),  # The level of the node in the classification
                           node.freq         = c(NA),  # The frequency of the node
                           node.par.lev      = c(NA),  # the level of the node's parent  
                           node.par.name     = c(NA),  # the name of the node's Parent
                           term.int.node     = c(NA),  # is the node terminal or internal?
                           clas.yes.no       = c(NA),  # is the node a parent of a classification
                           classifier.ID     = c(NA),  # the classifier.ID of the classifier
                           classified.in     = c(NA),  # the classifier.ID in which the node is a child or the closest descendent
                           lev.above.clas.in = c(NA)   # The number of levels above in which the node was clasifed- minimum of 1    
                          )
  
  
  
  ##############################################################
  ### Run the lRF.info loop                                  ###
  ##############################################################
  
  # initialize counter that counts the number of local classifers
  # the counter is updated everytime a new local classifer is added to the lRF.info table
  count.clas <- 1
  
  # Start the loop that identifies the hierarchical structure
  
  for (i in 1:(c.unique.path - 1)) # run on each column that contains parent nodes
  {
    # subset the parent and child columns from unique.path
    focal.columns <- unique.path[, i:(i + 1)]
    parent.fre    <- table(droplevels(focal.columns[, 1]))
    
    # Loop works separately on each parent node
    for (j in 1:length(parent.fre))
    { # subset the relevant parent and childs
      focal.parent <- subset(focal.columns, focal.columns[, 1] == names(parent.fre)[j])
      
      # the sibling for this classification
      child.fre <- table(droplevels(focal.parent[, 2]))
      
      # conditions to identify an internal node that requires a local classifier
      # name of the classifier is not end.path.name, 
      # the parent appears more than one time 
      # and has more than 1 child
      if(names(parent.fre)[j] != end.path.name && parent.fre[j] > 1 && length(child.fre) > 1) 
      {
        # create the ID for the local classifier
        lRF.info[count.clas, 1] <- paste("C.", count.clas, sep="") 
        
        # level of the parent node
        lRF.info[count.clas, 2] <- i - 1
        
        # Name of the parent node
        lRF.info[count.clas, 3] <- names(parent.fre)[j]
        
        # number of siblings
        lRF.info[count.clas, 5] <- length(child.fre)
        
        #update the classifier.ID counter
        count.clas <- count.clas + 1      
      }   
    }  
  }
  
  # Remove some objects
  rm(list = c("focal.columns",
              "focal.parent",
              "i",
              "j",
              "count.clas",
              "parent.fre"))
  ##############################################################
  ### Run the nodes.info loop                                 ###
  ##############################################################
  
  # add to nodes.info the values for the root node 
  
  nodes.info[1, 1]  <- root.name
  nodes.info[1, 2]  <- 0
  nodes.info[1, 3]  <- r.unique.path
  nodes.info[1, 4]  <- -1
  nodes.info[1, 5]  <- c("no.parent")
  nodes.info[1, 6]  <- c("int.node")
  nodes.info[1, 7]  <- c("yes")
  nodes.info[1, 8]  <- lRF.info$classifier.ID[1]
  nodes.info[1, 9]  <- c(NA)
  nodes.info[1, 10] <- c(NA)
  
  # initialize counter that counts the number of nodes that are not end.path.names
  # the counter is updated everytime a new node is identified
  count.node <- 2
 
  # Start the loop that identifies the hierarchical structure
  for (i in 1:(c.unique.path - 2))
  {
    # subset the parent and child columns from unique.path
    work.data  <- unique.path[, i:(i + 1)]
    parent.fre <- table(droplevels(work.data[, 1]))
    
    # Loop works separately on each parent node
    for (j in 1:length(parent.fre))
    {
     
      # subset the relevant parent and childs
      work.data.2 <- subset(work.data, work.data[, 1] == names(parent.fre)[j])
      
      # the sibling for this classification
      child.fre   <- table(droplevels(work.data.2[, 2]))
      
      # conditions to identify an internal node 
      # name of the classifier is not end.path.name, 
      # and 
      # (the parent appears more than one time 
      # or 
      # the parent appears one time, but its only child is not end.path.name)
      
      if(names(parent.fre)[j] != end.path.name && (parent.fre[j] > 1 || (parent.fre[j] == 1 && work.data.2 != end.path.name )))    
      {
        # start a loop that follow every child node
        for (count.1 in 1:length(child.fre))
        {
          if(names(child.fre)[count.1] != end.path.name)
          {
          #Node name
          nodes.info[count.node, 1] <- names(child.fre)[count.1]
          
          # Node level
          nodes.info[count.node, 2] <- i
          
          # Node Frequancy in unique.path
          nodes.info[count.node, 3] <- (child.fre)[count.1]
          
          # Parent Level
          nodes.info[count.node, 4] <- i-1
          
          # Parent Name
          nodes.info[count.node, 5] <- names(parent.fre)[j]
          
          # Update the node number counter
          count.node <- count.node + 1
          }
        }           
      }   
    
      
    }  
  } # end the loop that identifies the hierarchical structure
  
  # remove some objects
  rm(list = c("child.fre",
              "count.1",
              "i",
              "j",
              "count.node",
              "parent.fre",
              "work.data",
              "work.data.2"))
  
  # Terminal nodes loop
  parent.names <- nodes.info$node.par.name
  
  for(k2 in 2:dim(nodes.info)[1]) # loop that runs on all nodes except the root node
  { # Start Loop K2
    focal.name <- nodes.info$node.name[k2]
    
    # if the node is not a prent of any other node - it is a terminal node
    if(length(subset(parent.names, parent.names == focal.name)) == 0)
         {nodes.info[k2, 6] <- c("term.node")} 
    else {nodes.info[k2, 6] <- c("int.node") }
    
    # Local Classifier yer or no
    if(nodes.info[k2, 6] == "term.node") # if it is a terminal node, it is not a parentof a local classifer
              { nodes.info[k2, 7] <- c("no")
                nodes.info[k2, 8] <- c(NA)} 
    else if(length(intersect(nodes.info[k2, 1], lRF.info$par.name)) == 0) # an internal node, with one child  
              { nodes.info[k2, 7] <- c("no")
                nodes.info[k2, 8] <- c(NA)}
    else
              { nodes.info[k2, 7] <- c("yes")
                nodes.info[k2, 8] <- subset(lRF.info, lRF.info$par.name == nodes.info[k2, 1])[1, 1]}  
    
    # Identify the local classifier ID for which the current parent is a child
    nodes.info[k2, 9] <- subset(lRF.info, lRF.info$par.name == nodes.info[k2, 5])[1, 1]
    
    # Deal with parents that have no siblings + update the number of levels above 
    if(is.na(nodes.info[k2, 9]))
        { clos.lc           <- subset(nodes.info, nodes.info$node.name == nodes.info[k2, 5])
          nodes.info[k2, 9]  <- clos.lc[1, 9]
          nodes.info[k2, 10] <- clos.lc[1, 10] + 1}
    else
        { nodes.info[k2, 10] <- 1}
  } # End Loop K2
  
  # remove some objects
  rm(list = c("parent.names",
              "k2",
              "focal.name")) 
     
  ###############################################################
  ### Update the par.clas.ID and siblings numbers in clas.ID  ###
  ###############################################################
   
  for (count.2 in 1:dim(lRF.info)[1])
     { # start count.2 loop
     lRF.info[count.2, 4]             <- nodes.info[match(lRF.info[, 1], nodes.info[, 8])[count.2], 9]
     focal.nodes                      <- subset(nodes.info, nodes.info$node.par.name == lRF.info[count.2, "par.name"])
     lRF.info[count.2, "num.sib.ter"] <- dim(subset(focal.nodes, focal.nodes$term.int.node == "term.node"))[1]
     lRF.info[count.2, "num.sib.int"] <- dim(subset(focal.nodes, focal.nodes$term.int.node == "int.node"))[1]
     
     # check for consistancy
     if(lRF.info[count.2, "num.sib.ter"] + lRF.info[count.2, "num.sib.int"] != lRF.info[count.2, "num.sib.tot"])
     {cat(paste("\n Inconsistancy in siblings' numbers for classifer: ", lRF.info[count.2, 1], sep="" ))}
     } # end count.2 loop
     
  # remove some objects
  rm(list=c("count.2","focal.nodes","c.unique.path","r.unique.path"))


  ##############################################################
  ### Check if Directed Acyclic Graph                        ###
  ##############################################################
     
  CheckNotDAG(nodes.info)
     
     
  ##############################################################
  ### Print some info                                        ###
  ##############################################################
     
  cat(paste("\n", "Found a total of ", dim(unique.path)[1]," Unique pathes from the tree root to terminal nodes", "\n", sep=""))
     
  cat(paste("\n", dim(lRF.info)[1], " local classifiers are required", "\n", sep=""))
       
  ##############################################################
  ### Create the return list                                 ###
  ##############################################################
     
  list(lRF.info          = lRF.info,          # The data frame for each local random forst classifer
       nodes.info        = nodes.info,        # THe data frame for each internal and terminal node
       unique.path       = unique.path,       # the data frame containing all the pathes. Each row is a path, number of column is the number of levels
       internal.end.path = internal.end.path, # Logical (TRUE/FALSE)- are there terminal nodes ending in levels lower than the number of columns?
       end.path.name     = end.path.name,     # Character - the name used in level i+1 for terminal nodes ending in level i.
       root.include      = root.include,      # Logical (TRUE/FALSE)is the first column the tree root?
       root.name         = root.name,         # Character - name to use for the tree root
       call              = match.call()       # the call
       )
            
} # end function 

