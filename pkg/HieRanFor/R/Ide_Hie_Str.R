

Ide_Hie_Str=function(Unique_Path,                     # the data frame containing all the pathes. Each row is a path, number of column is the number of levels
                     Internal_End_Path = FALSE,       # Logical (TRUE/FALSE)- are there terminal nodes ending in levels lower than the number of columns?
                     End_Path_Name     = "End_Path",  # Character - the name used in level i+1 for terminal nodes ending in level i.
                     Root_Include      = FALSE,       # Logical (TRUE/FALSE)is the first column the tree root?
                     Root_Name         = "TREE_ROOT", # Character - name to use for the tree root
                     ...) 
  {
  
  
  ##############################################################
  ###  Performs checks for errors                            ###
  ##############################################################
  
  # Check hierrarchy
  if(dim(Unique_Path)[2]<=1)
  {stop("\n Unique_Path contains a single level when multiple levels are needed \n")}
  
  # check set 1:  check if input is in correct class
  
  if(!is.data.frame(Unique_Path))
  {stop("\n Unique_Path should be a data frame \n")}
  
  if(!is.logical(Internal_End_Path))
  {stop("\n Internal_End_Path should be TRUE/FALSE \n")}
  
  if(!is.character(End_Path_Name))
  {stop("\n End_Path_Name should be a character \n")}
  
  if(!is.logical(Root_Include))
  {stop("\n Root_Include should be TRUE/FALSE \n")}
  
  if(!is.character(Root_Name))
  {stop("\n Root_Name should be a character \n")}
  
  # check set 2:  checks related to Internal_End_Path and End_Path_Name
  
  if (Internal_End_Path==TRUE  && length(Unique_Path[Unique_Path==End_Path_Name])==0)
  {stop(paste("\n could not find End_Path_Name='",End_Path_Name,"' in Unique_Path, despite Internal_End_Path==TRUE \n",sep=""))}
  
  if (Internal_End_Path==FALSE  && length(Unique_Path[Unique_Path==End_Path_Name])!=0)  
  {stop(paste("\n Some enteries within Unique_Path are End_Path_Name='",End_Path_Name,"', despite Internal_End_Path==FALSE \n",sep=""))}
  
  # check set 3:  checks related to Root_Include and Root_Name
  
  num_root=dim(Unique_Path[Unique_Path[,1]==Root_Name,])[1]
  
  if (Root_Include==TRUE && num_root!=dim(Unique_Path)[1])
  {stop(paste("\n if Root_Include==TRUE, the first column should contain only Root_Name='",Root_Name,"' \n" ,sep=""))}
  
  if (Root_Include==FALSE && num_root==dim(Unique_Path)[1])
  {stop(paste("\n the first column contains only Root_Name='",Root_Name,"', despite Root_Include==FALSE \n",sep=""))}
  
  
  ##############################################################
  ###  Arrange the level/path data frame                     ###
  ##############################################################
  
  ### add a column for the root if it is not included in the data frame
  ### use "TREE_ROOT" as the defalut name for the root, or the user defined name
  
  if (!Root_Include)  
  {Unique_Path <- cbind(rep(Root_Name,dim(Unique_Path)[1]),
                        Unique_Path)}
  
  ### add a column for the end path 
  ### use "End_Path" as the defalut name, or the user defined name
  
  if(dim(Unique_Path)[1]==dim(Unique_Path[Unique_Path[,dim(Unique_Path)[2]]==End_Path_Name,])[1])
  {cat(paste("\n the last coulmn do not contain any category other than End_Path_Name='",End_Path_Name,"', and is not counted as a hierarchical level \n",sep=""))} else 
  {Unique_Path <-cbind(Unique_Path,rep(End_Path_Name,dim(Unique_Path)[1]))}
  
  ### arrange Col names as "L0", "L1", "L2"....
  colnames(Unique_Path) <- c(paste("L",0:(dim(Unique_Path)[2]-1),sep=""))
  
  # remove duplicate pathes if any exist
  Unique_Path <- Unique_Path[!duplicated(Unique_Path),]
  
  # checks if any node names is used in more than one level and stops the function if true 
  Check_Multi_Levels(Unique_Path,End_Path_Name=End_Path_Name)
  
  # sort Unique_Path
  Unique_Path <- Sort_Unique_Path(Unique_Path)
  
  # number of rows and columns in Unique_Path
  R_Unique_Path <- dim(Unique_Path)[1]
  C_Unique_Path <- dim(Unique_Path)[2]
    
  ################################################################
  ### Initialize the LRF_Info and the Nodes_Info data frames   ###
  ################################################################
  
  # initialize the LRF_Info data frame
  LRF_Info <- data.frame(Classifier_ID = c(NA),    # the ID of the local RF classifier
                         Par_Level = c(NA),         # The level of the parent node in the cklassification
                         Par_Name = c(NA),          # The name of the node in the parent node
                         Par_Clas_ID = c(NA),       # The classifier_ID that separate the parent node from its siblings
                         Num_Sib_Tot = c(NA),       # number of categories in the current RF classifier
                         Num_Sib_Ter = c(NA),       # number of sibling that are terminal nodes
                         Num_Sib_Int = c(NA)        # Number of siblings that are internal nodes
                        )
  
  # initialize the Nodes_Info data frame
  Nodes_Info <- data.frame(Node_Name = c(NA),       # the name of the node
                           Node_Level=c(NA),         # The level of the node in the classification
                           Node_Freq =c(NA),         # The frequency of the node
                           Node_Par_Lev= c(NA),      # the level of the node's parent  
                           Node_Par_Name =c(NA),     # the name of the node's Parent
                           Term_Int_node =c(NA),     # is the node terminal or internal?
                           Clas_yes_no=c(NA),        # is the node a parent of a classification
                           Classifier_ID=c(NA),      # the Classifier_ID of the classifier
                           Classified_In=c(NA),      # the Classifier_ID in which the node is a child or the closest descendent
                           Lev_Above_Clas_In=c(NA)   # The number of levels above in which the node was clasifed- minimum of 1    
                          )
  
  
  
  ##############################################################
  ### Run the LRF_Info loop                                  ###
  ##############################################################
  
  # initialize counter that counts the number of local classifers
  # the counter is updated everytime a new local classifer is added to the LRF_Info table
  count_clas <- 1
  
  # Start the loop that identifies the hierarchical structure
  
  for (i in 1:(C_Unique_Path-1)) # run on each colum that contains parent nodes
  {
    # subset the parent and child columns from unique_path
    Focal_Columns <- Unique_Path[,i:(i+1)]
    Parent_fre    <- table(droplevels(Focal_Columns[,1]))
    
    # Loop works separately on each parent node
    for (j in 1:length(Parent_fre))
    { # subset the relevant parent and childs
      Focal_Parent <- subset(Focal_Columns,Focal_Columns[,1]==names(Parent_fre)[j])
      
      # the sibling for this classification
      Child_fre <- table(droplevels(Focal_Parent[,2]) )
      
      # conditions to identify an internal node that requires a local classifier
      # name of the classifier is not End_Path_Name, 
      # the parent appears more than one time 
      # and has more than 1 child
      if(names(Parent_fre)[j] != End_Path_Name && Parent_fre[j]>1 && length(Child_fre)>1) 
      {
        # create the ID for the local classifier
        LRF_Info[count_clas,1] <- paste("C_",count_clas,sep="") 
        
        # level of the parent node
        LRF_Info[count_clas,2] <- i-1
        
        # Name of the parent node
        LRF_Info[count_clas,3] <- names(Parent_fre)[j]
        
        # number of siblings
        LRF_Info[count_clas,5] <- length(Child_fre)
        
        #update the classifier_ID counter
        count_clas <- count_clas+1      
      }   
    }  
  }
  
  # Remove some objects
  rm(list=c("Focal_Columns","Focal_Parent","i","j","count_clas","Parent_fre"))
  ##############################################################
  ### Run the Node_Info loop                                 ###
  ##############################################################
  
  # add to Node_Info the values for the root node 
  
  Nodes_Info[1,1]  <- Root_Name
  Nodes_Info[1,2]  <- 0
  Nodes_Info[1,3]  <- R_Unique_Path
  Nodes_Info[1,4]  <- -1
  Nodes_Info[1,5]  <- c("No_Parent")
  Nodes_Info[1,6]  <- c("Int_Node")
  Nodes_Info[1,7]  <- c("Yes")
  Nodes_Info[1,8]  <- LRF_Info$Classifier_ID[1]
  Nodes_Info[1,9]  <- c(NA)
  Nodes_Info[1,10] <- c(NA)
  
  # initialize counter tat counts the number of nodes that are not End_Path_Names
  # the counter is updated everytime a new node is identified
  count_node <- 2
 
  # Start the loop that identifies the hierarchical structure
  for (i in 1:(C_Unique_Path-2))
  {
    # subset the parent and child columns from unique_path
    work_data  <- Unique_Path[,i:(i+1)]
    Parent_fre <- table(droplevels(work_data[,1]))
    
    # Loop works separately on each parent node
    for (j in 1:length(Parent_fre))
    {
     
      # subset the relevant parent and childs
      work_data_2 <- subset(work_data,work_data[,1]==names(Parent_fre)[j])
      
      # the sibling for this classification
      Child_fre   <- table(droplevels(work_data_2[,2]) )
      
      # conditions to identify an internal node 
      # name of the classifier is not End_Path_Name, 
      # and 
      # (the parent appears more than one time 
      # or 
      # the parent appears one time, but its only child is not End_Path_Name)
      
      if(names(Parent_fre)[j] != End_Path_Name && (Parent_fre[j]>1 || (Parent_fre[j]==1 && work_data_2!=End_Path_Name )))    
      {
        # start a loop that follow every child node
        for (count_1 in 1:length(Child_fre))
        {
          if(names(Child_fre)[count_1]!=End_Path_Name)
          {
          #Node name
          Nodes_Info[count_node,1] <- names(Child_fre)[count_1]
          
          # Node level
          Nodes_Info[count_node,2] <- i
          
          # Node Frequancy in Unique_Path
          Nodes_Info[count_node,3] <- (Child_fre)[count_1]
          
          # Parent Level
          Nodes_Info[count_node,4] <- i-1
          
          # Parent Name
          Nodes_Info[count_node,5] <- names(Parent_fre)[j]
          
          # Update the node number counter
          count_node <- count_node+1
          }
        }           
      }   
    
      
    }  
  } # end the loop that identifies the hierarchical structure
  
  # remove some objects
  rm(list=c("Child_fre","count_1","i","j","count_node","Parent_fre","work_data","work_data_2"))
  
  # Terminal nodes loop
  Parent_names <- Nodes_Info$Node_Par_Name
  
  for(k2 in 2:dim(Nodes_Info)[1]) # loop that runs on all nodes except the root node
  { # Start Loop K2
    Focal_Name <- Nodes_Info$Node_Name[k2]
    
    # if the node is not a prent of any other node - it is a terminal node
    if(length(subset(Parent_names,Parent_names==Focal_Name))==0)
         {Nodes_Info[k2,6] <- c("Term_Node")} 
    else {Nodes_Info[k2,6] <- c("Int_Node") }
    
    # Local Classifier yer or no
    if      (Nodes_Info[k2,6]=="Term_Node") # if it is a terminal node, it is not a parentof a local classifer
              { Nodes_Info[k2,7] <- c("No")
                Nodes_Info[k2,8] <- c(NA)   }
    else if (length(intersect(Nodes_Info[k2,1],LRF_Info$Par_Name))==0) # an internal node, with one child  
              { Nodes_Info[k2,7] <- c("No")
                Nodes_Info[k2,8] <- c(NA)   }
    else
              { Nodes_Info[k2,7] <- c("Yes")
                Nodes_Info[k2,8] <- subset(LRF_Info,LRF_Info$Par_Name==Nodes_Info[k2,1])[1,1]}  
    
    # Identify the local classifier ID for which the current parent is a child
    Nodes_Info[k2,9] <- subset(LRF_Info,LRF_Info$Par_Name==Nodes_Info[k2,5])[1,1]
    
    # Deal with parents that have no siblings + update the number of levels above 
    if(is.na(Nodes_Info[k2,9]))
        { Clos_LC           <- subset(Nodes_Info,Nodes_Info$Node_Name==Nodes_Info[k2,5])
          Nodes_Info[k2,9]  <- Clos_LC[1,9]
          Nodes_Info[k2,10] <- Clos_LC[1,10]+1  }
    else
        { Nodes_Info[k2,10] <- 1}
  } # End Loop K2
  
  # remove some objects
  rm(list=c("Parent_names","k2","Focal_Name")) 
     
  ###############################################################
  ### Update the Par_Clas_ID and siblings numbers in Clas_ID  ###
  ###############################################################
   
  for (count_2 in 1:dim(LRF_Info)[1])
     { # start count_2 loop
     LRF_Info[count_2,4]             <- Nodes_Info[match(LRF_Info[,1],Nodes_Info[,8])[count_2],9]
     Focal_nodes                     <- subset(Nodes_Info,Nodes_Info$Node_Par_Name==LRF_Info[count_2,"Par_Name"])
     LRF_Info[count_2,"Num_Sib_Ter"] <- dim(subset(Focal_nodes,Focal_nodes$Term_Int_node=="Term_Node"))[1]
     LRF_Info[count_2,"Num_Sib_Int"] <- dim(subset(Focal_nodes,Focal_nodes$Term_Int_node=="Int_Node"))[1]
     
     # check for consistancy
     if(LRF_Info[count_2,"Num_Sib_Ter"]+LRF_Info[count_2,"Num_Sib_Int"]!=LRF_Info[count_2,"Num_Sib_Tot"])
     {cat(paste("\nInconsistancy in siblings' numbers for classifer: ",LRF_Info[count_2,1],sep="" ))}
     } # end count_2 loop
     
  # remove some objects
  rm(list=c("count_2","Focal_nodes","C_Unique_Path","R_Unique_Path"))


  ##############################################################
  ### Check if Directed Acyclic Graph                        ###
  ##############################################################
     
  Check_Not_DAG(Nodes_Info)
     
     
  ##############################################################
  ### Print some info                                        ###
  ##############################################################
     
  cat(paste("\n","Found a total of ",dim(Unique_Path)[1]," Unique pathes from the tree root to terminal leaves", "\n",sep=""))
     
  cat(paste("\n",dim(LRF_Info)[1]," local classifiers are required", "\n",sep=""))
       
  ##############################################################
  ### Create the return list                                 ###
  ##############################################################
     
  list(LRF_Info          = LRF_Info,      # The data frame for each local random forst classifer
       Nodes_Info        = Nodes_Info,    # THe data frame for each internal and terminal node
       Unique_Path       = Unique_Path,   # the data frame containing all the pathes. Each row is a path, number of column is the number of levels
       Internal_End_Path = FALSE,         # Logical (TRUE/FALSE)- are there terminal nodes ending in levels lower than the number of columns?
       End_Path_Name     = "End_Path",    # Character - the name used in level i+1 for terminal nodes ending in level i.
       Root_Include      = FALSE,         # Logical (TRUE/FALSE)is the first column the tree root?
       Root_Name         = "TREE_ROOT",   # Character - name to use for the tree root
       call              = match.call()   # the call
       )
            
} # end function 

