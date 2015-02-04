#' Create a random class hierarchy tree and random training and New data thtat fits it
#' 
#' This funtion uses the \code{random.hierarchical.data} function of the package
#' \emph{"treemap"} to create a tree hierarchy with a given number of terminal
#' nodes and with a given number of hierarchical levels. Next, random quantitative and
#' categorical explanatory variables are added to a user-defined number of
#' training and new_data cases of each terminal class.
#' 
#' @note Currently, the function can only return hierarchies that end at the
#'   lowest level. Future versions may allow trimmed hierarchy trees.
#'   


Random_HRF_Data=function(Num_Term_Nodes      = 25,  # the number of terminal nodes at the lowest level
                         Tree_Depth          = 5,   # the number of levels in the tree
                         Cells_per_Cat_Train = 15,         # Integer, number of training cells to create for each terminal node
                         Num_Num_Var         = 10,           # Integer, Number of numeric explanatory variables 
                         Num_Cat_Var         = 10,           # Integer, Number of categorical explanatory variables
                         Case_ID             = "Case_ID",        # character, the column name for the Cell ID
                         cells_per_cat_new   = 25, # Integer, Number of cases per terminal node in the New_Data data frame
                         ...)
{
  
  #require("treemap")
  
  Unique_Path  <- treemap::random.hierarchical.data(n=Num_Term_Nodes,depth=Tree_Depth) # create the random hierarchy
  Unique_Path  <- Unique_Path[,1:dim(Unique_Path)[2]-1]  #remove numerical random number
  
  ########################################################
  ## Create random training data sets for exploring HRF ##
  ########################################################
  
  # remove duplicates from clipboard input
  a6 <- Unique_Path[!duplicated(Unique_Path),]
  
  # Add a cetegory code for each category
  
  #a6 <-cbind(1:dim(a6)[1],a6)
  #colnames(a6)[1]=Cat_Code
  
  # number of termimal nodes
  Num_Terminal_Nodes  <- dim(a6)[1]
  
  #number of training cells per termimal nodes
  #Cells_per_Cat       <- Cells_per_Cat
  
  
  # number of training cells
  Num_Train_Cells     <- Num_Terminal_Nodes*Cells_per_Cat_Train
  
  # create random numeric and categorical data  
  Train_Data <- cbind(data.frame(replicate(Num_Num_Var,sample(0:round(runif(n=1,min=1,max=50)),Num_Train_Cells,rep=TRUE))),
                      data.frame(replicate(Num_Cat_Var,sample(letters[1:10],Num_Train_Cells,rep=TRUE))))
  
  # arrange the column names for the variables
  colnames(Train_Data) <- c(paste("Var",1:(dim(Train_Data)[2]),sep=""))
  
  
  # repeat the categories Cells_per_Cat_Train times and rbind them 
  a6 <- do.call("rbind", replicate(Cells_per_Cat_Train, a6 , simplify = FALSE))
  
  # bind for each row of random variables a category in a similar structure as clipboard 
  Train_Data <- cbind(a6,Train_Data)
  
  # Add a cell ID number
  Train_Data              <- cbind(1:dim(Train_Data)[1],Train_Data)
  colnames(Train_Data)[1] <- Case_ID
  
  
  ##########################################################
  ## Create random predicting data sets for exploring HRF ##
  ##########################################################
  
  
  #Predict data
  #cells_per_cat_new <- cells_per_cat_new
  Num_predict_cells     <- Num_Terminal_Nodes*cells_per_cat_new
  
  
  New_Data <- cbind(data.frame(replicate(Num_Num_Var,
                                             sample(0:round(runif(n=1,min=1,max=50)),
                                                    Num_predict_cells,
                                                    rep=TRUE))),
                        data.frame(replicate(Num_Cat_Var,
                                             sample(letters[1:10],
                                                    Num_predict_cells,
                                                    rep=TRUE))))
  
  # arrange the column names for the variables
  colnames(New_Data) <- c(paste("Var",1:(dim(New_Data)[2]),sep=""))
  
  start_cell_ID <- dim(Train_Data)[1]+1
  
  New_Data              <- cbind(start_cell_ID:(Num_predict_cells+start_cell_ID-1),
                                     New_Data)
  colnames(New_Data)[1] <- Case_ID
  
  
  ################
  
  # Return
  list(Train_Data   = Train_Data,    # data frame, training data dset       
       New_Data = New_Data)  # data frame, predict data set
 
} # end funtion