
##### roxygen2 comments if thefunction will be published
# just had ' after each #
#  Randomly select a class from the inbag trees down the hierarchical class structure
# 
#  
# This function is for a single case. It dependes on individual trees in each
# local classifer and as such can only work when \code{RunHRF} was set to
# \code{keep.inbag} = \code{TRUE}.
# For a given case, it starts in the \code{tree.root} and randomly select one tree in 
# which the case was OOB (Out Of Bag). The predicted class of this tree is set as
# the new parent class and returned if the class is a terminal node. If the 
# class is an internal node, the next local classifer down the hierarchy from 
# the new parent node is identified and the process continues: A random tree in
# which the case is OOB is chosen and its predicted class is set as the new 
# parent node. This continues until a terminal node is reached. Over multiple
# permutations, the proportion of times in which each terminal node is selected
# will converge to the output of \code{GetMultPropVotes}.
# 
# @param hie.RF     Object of class \code{"HRF"} - the output of \code{RunHRF}.
# @param case.data  Data frame, containing the \code{case.ID} and explantory variables for the focal case.
# @param case.ID    Integer of character, specifying the number or column name in \code{case.data} that contains the \code{case.ID}.
# @param exp.var    Integer of character, specifying the numbers or columns names in \code{case.data} that contain the explanatory variables
# 
# @details
# STILL UNDER DEVELOPMENT!!! 
# This unction can be used to create a crisp classifcation that is not based on any majority rule, but rather accounts for uncertaininty in classification. 
# As such it can provide a distribution around accuracy assesments that arrise from the the multiplicative and stepwise majority rule. 
# @return classifed.as - the name of the terminal node to which the case was classified. 
#  
# @export

PermNodeForCase = function(hie.RF,                       # An object of class HRF - the output of the RunHRF function
                           case.data,                    # Data frame, containing the case.ID and explantory variables for the focal case
                           case.ID  = 1,                 # Integer of character, specifying the number or coulmn name in case.data that contains the case.ID
                           exp.var  = 2:ncol(case.data), # Integer of character, specifying the numbers or coulmn names in case.data that contain the explanatory variables                     
                           ...)
{  # Start function
  
  # require(randomForest)
  
  # Make some checks and Change from a character to numeric column number 
  #train.data.ready <-hie.RF$train.data.ready
  
  if(is.character(case.ID))
  {case.ID.cha <- case.ID
   case.ID     <- match(case.ID, names(case.data))}
  
  if(is.character(exp.var))
  {exp.var  <- match(exp.var, names(case.data))}
  
  if(class(hie.RF) != "HRF")
  {stop("hie.RF should be of class HRF --> the output of the RunHRF function")}
  
  if(nrow(case.data) != 1)
  {cat(paste("\n", "case.data should be a data frame with a single row", "\n","Data from the first row is used", sep=""))}
  
  # Check if the inbags were kept when running RunHRF
  if(!hie.RF$call$keep.inbag)
  {stop("the hie.RF was called with keep.inbag=FALSE --> cannot execute permutation ")}
  
  # extract the relevant information from hie.RF
  lRF.info          <- hie.RF$hier.struc$lRF.info     # the info data frame on each local classifer
  nodes.info        <- hie.RF$hier.struc$nodes.info   # the info data frame on each node
  all.local.RF      <- hie.RF$all.local.RF            # list containing all the local random forests. For each random forest, there is a list with two lists: the local.data and the local.RF
  classifer.in.list <- hie.RF$order.local.RF          # THe location in all.local.RF in which the data and model of each local classifer is stored
  
  # arrange the data for the focal case
  case.exp.data <- case.data[1, exp.var]
  focal.case    <- case.data[1, case.ID]
  
  # start in the Tree Root using the user defined root.name
  parent.node.name <- hie.RF$call$root.name
    
  repeat{ # start repeat 1
    
    # Get the info for the local classifer - data + RF
    local.classifer <- subset(lRF.info, lRF.info$par.name == parent.node.name)[1, 1]
    place.in.all    <- subset(classifer.in.list, classifer.in.list$classifer.ID == local.classifer)[1, 2]
    
    # extract data for the local classifer
    local.RF.obj                     <- all.local.RF[[place.in.all]]   # a list with local.data and local.RF
    local.train.case.ID              <- as.data.frame(local.RF.obj$local.data)        # list, the local dat used in local classifer i
    colnames(local.train.case.ID)[1] <- colnames(hie.RF$train.data.ready)[hie.RF$case.ID]  
    local.RF                         <- local.RF.obj$local.RF          # object of class RandomForest for local classifer i
    
    
    # explore if the focal.case is in the Training data of the local classifier
    # returns NA if not and the integer with the row number if yes
    is.in.train <- match(focal.case, local.train.case.ID[, 1])
    
    if(is.integer(is.in.train) && !is.na(is.in.train))
    { # start -->  when the the focal case is in the training set
      
      # if yes use the inbag and keep forest of predict - output -->  a vector with potential child nodes
      # predict the child node for the focal case using all trees
      predict.case <- predict(object      = local.RF,
                              newdata     = case.exp.data,
                              predict.all = TRUE)$individual
      predict.case <- as.data.frame(predict.case)
      
      # Logical vector with FALSE for  trees in which the tree was inbag and TRUe for trees in which the focal case was out-of-bag
      col.true <- local.RF$inbag[is.in.train, ] == 0
      
      # subset from predict.case the prediction of the trees in which the focal case was out-of-bag
      predict.case <- subset(predict.case, select = col.true)
      
      rm(list = c("col.true"))
    } # End   -->  when the the focal case is in the training set
    
    if(is.na(is.in.train))
    { # start -->  when the the focal case is NOT in the training set
      
      # if not use predict -output --> a vector with potential child nodes
     
      # predict the child node for the focal case using all trees
      predict.case <- predict(object      = local.RF,
                              newdata     = case.exp.data,
                              predict.all = TRUE)$individual
      predict.case <- as.data.frame(predict.case)
    } # end -->  when the the focal case is NOT in the training set
    
    
    # select one child randomly from thecalssification results of the relevant trees
    
    child.node  <- as.character(sample(size = 1, predict.case)[1, 1])  
    term.yes.no <- nodes.info[match(child.node[1], nodes.info$node.name) , "term.int.node"]
    
    
    
    
    # if the child is NOT a terminal node --> find the next parent node in line
    if(term.yes.no == "int.node")
    { # start if "int.node"
      repeat{ # start repeat 2
        
        
        classifer.yes.no <- nodes.info[match(child.node[1], nodes.info$node.name), "clas.yes.no"]
        if(classifer.yes.no == "Yes")
        {
          parent.node.name <- child.node
          break
        }
        if(classifer.yes.no == "No")     
        {
          child.node    <- nodes.info[match(child.node[1], nodes.info$node.par.name), "node.name"]
          term.yes.no.2 <- nodes.info[match(child.node[1], nodes.info$node.name), "term.int.node"]
          if(term.yes.no.2 == "term.node")
          {break}
        } 
       }# end repeat 2
    } # End if "int.node"
    
    # if the child is a terminal node --> assign the child to classifed.as and break
    
    term.yes.no <- nodes.info[match(child.node[1], nodes.info$node.name), "term.int.node"]
    
    if(term.yes.no == "term.node")
    {classifed.as<- child.node
     break}   
    
  } # End repeat 1
  
  classifed.as
  
}  # End function