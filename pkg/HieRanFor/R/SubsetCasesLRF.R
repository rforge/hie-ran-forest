

# Takes as input the lRF.info, train.data.ready, hie.levels and exp.var and
# returns for a specifci classifer in LRF the rows of 
# train.data that are relveant to the local RandomForest classifer (local.class)

SubsetCasesLRF = function(local.class,        # Integer, specifying the row in lRF.info for which subset of train.data should be returned
                          lRF.info,           # data frame, specifying each local classifer as a row- the output of IdentifyHieStruc
                          train.data.ready,   # the data frame with the training data  
                          hie.levels,         # columns in train.data.ready that contains the information on the hierarchical levels
                          exp.var,            # columns in train.data.ready that contains the iexplanatory variables
                          ...)
{
  
  
  class.ID      <- lRF.info[local.class , 1]      # the name of the local classifer
  par.node.lev  <- lRF.info[local.class , 2]      # The level of the parent node
  par.node.name <- lRF.info[local.class , 3]      # the name of the parent node
  col.parent    <- hie.levels[1 + par.node.lev]   # the column number of train.data.ready in which the parent node appears
  col.child     <- hie.levels[2 + par.node.lev]   # the column number of train.data.ready in which the childrens nodes appear
  
  # the information on the lcoal classifier
  local.class.info      <- lRF.info[local.class , ] 
 
 # subseting the relevent cases from train.data.ready
  local.train.data      <- subset(train.data.ready , train.data.ready[ , col.parent]==par.node.name)
 
 # The local case.IDs, path.name, 
  local.train.case.ID   <- local.train.data[ , 1]
  local.train.path.name <- local.train.data[ , 2]
 
 # The coulmn from local.train.data with the child categories, drop levels is applied
 #local.train.cat       <- local.train.data[,col.child]
 
 local.train.cat       <- droplevels(local.train.data[ , col.child])
 
 # The coulmn from local.train.data with the explanatory varaibles, 
 # drop levels is not applied to allow predictions for all levels of factors that are found within the training set
 local.train.vars  <- local.train.data[ , exp.var]
 
 # local.train.vars      <- droplevels(local.train.data[,exp.var])
  
 #  the list to return
  list(local.class.info      = local.class.info,         # all the information on the local clasifier
       local.train.data      = local.train.data,         # all the cases in train.data.ready that are classified in the local classifer
       local.train.case.ID   = local.train.case.ID,      # The case.ID of the cases in local.train.data
       local.train.path.name = local.train.path.name,    # The path.name of the cases in local.train.data
       local.train.cat       = local.train.cat,          # The child categories of all the cases in local.train.data
       local.train.vars      = local.train.vars)         # THe explantory vartiabled for all cases in local.train.data
       
} # end function 


                      
