

# function, takes the output of Predict.HRF and the HRF object and estimates for each case the stepwise majority vote
# 

GetStepMajRule = function(hie.RF, # object of class HRF - the output of RunHRF
                          prop.vote,    # data frame, one of the data frame in the list returned from the function Predict.HRF. the first column (train.or.test) specifies wether the case (rows) was in the training data or in the test data. The second column is the case.ID. the other columns are the proportion of votes for each node (terminal or internal)
                          bind.prop.step.maj = TRUE,  # Logical, if TRUE, the Stepwise majortiy rule is added to the prop.vote data frame. if FALSE the Stepwise majority rule is addedonly to the first two columns of prop.votes.
                          ...)
  
{ # Start function
  
  
  # check class of hie.RF
  if(class(hie.RF)!="HRF")
  {stop(paste("\n" , "In GetStepMajRule:  hie.RF should be of class HRF" , "\n" , sep=""))}
  
  
  # check class of bind.prop.step.maj
  if(!is.logical(bind.prop.step.maj))
  {cat(paste("\n" , "In GetStepMajRule:  bind.prop.step.maj should be logical. Default or TRUE is used" , "\n" , sep=""))}
  
  
  # Extract info from hie.RF  
  unique.path   <- hie.RF$hier.struc$unique.path
  nodes.info    <- hie.RF$hier.struc$nodes.info
  lRF.info      <- hie.RF$hier.struc$lRF.info
  end.path.name <- hie.RF$call$end.path.name
  if(is.null(end.path.name)){end.path.name <- "END.PATH"}
  root.name     <- hie.RF$call$root.name
  if(is.null(root.name)){ root.name <- "TREE.ROOT"}
  
  # Explore if prop.vote contains all nodes
  nodes.name      <- c(as.vector(as.character(nodes.info$node.name)) , end.path.name)
  prop.vote.names <- colnames(prop.vote)[3:ncol(prop.vote)]
  num.intersect   <- length(intersect(nodes.name , prop.vote.names))
  if (num.intersect!=length(prop.vote.names) || num.intersect!=length(nodes.name) )
  {stop(paste("\n" , "In GetStepMajRule:  nodes in prop.vote (Predict.HRF) differ from those in hie.RF " , "\n" , sep=""))}
  
  # start the stepwise.maj data frame in which the classification will be collected
  stepwise.maj                        <- data.frame(stepwise.majority.rule = c(NA))
  stepwise.maj[1:nrow(prop.vote) , 1] <- NA
  
  # start a loop that runs on each case
  for(count.case in 1: nrow(prop.vote))
  { # Start the count.case loop
    par.name <-root.name
    
    for(count.level in 2:ncol(unique.path))
    { # Start the count.level loop
      
      # the parent and child columns from unique.path
      child.and.par <- unique.path[ , c(count.level - 1 , count.level)]
      
      # subseting the child according to par.name
      focal.nodes   <- subset(child.and.par , child.and.par[ , 1]==par.name)
      focal.nodes   <- unique(focal.nodes[ , 2])
      
      # retreiving the proportion of votes for the relevant child node and case
      focal.prop    <- prop.vote[count.case, match(focal.nodes, colnames(prop.vote)) , drop=FALSE]
      
      # selecting the node with highest proportion of votes
      par.name.temp <- colnames(focal.prop)[match(max(focal.prop) , focal.prop)]
      
      # stop if the child node is end.path.name, elseupdate the parent node
      if(par.name.temp==end.path.name)
      {break}else(par.name <- par.name.temp)
    } # End the count.level loop
    
    # insert the classifed node into the correct place and remove objects for this case
    stepwise.maj[count.case , 1] <- par.name
    rm(list=c("count.level",
              "child.and.par",
              "focal.nodes",
              "focal.prop",
              "par.name.temp")) 
  } # End the count.case loop
    
# to bind or not to bind  
if(bind.prop.step.maj)
{stepwise.maj <- cbind(prop.vote,stepwise.maj)}

if(!bind.prop.step.maj)
{stepwise.maj <- cbind(prop.vote[ , c(1 , 2)],stepwise.maj)}

# return oobject
stepwise.maj
  
} # End function