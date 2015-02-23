

# Function - compares the case.ID values of new.ID to those used in the hie.RF
# stops the run of the calling function if IDs overlap

CheckNewID = function(hie.RF,  # an object of class HRF
                      new.ID   # a vector/data rame that contains the new Case_IDs
                      )
  
{
  train.data.ready <- hie.RF$train.data.ready
  train.ID <- train.data.ready[, hie.RF$case.ID]
  if(length(intersect(new.ID, train.ID)) > 0)
  {
    stop("\n At least one ID in the new.data already occurs in the training data of hie.RF \n Please ensure that all case.ID are unique to a single case")
  }
  
  
}