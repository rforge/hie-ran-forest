

# Function, compares the names of the explnaatory variables of new.data with those used when creating HRF
# Stops the calling function if the match is not perfect

CheckNewExpVar = function(hie.RF, # Object of class HRF
                          new.data,  # the new.data that is expected to be used with Predict
                          new.data.exp.var)  # vector specifying the column numbers of the explanatory variables in new.data
{
  train.exp.var <- names(hie.RF$train.data.ready)[hie.RF$exp.var]
  new.exp.var   <- names(new.data)[new.data.exp.var]
  
  obs.length       <- length(intersect(new.exp.var ,train.exp.var))
  exp.length.train <- length(train.exp.var)
  exp.length.new   <- length(new.exp.var)
  
  if(obs.length != exp.length.train || obs.length != exp.length.new)
  {stop("\n The explanatory variables in new.data do not match those of hie.RF \n Please ensure that all explanatory variables have identical names or that the column numbers are coorrect")} 
} # End Function