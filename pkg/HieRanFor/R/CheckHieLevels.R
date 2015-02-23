

# checks wether the columns specified in hie.levels contains factors

CheckHieLevels = function(hie.levels)
{
  for (count.i in 1:dim(hie.levels)[2]) # loop that runs on all columns 
  { X <- hie.levels[, count.i]
    if(!is.factor(X))
      {stop(paste("column", names(hie.levels)[count.i]," should contain factors" ))}  
    
    # Check that the levels have a  syntactically valid name 
    if(!all(levels(hie.levels[, 1]) == make.names(levels(hie.levels[, 1]))))
    {
      stop(paste("At least one factor level in column", names(hie.levels)[count.i]," is not a syntactically valid name - \n", 
                 "A syntactically valid name cannot start with a number or the dot followed by a number.\n",
                 "Names such as 2, .2 or .1.2.4 or  .2way are not valid\n",
                 "please update the names and re-run the function \n",
                 "Consult:  ?make.names  for additional info"))
     }
  }
}