

# checks wether the columns specified in Hie_Levels contains factors

Check_Hie_Levels=function(Hie_Levels)
{
  for (count_i in 1:dim(Hie_Levels)[2]) # loop that runs on all columns 
  { X <- Hie_Levels[,count_i]
    if(!is.factor(X))
      {stop(paste("column", names(Hie_Levels)[count_i]," should contain factors" ))}  
    
    # Check that the levels have a  syntactically valid name 
    if(!all(levels(Hie_Levels[,1])==make.names(levels(Hie_Levels[,1]))))
    {
      stop(paste("At least one factor level in column", names(Hie_Levels)[count_i]," is not a syntactically valid name - \n", 
                 "A syntactically valid name cannot start with a number or the dot followed by a number.\n",
                 "Names such as 2, .2 or .1.2.4 or  .2way are not valid\n",
                 "please update the names and re-run the function \n",
                 "Consult:  ?make.names  for additional info"))
     }
  }
}