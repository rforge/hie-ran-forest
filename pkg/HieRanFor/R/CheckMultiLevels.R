

 #checks if any node names is used in more than one level and stops the function if true 
CheckMultiLevels = function(unique.path,
                            end.path.name = "END.PATH")
{
  unique.path <- unique.path[, 2:(dim(unique.path)[2] - 1)] # remove the root and leaves columns
  for (count.i in 1:(dim(unique.path)[2] - 1)) # loops that work on all but the last column
  {
    higher.level <- unique.path[, count.i] # takes the column count.i
    higher.level <- subset(higher.level, higher.level[] != end.path.name) # remove the end.path.name from the higher level
   
    for (count.j in count.i:(dim(unique.path)[2] - 1)) # a loop that runs on all columns from count.i+1 till the last one
    {
     lower.level <- unique.path[,count.j + 1]
     lower.level <- subset(lower.level, lower.level[] != end.path.name)
    
    if(!is.na(table(lower.level %in% higher.level)["TRUE"]))
    {stop("\n Some categories in unique.path appear on more than one level, \n please ensure that each category appears in only one level")}
    }
  }  
}
