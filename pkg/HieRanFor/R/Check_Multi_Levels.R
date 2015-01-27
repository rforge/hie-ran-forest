

 #checks if any node names is used in more than one level and stops the function if true 
Check_Multi_Levels=function(Unique_Path,End_Path_Name = "End_Path")
{
  Unique_Path <- Unique_Path[,2:(dim(Unique_Path)[2]-1)] # remove the root and leaves columns
  for (count_i in 1:(dim(Unique_Path)[2]-1)) # loops that work on all but the last column
  {
    Higher_Level <- Unique_Path[,count_i] # takes the column count_i
    Higher_Level <- subset(Higher_Level,Higher_Level[]!=End_Path_Name) # remove the End_Path_Name from the higher level
   
    for (count_j in count_i:(dim(Unique_Path)[2]-1)) # a loop that runs on all columns from count_i+1 till the last one
    {
     Lower_Level <- Unique_Path[,count_j+1]
     Lower_Level <- subset(Lower_Level,Lower_Level[]!=End_Path_Name)
    
    if(!is.na(table(Lower_Level %in% Higher_Level)["TRUE"]))
    {stop("\n Some categories in Unique_Path appear on more than one level, \n please ensure that each category appears in only one level")}
    }
  }  
}
