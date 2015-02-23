
# checks wether the columns specified as Case_ID contains a single entery for each ID

CheckCaseID = function(case.ID)
{
  max.duplicates <- max(table(case.ID)) 
  if(max.duplicates != 1)
  {stop("\n At least one case appears in more than one row of Train_Data \n Please make sure that case.ID is unique for each case (row)")}
  
}