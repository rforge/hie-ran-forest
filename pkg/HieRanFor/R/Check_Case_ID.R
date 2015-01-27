
# checks wether the columns specified as Case_ID contains a single entery for each ID

Check_Case_ID=function(Case_ID)
{
  Max_Duplicates <- max(table(Case_ID)) 
  if(Max_Duplicates!=1)
  {stop("\n At least one case appears in more than one row of Train_Data \n Please make sure that Case_ID is unique for each case (row)")}
  
}