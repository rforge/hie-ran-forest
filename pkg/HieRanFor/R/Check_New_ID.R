

# Function - compares the Case_ID values of New_ID to those used in the Hie_RF
# stops the run of the calling function if IDs overlap

Check_New_ID = function(Hie_RF,  # an object of class Hier.Random.Forest
                        New_ID   # a vector/data rame that contains the new Case_IDs
                        )
  
{
  Train_Data_Ready <- Hie_RF$Train_Data_Ready
  Train_ID <- Train_Data_Ready[,Hie_RF$Case_ID_2]
  if(length(intersect(New_ID,Train_ID))>0)
  {
    stop("\n At least one ID in the New_Data already occurs in the training data of Hie_RF \n Please ensure that all Case_IDs are unique to a single case")
  }
  
  
}