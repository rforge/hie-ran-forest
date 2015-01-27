

# Takes as input the LRF_Info, Train_Data_Ready, Hie_Levels_2 and Exp_Var_2 and
# returns for a specifci classifer in LRF the rows of 
# Train_Data that are relveant to the local RandomForest classifer (Local_Class)

Subset_LRF_Cases = function(Local_Class,        # Integer, specifying the row in LRF_Info for which subset of Train_Data should be returned
                            LRF_Info,           # data frame, specifying each local classifer as a row- the output of Ide_Hie_Str
                            Train_Data_Ready,   # the data frame with the training data  
                            Hie_Levels_2,       # columns in Train_Data_ready that contains the information on the hierarchical levels
                            Exp_Var_2,          # columns in Train_Data_ready that contains the iexplanatory variables
                             ...)
{
  
  
  Class_ID      <- LRF_Info[Local_Class,1]      # the name of the local classifer
  Par_Node_Lev  <- LRF_Info[Local_Class,2]      # The level of the parent node
  Par_Node_Name <- LRF_Info[Local_Class,3]      # the name of the parent node
  Col_Parent    <- Hie_Levels_2[1+Par_Node_Lev] # the column number of Train_Data_Ready in which the parent node appears
  Col_Child     <- Hie_Levels_2[2+Par_Node_Lev] # the column number of Train_Data_ready in which the childrens nodes appear
  
  # the information on the lcoal classifier
  Local_Class_Info      <- LRF_Info[Local_Class,] 
 
 # subseting the relevent cases from Train_Data_Ready
  Local_Train_Data      <- subset(Train_Data_Ready,Train_Data_Ready[,Col_Parent]==Par_Node_Name)
 
 # The local Case_IDs, Path_Name, 
  Local_Train_Case_ID   <- Local_Train_Data[,1]
  Local_Train_Path_Name <- Local_Train_Data[,2]
 
 # The coulmn from Local_Train_data with the child categories, drop levels is applied
 #Local_Train_Cat       <- Local_Train_Data[,Col_Child]
 Local_Train_Cat       <- droplevels(Local_Train_Data[,Col_Child])
 
 # The coulmn from Local_Train_data with the explanatory varaibles, 
 # drop levels is not applied to allow predictions for all levels of factors that are found within the training set
 Local_Train_Vars  <- Local_Train_Data[,Exp_Var_2]
 
 # Local_Train_Vars      <- droplevels(Local_Train_Data[,Exp_Var_2])
  
 #  the list to return
  list(Local_Class_Info      = Local_Class_Info,         # all the information on the local clasifier
       Local_Train_Data      = Local_Train_Data,         # all the cases in Train_Data_Ready that are classified in the local classifer
       Local_Train_Case_ID   = Local_Train_Case_ID,      # The case_Id of the cases in Local_Train_Data
       Local_Train_Path_Name = Local_Train_Path_Name,    # The Path_Name of the cases in Local_Train_Data
       Local_Train_Cat       = Local_Train_Cat,          # The child categories of all the cases in Local_Train_Data
       Local_Train_Vars      = Local_Train_Vars)         # THe explantory vartiabled for all cases in Local_Train_Data
       
} # end function 


                      
