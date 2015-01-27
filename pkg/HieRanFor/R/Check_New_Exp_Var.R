

# Function, compares the names of the explnaatory variables of New_Data with those used when creating HRF
# Stops the calling function if the match is not perfect

Check_New_Exp_Var = function(Hie_RF, # Object of class Hier.Random.Forest
                             New_Data,  # the New_Data that is expected to be used with Predict
                             New_Data_Exp_Var)  # vector specifying the column numbers of the explanatory variables in New_Data
{
  Train_Exp_Var <- names(Hie_RF$Train_Data_Ready)[Hie_RF$Exp_Var_2]
  New_Exp_Var   <- names(New_Data)[New_Data_Exp_Var]
  
  Obs_Length <- length(intersect(New_Exp_Var ,Train_Exp_Var))
  Exp_Length_Train <- length(Train_Exp_Var)
  Exp_Length_New <- length(New_Exp_Var)
  
  if(Obs_Length!=Exp_Length_Train || Obs_Length!=Exp_Length_New )
  {
    stop("\n The explanatory variables in New_Data do not match those of Hie_RF \n Please ensure that all explanatory variables have identical names or that the column numbers are coorrect")
  }
  
  
  
} # End Function