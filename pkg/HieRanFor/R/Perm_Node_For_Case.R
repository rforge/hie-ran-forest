

Perm_Node_For_Case = function(Case_Data,                   # Data frame, containing the case_ID and explantory variables for the focal case
                              Case_ID=1,                   # Integer of character, specifying the number or coulmn name in Case_Data that contains the Case_ID
                              Exp_Var=2:ncol(Case_Data),   # Integer of character, specifying the numbers or coulmn names in Case_Data that contain the explanatory variables
                              Hie_RF,                      # An object of class Hier.Random.Forest - the output of the Run_HRF function
                              ...)
{  # Start function
  
  require(randomForest)
  # Make some checks and Change from a character to numeric column number 
  
  if(is.character(Case_ID))
  {Case_ID_Cha <- Case_ID
   Case_ID     <- match(Case_ID,names(Case_Data))}
  
  if(is.character(Exp_Var))
  {Exp_Var  <- match(Exp_Var,names(Train_Data))}
  
  if(class(Hie_RF)!="Hier.Random.Forest")
  {stop("Hie_RF should be of class Hier.Random.Forest --> the output of the Run_HRF function")}
  
  if(nrow(Case_Data)!=1)
  {cat(paste("\n","Case_Data should be a data frame with a single row","\n","Data from the first row is used",sep=""))}
  
  # Check uf the inbags were kept when running Run_HRF
  if(!Hie_RF$call$keep.inbag)
  {stop("the Hie_RF was called with keep.inbag=FALSE --> cannot execute permutation ")}
  
  # extract the relevant information from Hie_RF
  LRF_Info          <- Hie_RF$Hier_Struc$LRF_Info        # the info data frame on each local classifer
  Nodes_Info        <- Hie_RF$Hier_Struc$Nodes_Info      # the info data frame on each node
  All_Local_RF      <- Hie_RF$All_Local_RF               # list containing all the local random forests. For each random forest, there is a list with two lists: the Local_Data and the Local_RF
  Classifer_in_List <- Hie_RF$Classifer_in_All_Local_RF  # THe location in All_Local_RF in which the data and model of each local classifer is stored
  
  # arrange the data for the focal case
  Case_Exp_Data <-  Case_Data[1,Exp_Var]
  Focal_Case <- Case_Data[1,Case_ID]
  
  # start in the Tree Root using the user defined Root_Name
  Parent_Node_Name <- Hie_RF$call$Root_Name
    
  repeat{ # start repeat 1
    
    # Get the info for the local classifer - data + RF
    Local_Calssifer <- subset(LRF_Info,LRF_Info$Par_Name==Parent_Node_Name)[1,1]
    Place_in_All    <- subset(Classifer_in_List,Classifer_in_List$Classifier_ID==Local_Calssifer)[1,2]
    
    # extract data for the local classifer
    Local_RF_Obj  <- All_Local_RF[[Place_in_All]]   # a list with Local_Data and Local_Rf
    Local_Data    <- Local_RF_Obj$Local_Data        # list, the local dat used in local classifer i
    Local_RF      <- Local_RF_Obj$Local_RF          # object of class RandomForest for local classifer i
    
    Local_Train_Case_ID <- as.data.frame(Local_Data$Local_Train_Case_ID)      # the Case_ID of the Local_Train_Data
    
    # explore if the Focal_Case is in the Training data of the local classifier
    # returns NA if not and the integer with the row number if yes
    Is_In_Train <- match(Focal_Case,Local_Train_Case_ID[,1] )
    
    if(is.integer(Is_In_Train) && !is.na(Is_In_Train))
    { # start -->  when the the focal case is in the training set
      
      # if yes use the inbag and keep forest of predict - output -->  a vector with potential child nodes
      # predict the child node for the focal case using all trees
      Predict_Case <- predict(object = Local_RF,
                              newdata = Case_Exp_Data,
                              predict.all = TRUE)$individual
      Predict_Case <- as.data.frame(Predict_Case)
      
      # Logical vector with FALSE for  trees in which the tree was inbag and TRUe for trees in which the focal case was out-of-bag
      col_true = Local_RF$inbag[Is_In_Train,]==0
      
      # subset from Predict_Case the prediction of the trees in which the focal case was out-of-bag
      Predict_Case <- subset(Predict_Case,select = col_true)
      
      rm(list=c("col_true"))
    } # End   -->  when the the focal case is in the training set
    
    if(is.na(Is_In_Train))
    { # start -->  when the the focal case is NOT in the training set
      
      # if not use predict -output --> a vector with potential child nodes
     
      # predict the child node for the focal case using all trees
      Predict_Case <- predict(object = Local_RF,
                              newdata = Case_Exp_Data,
                              predict.all = TRUE)$individual
      Predict_Case <- as.data.frame(Predict_Case)
    } # end -->  when the the focal case is NOT in the training set
    
    
    # select one child randomly from thecalssification results of the relevant trees
    
    Child_Node <- as.character(sample(size=1,Predict_Case)[1,1])  

    Term_Yes_No <- Nodes_Info[match(Child_Node[1],Nodes_Info$Node_Name),"Term_Int_node"]
    
    
    
    
    # if the child is NOT a terminal node --> find the next parent node in line
    if(Term_Yes_No=="Int_Node")
    { # start if "Int_Node"
      repeat{ # start repeat 2
        
        
        Classifer_Yes_No <- Nodes_Info[match(Child_Node[1],Nodes_Info$Node_Name),"Clas_yes_no"]
        if(Classifer_Yes_No =="Yes")
        {
          Parent_Node_Name <- Child_Node
          break
        }
        if(Classifer_Yes_No =="No")     
        {
          Child_Node <- Nodes_Info[match(Child_Node[1],Nodes_Info$Node_Par_Name),"Node_Name"]
          Term_Yes_No_2 <- Nodes_Info[match(Child_Node[1],Nodes_Info$Node_Name),"Term_Int_node"]
          if(Term_Yes_No_2=="Term_Node")
          {break}
        } 
       }# end repeat 2
    } # End if "Int_Node"
    
    # if the child is a terminal node --> assign the child to Classsified_As and break
    
    Term_Yes_No <- Nodes_Info[match(Child_Node[1],Nodes_Info$Node_Name),"Term_Int_node"]
    
    if(Term_Yes_No=="Term_Node")
    {Classifed_As <- Child_Node
     break}   
    
  } # End repeat 1
  
  Classifed_As
  
}  # End function