#' Plot the Hierarchical class structure
#' 
#' 
#' 

Plot_Hie_Tree = function(Nodes_Info,                       # Data frame, containing the Node_Info data from Ide_Hie_Str or from Run_HRF
                         Unique_Path,                      # Data frame, containing the Unique_Path data from Ide_Hie_Str or from Run_HRF
                         Rect_Width=0.8,                   # Numeric, the width of boxes for each node, at width=1, the boxes overlap
                         Rect_Hieght=0.5,                  # Numeric, the hieght of the boxes for each node, center to center distance betwen levels is 1. 
                         Text_Size=7.5,                    # Numeric, a scaling number for the size of text 
                         Text_Angle=0,                     # Numeric, the angle of the text of the nodes
                         Split_Text = 6,                   # Numeric, the number of characthers in nodes names above which the text is broken to two lines
                         Int_Col="white",                  # Character, specifying the color of the fill of the internal nodes
                         Term_col="gray80",                # Character, specifying the color of the fill of the terminal nodes
                         Root_Col="gray50",                # Character, specifying the color of the fill of the tree root node
                         Y_Title="Hierarchical Level\n",   # Character, title of the y axis
                         Label_Nodes=TRUE,                 # Logical, should the nodes be labeled?
                         Label_Classifiers=TRUE,           # Logical, should the names of the local classifers be added to the tree?
                         Class_ID_Rect_Col="black",        # Character, specifying the color of the border of the classifer ID boxes
                         Class_ID_Rect_Fill="gold",        # Character, specifying the color of the fill of the classifer ID boxes
                         Class_ID_Rect_Linetype="dotted",  # Character, specifying the linetype of the classifer ID boxes
                         Class_ID_Rect_Hieght_Scale=0.25,  # Numeric, a scaling of the classifer ID box relative to the nodes boxes 
                         Class_ID_Text_Size_Scale=0.8,     # Numeric, a scaling of the classifer ID text relative to the nodes text 
                         Class_ID_Text_Color="black",      # Character, specifying the color of the text for the classifer ID
                         ...)
{
  
  require(ggplot2)
  
  # define the local environment
  localenv <- environment()
  
  # add the Num_Code column
  Work_Data_1              <- cbind(c(1:dim(Nodes_Info)[1]),Nodes_Info)
  colnames(Work_Data_1)[1] <- "Num_Node"
  
  # add the Num_Path column
  Work_Data_2              <- cbind(c(1:dim(Unique_Path)[1]),Unique_Path)
  colnames(Work_Data_2)[1] <- "Num_Path"
  
  # add the X and Y locations for each node
  for (i in 1:dim(Work_Data_1)[1])
  {
    Work_Data_1$Y[i] <- max(Work_Data_1$Node_Level)-Work_Data_1$Node_Level[i]+1
    Work_Node        <- Work_Data_1$Node_Name[i]
    Work_Node_Level  <- Work_Data_1$Node_Level[i]+2
    Work_Data        <- subset(Work_Data_2,Work_Data_2[,Work_Node_Level]==Work_Node)
    Work_Data_1$X[i] <- mean(Work_Data$Num_Path) 
  }
  
  # add the X_End and Y_End for each node according to its parent
  
  # the root gets NAs
  Work_Data_1$X_End[1] <- NA
  Work_Data_1$Y_End[1] <- NA
  
  # the other nodes
  for (k in 2:dim(Work_Data_1)[1])
  {
    Node_Par_Name        <- Work_Data_1$Node_Par_Name[k]
    Work_Data            <- subset(Work_Data_1[Work_Data_1[,2]==Node_Par_Name,])
    Work_Data_1$X_End[k] <- Work_Data$X
    Work_Data_1$Y_End[k] <- Work_Data$Y 
  }
  
  # the names for the levels on the Y axis
  Levels_name <- rev(colnames(Work_Data_2)[2:(dim(Work_Data_2)[2]-1)])
  
  ###################################
  # create the segments data frames #
  ###################################
  
  Segments_Data <- Work_Data_1[2:dim(Work_Data_1)[1],c("X","Y","X_End","Y_End")]
  Seg_New       <- Segments_Data[1,]
  Seg_New[1,]   <- NA
  
  for (k2 in 1:dim(Segments_Data)[1])
  {
    
    local_Seg       <- Segments_Data[k2,]
    local_Seg$Y     <- local_Seg$Y     + Rect_Hieght/2
    local_Seg$Y_End <- local_Seg$Y_End - Rect_Hieght/2
    
    X               <- local_Seg$X
    Y               <- local_Seg$Y
    X_End           <- local_Seg$X_End
    Y_End           <- local_Seg$Y_End
    Y_Mid           <- Y+(Y_End-Y)/2   
    
    Seg_New[(3*k2-2),c("X","Y","X_End","Y_End")] <- c(X,Y,X,Y_Mid)             # vertical child segment
    Seg_New[(3*k2-1),c("X","Y","X_End","Y_End")] <- c(X,Y_Mid,X_End,Y_Mid)     # horizontal segment
    Seg_New[3*k2,c("X","Y","X_End","Y_End")]     <- c(X_End,Y_Mid,X_End,Y_End) # vertical parent segment
  }
  
  
  ######################################################################
  # create the rectangles data frames for all nodes (except root node) #
  ######################################################################
  
  Rect_Data <- data.frame(X_Min = c(NA),    
                          X_Max = c(NA),         
                          Y_Min = c(NA),         
                          Y_Max = c(NA),       
                          Term_Int_node = c(NA))
   
                                         
  for (k3 in 2:dim(Work_Data_1)[1])
  {
    Rect_Data[k3-1,"X_Min"]         <- Work_Data_1[k3,"X"] - Rect_Width/2
    Rect_Data[k3-1,"X_Max"]         <- Work_Data_1[k3,"X"] + Rect_Width/2
    Rect_Data[k3-1,"Y_Min"]         <- Work_Data_1[k3,"Y"] - Rect_Hieght/2
    Rect_Data[k3-1,"Y_Max"]         <- Work_Data_1[k3,"Y"] + Rect_Hieght/2
    Rect_Data[k3-1,"Term_Int_node"] <- Work_Data_1[k3,"Term_Int_node"]
  }  
   
  fillScaleValues <- c("Int_Node"  = Int_Col,
                       "Term_Node" = Term_col)
  
  ######################################################
  # create the rectangle data frames for the root node #
  ######################################################
  
  Rect_Data_Root <- data.frame(X_Min    = c(NA),    
                              X_Max    = c(NA),         
                              Y_Min    = c(NA),         
                              Y_Max    = c(NA),       
                              Root_Col = c(NA))
  
  Rect_Data_Root[1,"X_Min"]    <- Work_Data_1[1,"X"] - Rect_Width*2
  Rect_Data_Root[1,"X_Max"]    <- Work_Data_1[1,"X"] + Rect_Width*2
  Rect_Data_Root[1,"Y_Min"]    <- Work_Data_1[1,"Y"] - Rect_Hieght/2
  Rect_Data_Root[1,"Y_Max"]    <- Work_Data_1[1,"Y"] + Rect_Hieght/2
  Rect_Data_Root[1,"Root_Col"] <- Root_Col
  
  ##########################################
  # create the nodes name text data frames #
  ##########################################
  
  Node_Text <- data.frame(X = c(NA),    
                          Y = c(NA),         
                          Label_Node = c(NA))
  
  for (k4 in 1:dim(Work_Data_1)[1])
  {
    Node_Text[k4,"X"]          <- Work_Data_1[k4,"X"]
    Node_Text[k4,"Y"]          <- Work_Data_1[k4,"Y"]  
    
    if(nchar(Work_Data_1[k4,"Node_Name"])<= Split_Text)
      { Node_Text[k4,"Label_Node"] <- Work_Data_1[k4,"Node_Name"] }
    
    if(nchar(Work_Data_1[k4,"Node_Name"])> Split_Text)
      {
      End_Chr=round(nchar(Work_Data_1[k4,"Node_Name"])/2,0)
      First_Term <- substr(Work_Data_1[k4,"Node_Name"],1,End_Chr)
      Second_Term <-substr(Work_Data_1[k4,"Node_Name"],End_Chr+1,nchar(Work_Data_1[k4,"Node_Name"]))
      Node_Text[k4,"Label_Node"] <- paste(First_Term, Second_Term,sep="\n")
      }
  
  }  
  

  ############################################
  # create the classifer ID text data frames #
  ############################################
  
  if (Label_Classifiers==TRUE)
   {
    Local_classifiers <- subset(Work_Data_1,Work_Data_1$Clas_yes_no=="Yes")
    
    Local_RF_Text     <- data.frame(X           = c(NA),    
                                    Y           = c(NA),         
                                    Label_Class = c(NA))
    
    Rect_Classifier   <- data.frame(X_Min = c(NA),    
                                    X_Max = c(NA),         
                                    Y_Min = c(NA),         
                                    Y_Max = c(NA))
    
    for(k5 in 1:dim(Local_classifiers)[1])
    { # start k5 loop
      Local_RF_Text[k5,"X"]           <- Local_classifiers[k5,"X"]
      Local_RF_Text[k5,"Y"]           <- Local_classifiers[k5,"Y"]-0.5
      Local_RF_Text[k5,"Label_Class"] <- Local_classifiers[k5,"Classifier_ID"]
      
      Rect_Classifier[k5,"X_Min"]     <- Local_RF_Text[k5,"X"] - Rect_Width/2.5
      Rect_Classifier[k5,"X_Max"]     <- Local_RF_Text[k5,"X"] + Rect_Width/2.5
      Rect_Classifier[k5,"Y_Min"]     <- Local_RF_Text[k5,"Y"] - Rect_Hieght*Class_ID_Rect_Hieght_Scale
      Rect_Classifier[k5,"Y_Max"]     <- Local_RF_Text[k5,"Y"] + Rect_Hieght*Class_ID_Rect_Hieght_Scale  
    } # end k5 loop 
  } # end if Label_Classifiers==TRUE 
  
  
  ####################
  # Start the ggplot #
  ####################
  
  
  p <- ggplot() 
  
  # add the segments
  p <- p + geom_segment(data = Seg_New,
                        aes(x    = X, 
                            y    = Y, 
                            xend = X_End, 
                            yend = Y_End),
                        environment = localenv)
  
  # add the boxes for all nodes except the Root
  p <- p + geom_rect(data = Rect_Data,
                     aes(xmin = X_Min, 
                         xmax = X_Max, 
                         ymin = Y_Min, 
                         ymax = Y_Max,
                         fill=Term_Int_node),
                     color = "black")
  
  # add the box for the Root
  p <- p + geom_rect(data  = Rect_Data_Root,
                     aes(xmin = X_Min, 
                         xmax = X_Max, 
                         ymin = Y_Min, 
                         ymax = Y_Max),
                     fill  = Root_Col,
                     color ="black")
  
  # add the Nodes names inside the boxes
  if(Label_Nodes==TRUE)
  {
    p <- p + geom_text(data = Node_Text,
                       aes(x     = X, 
                           y     = Y,
                           label = Label_Node
                           ),
                       size = Rect_Hieght*Text_Size,
                       angle = Text_Angle)
  } # end if statement: (Label_Nodes==TRUE)
  
  ### the classifiers ID ###
  if(Label_Classifiers==TRUE)
  {
    # add the classifer ID boxes 
    p <- p + geom_rect(data     = Rect_Classifier,
                       aes(xmin = X_Min, 
                          xmax = X_Max, 
                          ymin = Y_Min, 
                          ymax = Y_Max
                       ),
                       color    = Class_ID_Rect_Col,
                       fill     = Class_ID_Rect_Fill,
                       linetype = Class_ID_Rect_Linetype)
    
    
    # add the classifer ID 
    p <- p + geom_text(data  = Local_RF_Text,
                       aes(x     = X, 
                           y     = Y,
                           label = Label_Class),
                       size  = Rect_Hieght*Text_Size*Class_ID_Text_Size_Scale,
                       color = Class_ID_Text_Color)
  } # end if statement: (Label_Classifiers==TRUE) 
  
  # arrange the theme
  p <- p + theme(axis.text.x     = element_blank(),
                 legend.position = "none",
                 axis.title.x    = element_blank(),
                 axis.title.y    = element_text(size   = Rect_Hieght*Text_Size*4,
                                                colour ="black"),
                 axis.text.y     = element_text(size   = Rect_Hieght*Text_Size*4,
                                                colour = "black"))
  
  # add the y title
  p <- p + ylab(Y_Title)
  
  # change the labels for the Y axis
  p <- p + scale_y_discrete (limit  = Levels_name)
  
  # adjust the colors for the fill of the boxes according to internal vs. terminal nodes
  p <- p + scale_fill_manual(values = fillScaleValues)
  
  # return the ggplot object
  return(print(p)) # ggplot object
  
} # end Function