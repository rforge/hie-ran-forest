


# Location of the R codes in the models computer
setwd("~/Tree of Knowledge/5-EU-BON/WP3.1/Hierarchical Random Forest/6_R codes/Package_HieRanFor")



# library(randomForest)

## remove everything and reload all functions

rm(list=ls())

Function_List <-list(##### Check functions
                     "Check_Case_ID.R",                   # function, Checks unique casees for Case_ID
                     "Check_Hie_Levels.R",                # function, check if the columns specified in Hie_Levels contains factors
                     "Check_Multi_Levels.R",              # function, checks if any node names is used in more than one level and stops the function if true 
                     "Check_Not_DAG.R",                   # function, checks that every node as a single parent
                     "Check_New_ID.R",                    # function, checks if any of the New_ID already occur in the training data of HieRF
                     "Check_New_Exp_Var.R",               # function, check wether the column nmaes of the explanatory variables of New_Data match exactly those used in HieRF 
                                     
                     ##### Generate data functions
                     "Random_HRF_Data.R",                 # function, creates random hierarchical data for a predefined number of categorical and numerical varaibles. Based on runif
                     
                     ##### Run functions
                     "Run_HRF.R",                         # function, main function that runs the hierarchical radndom forest and calls varioues other functions
                     "Run_HRF_As_Flat.R",                 # function, takes as input an object of class Hier.Random.Forest and runs a flat randomforest.By default, use the same rnadomForest parameters as each local classifer in Hie_RF
                     
                     ##### Plot functions
                     "Plot_Cat_Cat_Var.R",                # function, plot the hierarchical importance as variable vs. classifer. importance is given as tiles or bubbles with color or area relative to the mean decrease in accuracy. 
                     "Plot_Hie_Tree.R",                   # function, basic function that plots the hierarchical tree with/without the local classifers
                     
                     ##### Hierarchy related functions
                     "Ide_Hie_Str.R",                     # function, idnetifies the hierechical structure given in the Train data
                     "Sort_Unique_Path.R",                # function, sort the unique path data frame according to all colomn (order is kept)
                     "Get_Terminal_Node.R",               # function, identifies the terminal node of a a path.
                     "Subset_LRF_Cases.R",                # function, subset from the global training set, the local training set for a local clasifier
                     "Num_Shared_Nodes.R",                # function, takes the Unique_Path data frame as input and returns a square table, with every terminal node as a column and row. The entries in the table are the number of shared nodes in the two pathes from the two two nodes to the tree root, excluding the tree root.
                     
                     ##### Importance function
                     "Hie_Importance.R",                  # function, extract an hierarchial importance table with importatncw values of each variable in each local classifer
                     
                     ##### Predict functions
                     "Predict_New.R",                     # function, predicts the raw votes, multiplicative proportions and crisp classification for new data
                     
                     ##### Performance functions
                     "HRF_Performance.R",                 # function, returns mutltiple flat and hierarchical performance measures for a crisp classifcation. Crispt classifcation may be based on multiplicative majority, stepwise majority or multiplicative permutations 
                     "HRF_Performance_New_Data.R",        # function, predict and asses performance of New_Data, for which the 'true' class is known
                     "Flat_RF_Performance.R",             # function, takes as input an object of class Hier.Random.Forest, runs a flat classsification and estimate performance 
                     
                     ##### Performance indices
                     "Hie_F_Measure.R",                   # function, takes as input a confusion matrix, the unique_Path data frame and beta_H_F (weight of hierarchical precision relative to hierarchial recall when calcaultaing the hierarhcial F measure) and return the hierarchical precision, hierarchical recall and hierrarchaocla F measure
                     
                     ##### Proportion of votes functions
                     "Extract_Votes.R",                   # function, extract the proportion of votes for each case in each node of each local classifer
                     "Multiplicative_Prop_Votes.R",       # function, esetimate the proportion of votes for each case, in each local classifer, as well as multiplies the proportion of votes along the hierarchical tree. Proportion of votes is estimated with OOB. 
                     "Case_Multiplicative_Prob.R",        # function, for a single case, and all pathes, returns the multplication of the lcoal probabilites from tree root to terminial nodes
                     
                     ##### Majority rules functions
                     "Multiplicative_Majority_Rule.R",    # function, akes as input the proportion of votes for each node and for each case returns the noide that received the maximal proportion fo votes.
                     "Stepwise_Majority_Rule.R",          # function, takes the output of Extract_Votes and the Hier.Random.Forest object and estimates for each case the stepwise majority vote
                     
                     ##### Permutations related functions
                     "Perm_Node_For_Case.R",              # function, returns one terminal node for a focal case, by randomly selecting trees from tree root to leaves
                     "Perm_Multiplicative_Term_Node.R",   # function, takes as input the Multiplicativeian chain probabilities and return for each case (row) a random node as terminal node.                   
                     
                     ##### Additional functions
                     "Join_Levels.R",                     # function, takes as input two vecotrs, turn them into factors, add to each one the levels tat are not found in the other and sort the levels to be in the same order. Returns the two modified vectors
                     "List_Obj_Class.R",                  # function, return a data frame of all the objects in the global environment and their class
                     "TuneRF_2.R"                         # function, same as tuneRF of randomForest, but do not return error if the observed error rate is 0. 
                     )

lapply(Function_List,FUN=source)

rm(Function_List)


################################################
## View all objects in the global environment ##

All_Objects <- List_Obj_Class()
All_Objects <- All_Objects[order(All_Objects[,2]),]
print(All_Objects)


########################################################
### Create random hierarchical data                  ###
### using the random.hierarchical.data function      ###
### of the treeemap package.                         ###
### NOTE- terminal nodes in the output hierarachies  ###
### are always in the lowest level                   ###
########################################################

library("treemap")

Num_Term_Nodes <- 27  # the number of terminal nodes at the lowest level
Tree_Depth     <- 4   # the number of levels, excluding Tree Root
Unique_Path_2  <- random.hierarchical.data(n=Num_Term_Nodes,depth=Tree_Depth) # create the random hierarchy
Unique_Path_2  <- Unique_Path_2[,1:dim(Unique_Path_2)[2]-1]  #remove numerical random number



#######################################################
### read an hiearchy from the clipboard             ###
#######################################################

# alternative input of hierarchical strucutre
Unique_Path_2 <- read.delim("clipboard",header=TRUE)


############################################
### Create random Train and Predict data ###
### using the Random_HRF_Data function   ###
############################################

Random_HRF <-Random_HRF_Data(Unique_Path=Unique_Path_2,  # data frame with the hierarchies
                             Cells_per_Cat=15,           # Integer, number of training cells to create for each terminal node
                             Num_Num_Var=5,             # Integer, Number of numeric explanatory variables 
                             Num_Cat_Var=5,             # Integer, Number of categorical explanatory variables
                             Case_ID="Case_ID",          # character, the column name for the Cell ID
                             cells_per_cat_predict=25    # Integer, Number of cases per terminal node in the Predict_Data data frame
                             )
  

Train_Data     <-  Random_HRF$Train_Data
Predict_Data   <-  Random_HRF$Predict_Data


##################################
### Try the Run_HRF function   ###
##################################
Hie_Levels_col <- c(1:dim(Unique_Path_2)[2])+1



try_Run_HRF <- Run_HRF(Train_Data=Train_Data,                      # the data frame with the training data
                 Case_ID="Case_ID",                         # a character containing the name of column that should be used as row IDs. if a numeric is given, it is taken as the columns number in Train_Data                                                        # 
                 Hie_Levels=names(Train_Data[Hie_Levels_col]),                      # a character vector containing the names of column of the hierarchical levels, order of elements is according to hierarchy. # if a numeric vector is given, than the numbers should specify columns numbers in Train_Data                                                  
                 Exp_Var           = NA,          # a character vector containing the names of the columns used as explanatory variables in the random forest # For default, assume that all  columns other than Case_ID, Cat_Code and Hie_Level are varialbe for random forests          
                 Internal_End_Path = TRUE,       # Logical (TRUE/FALSE)- are all terminal nodes ending in the lowest level of the hierarchy?
                 End_Path_Name     = "End_Path",  # Character - the name used in level i+1 for terminal nodes ending in level i.
                 Root_Include      = FALSE,       # Logical (TRUE/FALSE)is the tree root included in Hie_Levels?
                 Root_Name         = "TREE_ROOT", # Character - name to use for the tree root
                 mtry              = "tuneRF",    # Number of variables randomly sampled as candidates at each split. 
                 # Note that the default values is to use tuneRF function of randomForest for each local classifier
                 ntree             = 500,         # number of trees to grow in each local classifier
                 importance        = TRUE,        # Should importance of predictors be assessed?
                 proximity         = TRUE,        # should the proximity be calcualted? 
                 keep.forest       = TRUE,        # Should proximity measure among the rows be calculated?
                 keep.inbag        = TRUE)        # should a Should an n by ntree matrix be returned that keeps track of which samples are
                 # “in-bag” in which trees (but not how many times, if sampling with replacement
                 # required for the permutation-based accuracy assemsents
                 
class(try_Run_HRF)

(a1 <- attributes(try_Run_HRF))
a2  <- try_Run_HRF$Hier_Struc$LRF_Info
a3  <- try_Run_HRF$Hier_Struc$Nodes_Info
a4  <- try_Run_HRF$Hier_Struc$Unique_Path
a5  <- try_Run_HRF$Train_Data_Ready
a6  <- try_Run_HRF$Case_ID_2
a7  <- try_Run_HRF$Path_Name_2
a8  <- try_Run_HRF$Hie_Levels_2
a9  <- try_Run_HRF$Exp_Var_2
a10 <- try_Run_HRF$All_Local_RF
a11 <- try_Run_HRF$Classifer_in_All_Local_RF
a12 <- try_Run_HRF$call
a13 <- try_Run_HRF$call$End_Path_Name
names(a12)




# run the Ide_Hie_Stu function

try_Ide_Hie_Stu <- Ide_Hie_Str(Unique_Path       = Unique_Path_2, 
                               Internal_End_Path = TRUE, 
                               End_Path_Name     = "End_Path",
                               Root_Include      = FALSE,
                               Root_Name         = "H0")



# run the Subset_LRF_Cases function

Try_Subset_LRF_Cases <- Subset_LRF_Cases(Local_Class      = 1,  # Integer, specifying the row in LRF_Info for which subset of Train_Data should be returned
                                         LRF_Info         = try_Run_HRF$Hier_Struc$LRF_Info, # data frame, specifying each local classifer as a row- the output of Ide_Hie_Str
                                         Train_Data_Ready = try_Run_HRF$Train_Data_Ready, # the data frame with the training data  
                                         Hie_Levels_2     = try_Run_HRF$Hie_Levels_2, # columns in Train_Data_ready that contains the information on the hierarchical levels
                                         Exp_Var_2        = ry_Run_HRF$Exp_Var_2)
  


#########################################
### Try the Hie_Importance function   ###
#########################################

Try_Hie_Importance <- Hie_Importance(Train_Data_Ready = try_Run_HRF$Train_Data_Ready,
                                     Exp_Var_2        = try_Run_HRF$Exp_Var_2,
                                     LRF_Info         = try_Run_HRF$Hier_Struc$LRF_Info,
                                     All_Local_RF     = try_Run_HRF$All_Local_RF
                                    )

# check out this link to interesting bubble plts to present the variables importance
# http://stackoverflow.com/questions/15840926/categorical-bubble-plot-for-mapping-studies


#################################################################
### Try the Plot_Cat_Cat_Var function with importance values  ###
#################################################################

Try_Imp_Var_4_Column <- Try_Hie_Importance$Imp_Var_4_Column

# try the bubble plot
Try_Plot_Bubble <- Plot_Cat_Cat_Var(Input_Data = Try_Imp_Var_4_Column,                                  # data frame containing the importance data, each row is for a specific combination of local classifer and explanatory variable. default format is assumed to be the 4 column format data frame of Hie_Importance
                                X_Data    = 2,                                  # integer, the column number that contains yje data for the X axis
                                Y_Data    = 3,                                  # integer, the column number that contains the data for the y axis
                                Imp_Data  = 4,                                  # the column number that contains the importance data for tthe tiles or bubbles
                                Plot_Type = "Bubble",                             # Character, meaningfull values are "Tile" (default) or "Bubble", the type of plot that will be returned. "tile" returns a geom tile, with colur of the tiel representing the mean decrease in accuracy. 
                                Imp_title = "Mean \nDecrease \nIn \nAccuracy",  # Character - the title to use for the Importance legend
                                X_Title   = "Parent Node",                      # Character, title for the x axis
                                Y_Title   = "Explanatory Variable",             # Character, title for the y axis
                                low_col   = "blue" ,                            # Charcther, if type=='Tile', the colour to use for the highest importance value                 
                                high_col  = "yellow",                              # Charcther, if type=='Tile', the colour to use for the lowest importance value    
                                Pos_Col   = "orange",                            # Charcther, if type=='Bubble', the colour to use for the  importance values >0          
                                Zero_col  = "white",                            # Charcther, if type=='Bubble', the colour to use for the importance values ==0          
                                Neg_Col   = "red")

# retreive the plot
P_Try_Plot_Bubble <- Try_Plot_Bubble$plot
# add additional arguments to the plot
P_Try_Plot_Bubble <- P_Try_Plot_Bubble + geom_text(data=NULL, x=1,y=5,label = "OK")
P_Try_Plot_Bubble



# try the tile plot
Try_Plot_Tile <- Plot_Cat_Cat_Var(Input_Data = Try_Imp_Var_4_Column,                                  # data frame containing the importance data, each row is for a specific combination of local classifer and explanatory variable. default format is assumed to be the 4 column format data frame of Hie_Importance
                              X_Data    = 2,                                  # integer, the column number that contains yje data for the X axis
                              Y_Data    = 3,                                  # integer, the column number that contains the data for the y axis
                              Imp_Data  = 4,                                  # the column number that contains the importance data for tthe tiles or bubbles
                              Plot_Type = "Tile",                             # Character, meaningfull values are "Tile" (default) or "Bubble", the type of plot that will be returned. "tile" returns a geom tile, with colur of the tiel representing the mean decrease in accuracy. 
                              Imp_title = "Mean \nDecrease \nIn \nAccuracy",  # Character - the title to use for the Importance legend
                              X_Title   = "Parent Node",                      # Character, title for the x axis
                              Y_Title   = "Explanatory Variable",             # Character, title for the y axis
                              low_col   = "red" ,                            # Charcther, if type=='Tile', the colour to use for the highest importance value                 
                              high_col  = "yellow",                              # Charcther, if type=='Tile', the colour to use for the lowest importance value    
                              Geom_Tile_Bor_Col = "white",                       # Charcther, the color for theif type=='Tile', the colour to use for the border of tile, set to NA for no tile border
                              Pos_Col   = "orange",                            # Charcther, if type=='Bubble', the colour to use for the  importance values >0          
                              Zero_col  = "white",                            # Charcther, if type=='Bubble', the colour to use for the importance values ==0          
                              Neg_Col   = "red")

Try_Plot_Tile <- Plot_Cat_Cat_Var(Input_Data = Try_Imp_Var_4_Column,                                  # data frame containing the importance data, each row is for a specific combination of local classifer and explanatory variable. default format is assumed to be the 4 column format data frame of Hie_Importance
                                  X_Data    = 2,                                  # integer, the column number that contains yje data for the X axis
                                  Y_Data    = 3,                                  # integer, the column number that contains the data for the y axis
                                  Imp_Data  = 4,                                  # the column number that contains the importance data for tthe tiles or bubbles
                                  Plot_Type = "Tile",                             # Character, meaningfull values are "Tile" (default) or "Bubble", the type of plot that will be returned. "tile" returns a geom tile, with colur of the tiel representing the mean decrease in accuracy. 
                                  Imp_title = "Mean \nDecrease \nIn \nAccuracy",  # Character - the title to use for the Importance legend
                                  X_Title   = "Parent Node",                      # Character, title for the x axis
                                  Y_Title   = "Explanatory Variable",             # Character, title for the y axis
                                  low_col   = "white" ,                            # Charcther, if type=='Tile', the colour to use for the highest importance value                 
                                  high_col  = "black",                              # Charcther, if type=='Tile', the colour to use for the lowest importance value    
                                  Geom_Tile_Bor_Col = "white",                       # Charcther, the color for theif type=='Tile', the colour to use for the border of tile, set to NA for no tile border
                                  Pos_Col   = "orange",                            # Charcther, if type=='Bubble', the colour to use for the  importance values >0          
                                  Zero_col  = "white",                            # Charcther, if type=='Bubble', the colour to use for the importance values ==0          
                                  Neg_Col   = "red")

# retreive the plot
P_Try_Plot_Tile <- Try_Plot_Tile$plot
# add additional arguments to the plot
P_Try_Plot_Tile <- P_Try_Plot_Tile + geom_text(data=NULL, x=1,y=5,label = "OK")
P_Try_Plot_Tile
P_Try_Plot_Tile <- P_Try_Plot_Tile + geom_tile(colour = "red")

########################################
### Try the Plot_Hie_Tree function   ###
########################################


Try_Plot_Tree <- Plot_Hie_Tree(Nodes_Info     = try_Run_HRF$Hier_Struc$Nodes_Info,
                               Unique_Path     = try_Run_HRF$Hier_Struc$Unique_Path,
                               Rect_Width       = 0.85,
                               Rect_Hieght       = 0.5,
                               Text_Size          = 10,
                               Int_Col             = "white",
                               Term_col             = "gray80",
                               Root_Col              = "gray50",
                               Y_Title                = "Hierarchical level\n",
                               Label_Nodes             = TRUE,
                               Label_Classifiers        = TRUE,
                               Class_ID_Rect_Col         = "black",
                               Class_ID_Rect_Fill         = "gold",
                               Class_ID_Rect_Linetype     = "dotted",
                               Class_ID_Rect_Hieght_Scale = 0.25,
                               Class_ID_Text_Size_Scale   = 0.8,
                               Class_ID_Text_Color        = "black"
                              )



plot_tree <- Try_Plot_Tree$plot
plot_tree <- plot_tree + ggtitle("Hie_8")
plot_tree <- plot_tree + theme(plot.title = element_text(lineheight=.8, face="bold",color="red"))
plot_tree

plot_tree <- plot_tree + theme( axis.title.y=element_text(size = 20,colour="red"))
plot_tree


########################################
### Try the Extract_Vote  function   ###
########################################

Try_Extract_Votes <- Extract_Votes(Hie_RF           = try_Run_HRF,
                                   Train_Predict    = TRUE,
                                   New_Data_Case_ID = 1,
                                   New_Data_Exp_Var = c(2:ncol(Predict_Data)),
                                   New_Data         = Predict_Data,
                                   Bind_Train_New   = TRUE
                                   )

names(Try_Extract_Votes)
Try_Prop_Vote_Train <- Try_Extract_Votes$Prop_Vote_Train
Try_Prop_Vote_New   <- Try_Extract_Votes$Prop_Vote_New
Try_Prop_Vote_Full  <- Try_Extract_Votes$Prop_Vote_Full

colnames(Try_Prop_Vote_Train)==colnames(Try_Prop_Vote_New)


####################################################
### Try the Multiplicative_Prop_Votes function   ###
####################################################

# With All_Levels = TRUE
Try_Multiplicative_Prop_Votes <- Multiplicative_Prop_Votes(Prop_Vote   = Try_Prop_Vote_Train,
                                                           Unique_Path = try_Run_HRF$Hier_Struc$Unique_Path,
                                                           All_Levels = TRUE)

names(Try_Multiplicative_Prop_Votes)

try_L1 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L1 
try_L2 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L2
try_L3 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L3
try_L4 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L4
try_L5 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L5
try_L6 <- Try_Multiplicative_Prop_Votes$Prop_Multiplicative_Votes_L6


# With All_Levels = FALSE
Try_Multiplicative_Prop_Votes <- Multiplicative_Prop_Votes(Prop_Vote   = Try_Prop_Vote_Train,
                                                           Unique_Path = try_Run_HRF$Hier_Struc$Unique_Path,
                                                           All_Levels = FALSE)


###########################################################
### Try the Multiplicative_Majority_Rule function       ###
###########################################################

# with bind
Try_Multip_Majority_Rule <- Multiplicative_Majority_Rule(Prop_Multiplicative_Votes = try_L4,          # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is Train_or_Test and the second is Case_ID
                                                  Rule_Treshold             = 0,               # Numeric, the minimum distance betweeen the class that recevied the hiest proptoion of votes and the class that came second, if thedistance is lower than the treshold, the case will be classifed as Class_If_Under 
                                                  Class_If_Under            = "Unclassified",
                                                  Bind_Prop_Mul_Maj         = TRUE)


# without bind
Try_Multip_Majority_Rule <- Multiplicative_Majority_Rule(Prop_Multiplicative_Votes = try_L4,          # data frame containing the porportion of votes that each case received for each terminal node. The sum of propotion should be 1. the first column is Train_or_Test and the second is Case_ID
                                                  Rule_Treshold             = 0,               # Numeric, the minimum distance betweeen the class that recevied the hiest proptoion of votes and the class that came second, if thedistance is lower than the treshold, the case will be classifed as Class_If_Under 
                                                  Class_If_Under            = "Unclassified",
                                                  Bind_Prop_Mul_Maj         = FALSE)



#################################################
### Try the Perm_Node_For_Case function       ###
#################################################

Case_Num_Try <- 100
## one time for a single case  ##
###   ###   ###   ###   ###   ###
Try_Case_Data <- try_Run_HRF$Train_Data_Ready[Case_Num_Try,]

TrY_Perm_Node_For_Case <- Perm_Node_For_Case(Case_Data = Try_Case_Data, 
                                             Case_ID=1,
                                             Exp_Var=try_Run_HRF$Exp_Var_2,
                                             Hie_RF = try_Run_HRF)


names(TrY_Perm_Node_For_Case)

###########################################################
### Try the Perm_Multiplicative_Term_Node function      ###
###########################################################


# with bind
try_Perm_Multiplicative_Term_Node <- Perm_Multiplicative_Term_Node(Multiplicative_Prop_Votes = try_L4,  # Data frame, containing the Multiplicativeian proportion of votes for each terminal node and for each case. onr of the outputs of Majority_Rule function. First column is the Train_or_test, second column is the Case_ID, 
                                                                   Perm_Num                  = 100,     # Integer, number of random votes to take for each case 
                                                                   Div_Logical               = TRUE,    # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                                                                   Div_Print                 = 10,      # integer, if Div_Logical is TUE, than progress message will be printed every Div_Print permutations
                                                                   Bind_Prop_Perm            = TRUE)    # logical, if true, the permutated terminal nodes are added at the end of the Multiplicative_Prop_Votes. If FALSE, a seperate data frame is returned, cointating only the Case_ID and the permuted terminal nodes


# without bind
try_Perm_Multiplicative_Term_Node <- Perm_Multiplicative_Term_Node(Multiplicative_Prop_Votes = try_L4,  # Data frame, containing the Multiplicativeian proportion of votes for each terminal node and for each case. onr of the outputs of Majority_Rule function. First column is the Train_or_test, second column is the Case_ID, 
                                                                   Perm_Num                  = 100,     # Integer, number of random votes to take for each case 
                                                                   Div_Logical               = TRUE,    # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                                                                   Div_Print                 = 10,      # integer, if Div_Logical is TUE, than progress message will be printed every Div_Print permutations
                                                                   Bind_Prop_Perm            = FALSE)   # logical, if true, the permutated terminal nodes are added at the end of the Multiplicative_Prop_Votes. If FALSE, a seperate data frame is returned, cointating only the Case_ID and the permuted terminal nodes


names(try_Perm_Multiplicative_Term_Node)



###########################################################
### Try the Stepwise_Majority_Rule function             ###
###########################################################

# with bind
Try_Stepwise_Majority_Rule <- Stepwise_Majority_Rule(Hie_RF=try_Run_HRF,
                                                     Prop_Vote=Try_Prop_Vote_Train,
                                                     Bind_Prop_Step_Maj = TRUE)

# without bind
Try_Stepwise_Majority_Rule <- Stepwise_Majority_Rule(Hie_RF=try_Run_HRF,
                                                     Prop_Vote=Try_Prop_Vote_Train,
                                                     Bind_Prop_Step_Maj = FALSE)


#################################################
### Try the HRF_Accuracy function             ###
#################################################


try_HRF_Performance <- HRF_Performance(Hie_RF      = try_Run_HRF ,                                         # object of class Hier.Random.Forest - the output of Run_HRF
                                    Per_Index   = c("Flat_Measures",
                                                   "Hie_F_Measure"),     # the accurcary index to use. 
                                    Crisp_Rule  = c("Multiplicative_Majority",
                                                    "Multiplicative_Permutation",
                                                    "Setpwise_Majority"),
                                    Perm_Num    = 50,
                                    By_Node     = TRUE,
                                    Div_Logical = TRUE,      # Logical, if TRUE progress when permutating the proportion of votes will be printed every Div_Print permutations
                                    Div_Print   = 5,
                                    Beta_H_F    = 1
                                    )

class(try_HRF_Performance)
names(try_HRF_Performance)

try_Raw_Votes              <- try_HRF_Performance$Raw_Votes
try_Train_Data_Acc         <- try_HRF_Performance$Train_Data_Acc
try_Crisp_Case_Class       <- try_HRF_Performance$Crisp_Case_Class

try_Multiplicative_Prop    <- try_HRF_Performance$Multiplicative_Prop

try_Hie_Perf_Mult_Maj      <- try_HRF_Performance$Hie_Perf_Mult_Maj
try_Hie_Perf_Mult_Perm     <- try_HRF_Performance$Hie_Perf_Mult_Perm
try_Hie_Perf_Step_Maj      <- try_HRF_Performance$Hie_Perf_Step_Maj

try_Hie_Perf_Step_Maj      <- try_HRF_Performance$Hie_Perf_Step_Maj
try_Nodes_Measures_columns <- try_HRF_Performance$Nodes_Measures_columns
try_call                   <- try_HRF_Performance$call


#################################################
### Try the Run_HRF_As_Flat function          ###
#################################################


try_Run_HRF_As_Flat = Run_HRF_As_Flat(Hie_RF= try_Run_HRF,                                   # object of class Hier.Random.Forest - the output of Run_HRF
                                      mtry         = try_Run_HRF$call$mtry,          # integer, Number of variables randomly sampled as candidates at each split. Note that the default is to use tuneRF function of randomForest for each local classifier
                                      ntree        = try_Run_HRF$call$ntree,         # number of trees to grow in each local classifier
                                      importance   = try_Run_HRF$call$importance,    # Should importance of predictors be assessed?
                                      proximity    = try_Run_HRF$call$proximity,     # should the proximity be calcualted? 
                                      keep.forest  = try_Run_HRF$call$keep.forest,   
                                      keep.inbag   = try_Run_HRF$call$keep.inbag    
                                     )


#################################################
### Try the Predict_New function              ###
#################################################

Try_Predict_New =   Predict_New(Hie_RF,                                         # object of class Hier.Random.Forest - the output of Run_HRF
                         New_Data         ,                #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                         New_Data_Case_ID = 1,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                         New_Data_Exp_Var = c(2:ncol(New_Data)),  # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the Hie_RF
                         Crisp_Rule  = c("Multiplicative_Majority","Setpwise_Majority")
)


#################################################
### Try the HRF_PEerformance_New_DAta function ###
#################################################

Try_Predict_New =   HRF_Performance_New_Data(Hie_RF=,                                         # object of class Hier.Random.Forest - the output of Run_HRF
                                New_Data=         ,                #  data frame containing  cases that were note a part of the original traning set, for which the prpoportion of votes should be extracted
                                New_Data_Case_ID = 1,                   # Integer, specifying the column number that contains the Case_ID in the New_Data data frame
                                New_Data_Exp_Var = c(2:ncol(New_Data)),  # vector of integers containg the coulmn numbers that contains the same explanatory variables as those used in the Hie_RF
                                Crisp_Rule  = c("Multiplicative_Majority","Setpwise_Majority")
)





