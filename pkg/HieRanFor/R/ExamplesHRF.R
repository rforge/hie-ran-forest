
##########################################################
## Examples for all function of the packge HieRanFor   ###
##                                                     ###
## Date:    24 Apr. 2015                               ###
## Author:  Yoni Gavish ,gavishyoni@gmail.com          ###
##########################################################

ExamplesHRF = function(){


############################################
##  \\\ Function: RandomHRF  ///          ##
##  Create a random data set              ## 
############################################

set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20,
                        tree.depth     = 4)

train.data <- random.hRF$train.data
new.data   <- random.hRF$new.data
random.hRF$call$tree.depth 
#  example with new.data.observed =TRUE
set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 15,
                        tree.depth     = 6,
                        new.data.observed = TRUE)
train.data <- random.hRF$train.data
new.data   <- random.hRF$new.data   

########################################################
##  \\\ Function: RunHRF  ///                         ##
##  run the hierarchical randomForest classifer       ## 
########################################################

# create random HRF data
set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data

# Run the Hierarchial randomForest
hie.RF.random <- RunHRF(train.data = train.data, 
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# S3 method for plot -> the class hierarchy
plot(hie.RF.random) 

# extracting information
lRF.info         <- hie.RF.random$hier.struc$lRF.info
nodes.info       <- hie.RF.random$hier.struc$nodes.info
unique.path      <- hie.RF.random$hier.struc$unique.path
train.data.ready <- hie.RF.random$train.data.ready
case.ID          <- hie.RF.random$case.ID
path.name        <- hie.RF.random$path.name
hie.levels       <- hie.RF.random$hie.levels
exp.var          <- hie.RF.random$exp.var
all.local.RF     <- hie.RF.random$all.local.RF
order.local.RF   <- hie.RF.random$order.local.RF
fun.call         <- hie.RF.random$call

# extracting the info for local classifier C.2
c.2.local.classifer <- all.local.RF[[order.local.RF[
                         order.local.RF$classifier.ID == "C.2", 2]]]

# structure for each local classifier
# info on the local classifier
c.2.local.lRF.info <- c.2.local.classifer$local.lRF.info 
# case.ID that were used to train the randomForest
c.2.local.case.ID  <- c.2.local.classifer$local.data 
# object of class randomForest    
c.2.local.RF       <- c.2.local.classifer$local.RF       
class(c.2.local.RF)

################
# the OliveOIlHie dataset contains terminal nodes at levels 2 and 3
# RunHRF Will return an error if internal.end.path is not set to TRUE

data(OliveOilHie)
# don't run - an error message is returned

# hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#                     case.ID           = "case.ID",
#                     hie.levels        = c(2:4),
#                     mtry              = "tuneRF2",
#                     internal.end.path = FALSE)

# no error message
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE)

plot(x = hie.RF.OO, text.size = 9, split.text = 10)

########################################################
##  \\\ Function: tuneRF2.HRF  ///                    ##
##  the tuneRF function of randomForest after         ##
##  correcting for error relating to errorOld=0       ## 
########################################################

data(OliveOilHie)
set.seed(250)
# note the "error in if (Improve > improve)..."
# further note the OOB error=0% and the 'Nan' in the printed info from tuneRF
# don't run
# hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#                     case.ID           = "case.ID",
#                     hie.levels        = c(2:4),
#                     mtry              = "tuneRF",
#                     internal.end.path = TRUE)
 
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE)

########################################################
##  \\\ Function: plot.HRF  ///                       ##
##  Plot the class hierarchy.                         ##
##  S3 plot method for class HRF                      ## 
########################################################

# create random HRF data
set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data
# run HRF
hie.RF.random <- RunHRF(train.data = train.data, 
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# S3 method for plot
plot.hie.RF.random <- plot(hie.RF.random)

# further editing of the plot with ggplot2
plot.hRF.tree.ran <- plot.hie.RF.random$plot
class(plot.hRF.tree.ran)
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
   ggtitle("The Class Hierarchy, internal and 
           terminal nodes and all local classifiers")
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     theme(plot.title   = element_text(lineheight=.8, 
                                                       face="bold", 
                                                       color="red"))
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     theme(axis.title.y = element_text(size = 20, 
                                                       colour="blue"))
plot.hRF.tree.ran
# the plot uses the following coordinates:
# x runs from 1 to the number of terminal nodes + 1
# y runs from 1 to the number of levels in the class hierarchy + 1
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     geom_text(data=NULL, 
                               aes(x     = 1, 
                                   y     = random.hRF$call$tree.depth + 1, 
                                   label = "Top-\nLeft"),
                               size = 5,
                               color = "magenta")
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     geom_text(data=NULL, 
                               aes(x     = 1, 
                                   y     = 1, 
                                   label = "Bottom-\nLeft"),
                               size = 5,
                               color = "magenta")
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     geom_text(data=NULL, 
                               aes(x = random.hRF$call$num.term.nodes + 1, 
                                   y = random.hRF$call$tree.depth + 1, 
                                   label = "Top-\nRight"),
                               size = 5,
                               color = "magenta")
plot.hRF.tree.ran <- plot.hRF.tree.ran + 
                     geom_text(data=NULL, 
                               aes(x = random.hRF$call$num.term.nodes + 1, 
                                   y = 1,
                                   label = "Bottom-\nRight"),
                               size = 5,
                               color = "magenta")
plot.hRF.tree.ran 
 
################
#example with a the OliveOilHie dataset
data(OliveOilHie)
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE,
                    ntree=20)

plot(x          = hie.RF.OO,
     text.size  = 9,
     split.text = 10)    

########################################################
##  \\\ Function: ImportanceHie  ///                  ##
##  Importance value of each explanatory              ##
##  variable in each local classifer                  ## 
########################################################

data(OliveOilHie)
hie.RF.OO <- RunHRF(train.data       = OliveOilHie,
                   case.ID           = "case.ID",
                   hie.levels        = c(2:4),
                   mtry              = "tuneRF2",
                   internal.end.path = TRUE)

impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
                                 format.out = c("col.4.out"))

plot(hie.RF.OO)
PlotImportanceHie(input.data = impor.hie.RF.OO,
                  X.data     = 2,                                     
                  Y.data     = 3,                            
                  imp.data   = 4,
                  plot.type  = "Tile",
                  X.Title = c("Parent node Name"),
                  Y.Title = c("Explanatory variable"),
                  imp.title = c("Mean \n Decrease \n in \n Accuracy"))

# table format
impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
                                 format.out = c("table.out"))

# both output formats
impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
                                 format.out = c("col.4.out","table.out"))

import.col.4.for <- impor.hie.RF.OO$imp.var.4.col
import.table.for <- impor.hie.RF.OO$imp.val.table

########################################################
##  \\\ Function: PlotImportanceHie  ///              ##
##  Plot the variable importance values using a       ##
##  heat-map (Tile) or a Bubble plot                  ## 
########################################################

set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data

hie.RF.random <- RunHRF(train.data = train.data, 
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
                         
Importance.hie.RF <- ImportanceHie(hie.RF     = hie.RF.random,
                                   format.out = c("col.4.out"))

PlotImportanceHie(input.data = Importance.hie.RF,
                  X.data     = 2,                                     
                  Y.data     = 3,                            
                  imp.data   = 4,
                  plot.type  = "Tile",
                  supp.warn  = FALSE) 

# Tile, black and white
# label with the classifier.ID instead of the parent node name.
impor.plot.tile <- PlotImportanceHie(input.data        = Importance.hie.RF,
                                     X.data            = 1,                                    
                                     Y.data            = 3,                            
                                     imp.data          = 4,
                                     plot.type         = "Tile",
                                     low.col           = "white" ,                            
                                     high.col          = "black",
                                     geom.tile.bor.col = "gray20",
                                     supp.warn         = TRUE) 
                                      
# further editing of the plot with ggplot2
impor.plot.tile.2 <- impor.plot.tile$plot + 
                     theme(axis.title.y = element_text(size   = 20,
                                                       colour = "red"))
impor.plot.tile.2 <- impor.plot.tile.2 + 
  ggtitle("The mean decrease in accuracy of each 
  explanatory variable in each local classifier")
impor.plot.tile.2 

#Bubble
PlotImportanceHie(input.data = Importance.hie.RF,
                  X.data     = 2,                                    
                  Y.data     = 3,                            
                  imp.data   = 4,
                  plot.type  = "Bubble")

#Bubble, black and white
PlotImportanceHie(input.data = Importance.hie.RF,
                  X.data     = 2,                                     
                  Y.data     = 3,                            
                  imp.data   = 4,
                  plot.type  = "Bubble",
                  pos.col    = "black",                                        
                  zero.col   = "gray50",                                         
                  neg.col    = "white") 

########################################################
##  \\\ Function: predict.HRF  ///                    ##
##  predict the proportion of votes for each case     ##
##  in each local classifier. S3 method for predict   ## 
########################################################

set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data
new.data   <- random.hRF$new.data

# run HRF
hie.RF.random <- RunHRF(train.data = train.data,
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# predict only for the training data
Predict.HRF.train    <- predict(hie.RF.random)
prop.votes.lRF.train <- Predict.HRF.train$prop.vote.train

# predict only for new.data
Predict.HRF.new <- predict(object           = hie.RF.random,
                           train.predict    = FALSE,                
                           new.data         = new.data,                
                           new.data.case.ID = 1,                   
                           new.data.exp.var = c(2:ncol(new.data)),  
                           bind.train.new   = FALSE)
prop.votes.lRF.new <- Predict.HRF.new$prop.vote.new

# predict for training and new data + bind
Predict.HRF.both <- predict(object = hie.RF.random,                                 
                            train.predict    = TRUE,                
                            new.data         = new.data,                
                            new.data.case.ID = 1,                   
                            new.data.exp.var = c(2:ncol(new.data)),  
                            bind.train.new   = TRUE)
attributes(Predict.HRF.both)
prop.votes.lRF.both <- Predict.HRF.both$prop.vote.full

# the prop.votes.lRF.both data frame contains 
# one additional column: 'train.or.test'
# cases from the training data sat are listed as train
# cases from the new.data data set are listed as test
names(prop.votes.lRF.both)[1]
levels(prop.votes.lRF.both$train.or.test)

########################################################
##  \\\ Function: GetMultPropVotes  ///               ##
##  For each case, the multiplicative proportion      ##
##  of votes along each path from tree root to        ## 
##  terminal nodes                                    ##
########################################################

set.seed(354)
# create a random training dataset
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data
# run HRF and predict
hie.RF.random <- RunHRF(train.data = train.data, 
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)),
                        mtry="tuneRF2")
prop.votes.lRF.train <- predict(hie.RF.random)$prop.vote.train

# multiply path only until the deepest level of the class hierarchy
multi.prop.votes.full <- GetMultPropVotes(
                         prop.vote   = prop.votes.lRF.train, 
                         unique.path = hie.RF.random$hier.struc$unique.path,  
                         all.levels  = FALSE)
multi.prop.votes.L4 <- multi.prop.votes.full[[1]]

#######
data(OliveOilHie)

hie.RF.OO <- RunHRF(train.data = OliveOilHie, 
                    case.ID    = "case.ID", 
                    hie.levels = c(2:4),
                    internal.end.path = TRUE,
                    mtry= "tuneRF2")

prop.votes.OO <- predict(hie.RF.OO)$prop.vote.train
mult.prop.OO  <- GetMultPropVotes(prop.vote   = prop.votes.OO,            
                           unique.path = hie.RF.OO$hier.struc$unique.path,      
                           all.levels  = TRUE)
plot(hie.RF.OO)
multi.prop.votes.L1 <-mult.prop.OO[["prop.multiplicative.votes.L1"]]
names(multi.prop.votes.L1)[3:ncol(multi.prop.votes.L1)]

multi.prop.votes.L2 <- mult.prop.OO[["prop.multiplicative.votes.L2"]]
names(multi.prop.votes.L2)[3:ncol(multi.prop.votes.L2)]

# note that terminal nodes from level 2 appears in L3 as well. 
multi.prop.votes.L3 <- mult.prop.OO[["prop.multiplicative.votes.L3"]]
names(multi.prop.votes.L3)[3:ncol(multi.prop.votes.L3)]

########################################################
##  \\\ Function: PerformanceHRF  ///                 ##
##  flat and hierarchical performance measures for    ##   
##  3 types of crisp classification                   ##
########################################################

# create a random HRF dataset and run HRF analysis
set.seed(354)
random.hRF    <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data    <- random.hRF$train.data
hie.RF.random <- RunHRF(train.data = train.data,
                        case.ID    = "case.ID",
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# assess performance
perf.hRF.random <- PerformanceHRF(hie.RF     = hie.RF.random,
                                  perm.num   = 20,
                                  div.print  = 5)

### Extract values ###
names(perf.hRF.random)
raw.vote.random               <- perf.hRF.random$raw.vote
crisp.case.class.random       <- perf.hRF.random$crisp.case.class        
hie.performance.random        <- perf.hRF.random$hie.performance
multiplicative.prop.random    <- perf.hRF.random$multiplicative.prop     
nodes.measures.columns.random <- perf.hRF.random$nodes.measures.columns
hie.perf.call.random          <- perf.hRF.random$call

#### example with the OliveOilHie dataset
data(OliveOilHie)
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE)

perf.hRF.olive <- PerformanceHRF(hie.RF    = hie.RF.OO,
                                 crisp.rule  = c("multiplicative.majority"))

### Extract values ###
crisp.case.class.olive <- perf.hRF.olive$crisp.case.class        
hie.performance.olive <- perf.hRF.olive$hie.performance

### plotting option ### 
# create a confusion matrix
conf.matr.olive <- as.data.frame(table(crisp.case.class.olive$obs.term.node,
                      crisp.case.class.olive$multiplicative.majority.rule))
# use the PlotImportanceHie to plot the confusion matrix
PlotImportanceHie(input.data = conf.matr.olive,
                  X.data     = 1,                                     
                  Y.data     = 2,                            
                  imp.data   = 3,
                  plot.type  = "Tile",
                  X.Title    = c("Observed"),
                  Y.Title    = c("mMultiplicative majority rule"),
                  imp.title  = c("frequency"),
                  low.col    = "darkslategray4",
                  high.col   = "red",
                  geom.tile.bor.col = "gray20")

########################################################
##  \\\ Function: PredictNewHRF  ///                  ##
##  flat and hierarchical performance measures for    ##   
##  3 types of crisp classification for new.data for  ##
##  which the observed calss are known                ##
########################################################

# create a random HRF dataset and RunHRF
set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
train.data <- random.hRF$train.data
new.data   <- random.hRF$new.data
hie.RF.random <- RunHRF(train.data = train.data, 
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# predict for new.data
pred.new.hRF <- PredictNewHRF(hie.RF     = hie.RF.random,
                              new.data   = new.data,
                              crisp.rule = c("stepwise.majority",
                                             "multiplicative.majority" , 
                                            "multiplicative.permutation"),
                              perm.num   = 10,
                              div.print  = 2)
 
# extract values
names(pred.new.hRF)
pred.new.votes       <- pred.new.hRF$raw.votes
pred.new.mult.prop   <- pred.new.hRF$multiplicative.prop
pred.new.crisp.class <- pred.new.hRF$crisp.case.class
pred.new.call        <- pred.new.hRF$call

########################################################
##  \\\ Function: PerformanceNewHRF  ///              ##
##  flat and hierarchical performance measures for    ##   
##  3 types of crisp classification for new.data for  ##
##  which the observed calss are known                ##
########################################################

# create a random HRF dataset and RunHRF
set.seed(354)
random.hRF <- RandomHRF(num.term.nodes = 20, 
                        tree.depth = 4, 
                        new.data.observed = TRUE)
train.data <- random.hRF$train.data
new.data   <- random.hRF$new.data
hie.RF.random <- RunHRF(train.data = train.data,
                        case.ID    = "case.ID", 
                        hie.levels = c(2:(random.hRF$call$tree.depth + 1)))

# assess performance for the new.data
perf.new.data <- PerformanceNewHRF(hie.RF           = hie.RF.random,
                 new.data         = new.data ,
                 new.data.case.id = 1,
                 new.data.hie     = c(2:(random.hRF$call$tree.depth + 1)),
                 crisp.rule       = c("stepwise.majority",
                                      "multiplicative.majority", 
                                      "multiplicative.permutation"),
                 perm.num         = 10,
                 div.print        = 2,
                 per.index        = c("flat.measures", "hie.F.measure"),
                 by.node          = TRUE)

# extract the data
names(perf.new.data)
perf.new.votes      <- perf.new.data$raw.votes
perf.new.crisp      <- perf.new.data$crisp.case.class  
perf.new.hie.perf   <- perf.new.data$hie.performance
perf.new.mult.prop  <- perf.new.data$multiplicative.prop
perf.new.nodes.meas <- perf.new.data$nodes.measures.columns
perf.new.call       <- perf.new.data$call 

##########################################################
##  \\\ Function: PerformanceFlatRF  ///                ##
##  Re-runs hie.RF as flat classifcation                ##
##  predict, turn to crisp class and asses performance  ##
##########################################################

# analyse the OliveOilHie dataset
data(OliveOilHie)
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE)

# run and assess performance as flat classification
flat.RF.OO <- PerformanceFlatRF(hie.RF     = hie.RF.OO,                                   
                                per.index  = c("flat.measures", 
                                               "hie.F.measure"),                         
                                crisp.rule = c("multiplicative.majority", 
                                               "multiplicative.permutation"),   
                                perm.num   = 10,         
                                div.print  = 2)
# extract values
names(flat.RF.OO)
flat.RF.OO.RF        <- flat.RF.OO$flat.RF # object of class randomForest
votes.flat           <- flat.RF.OO$flat.RF$votes 
flat.RF.OO.crisp     <- flat.RF.OO$crisp.case.class
hie.perf.flat        <- flat.RF.OO$hie.performance
flat.RF.OO.nodes.mea <- flat.RF.OO$nodes.measures.columns
flat.RF.OO.call      <- flat.RF.OO$call

# compare the hie.perf.flat to the HRF analysis
# Performance of the hierarchical randomForest
perf.hRF.OO <- PerformanceHRF(hie.RF     = hie.RF.OO,
                              crisp.rule  = c("multiplicative.majority", 
                                              "multiplicative.permutation"),
                              perm.num    = 10,
                              div.print   = 2)

hie.perf.HRF   <- perf.hRF.OO$hie.performance
 
# Despite the overall high performance, HRF is slightly better...
comp.perf <- data.frame(model = c("Flat","HRF"))
join.perf <- rbind(hie.perf.flat[1, c("Accuracy","Kappa","h.F.measure")],
                   hie.perf.HRF[1, c("Accuracy","Kappa","h.F.measure")])
comp.perf <- cbind(data.frame(model = c("Flat","HRF")),
                   join.perf)
comp.perf

##########################################################
##  \\\ Function: HieFMeasure  ///                      ##
##  Re-runs hie.RF as flat classifcation                ##
##  predict, turn to crisp class and asses performance  ##
##########################################################

data(OliveOilHie)
hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
                    case.ID           = "case.ID",
                    hie.levels        = c(2:4),
                    mtry              = "tuneRF2",
                    internal.end.path = TRUE)

# extract the crisp class using PerformanceHRF
perf.hRF.OO <- PerformanceHRF(hie.RF     = hie.RF.OO,
                              per.index  = c("hie.F.measure"),      
                              crisp.rule = c("stepwise.majority"))   
crisp.OO <- perf.hRF.OO$crisp.case.class

# Join factor levels of the observed and predicted 
joined.levels <- JoinLevels(vector.1 = crisp.OO$stepwise.majority.rule,
                            vector.2 = crisp.OO$obs.term.node)

# create the confusionMatrix object
library(caret)
conf.matr <- confusionMatrix(data      = joined.levels$vector.1,
                             reference = joined.levels$vector.2,
                             dnn       = c("Prediction", "Observed"))

# the HieFMeasure
hie.F.value <- HieFMeasure(conf.matr   = conf.matr,       
                           unique.path = hie.RF.OO$hier.struc$unique.path,     
                           beta.h.F    = 1,    
                           by.node     = TRUE)

# compare to the output of PerformanceHRF
hie.performance <- perf.hRF.OO$hie.performance

########################################################
##  \\\ Function: JoinLevels  ///                     ##
##  Join the factor levels of two vectors             ##
########################################################

# create two vectors
vec.1 <- as.factor(rep(c("a","b","c"),10))
vec.2 <- as.factor(rep(c("g","a","f","b"),20))

# run the function
new.Vec <- JoinLevels(vec.1,vec.2)

# Re-assign to the original vectors
vec.1 <- new.Vec[[1]]
vec.2 <- new.Vec[[2]]

# note the levels
levels(vec.1)
levels(vec.2)

}


