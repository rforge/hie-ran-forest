#' The importance value of each explanatory variable in each local classifer
#' 
#' This function extracts the importance value of each variable for each local
#' classifier. Requires setting \code{importance = TRUE} when running \code{\link{Run_HRF}}.
#' Relies on the function \code{\link{importance}} in the \code{randomForest} package.
#' 
#' 
#' @param hie.RF  Object of class \code{"Hier.Random.Forest"} - the output of \code{Run_HRF}.
#' @param format.out The format of the output. can be either \code{"4.col.out"} (default), \code{"table.out"} or both. See Value for details. 
#' @param scale.imp  Logical, if \code{TRUE}, variable importance is scaled by the standard error. Default is \code{FALSE}. 
#' See \code{\link{importance}} in the \code{randomForest} package for details.
#' @param ...   Optional parameters to be passed to the low level functions.
#'   
#' @details  Returns the mean decrease in accuracy (\code{type=1} in \code{randomForest::importance}).
#'   
#' @return
#' \item{imp.val}{A data frame or list containing the importance of each explanatory varaible in each local classifier.
#'  \describe{ 
#'  \item{\strong{1.}   If \code{format.out = "4.col.out"}}{Data frame with four columns:}
#'      {\tabular{lll}{
#'       \code{"Classifier_ID"} \tab \tab The name of the local classifer. \cr
#'       \code{"Par_Level"}     \tab \tab The name of the parent node in the local classifer. \cr
#'       \code{"Expl_Var"}      \tab \tab The name of theexplanatory vatiable. \cr
#'       \code{"Mean_Dec_Accu"} \tab \tab the mean decrease in accruacy.  \cr
#'        }}
#'  \item{\strong{2.}   If \code{format.out = "table.out"}}{Data frame, with the first column containing the name of the local classifer and the rest of the columns the names and variable importance of each explantory variable.} 
#'  \item{\strong{3.}   If \code{format.out = c("4.col.out","table.out")}}{A list containing the two data frames, under \code{"var.imp.4.col"} and \code{"var.imp.table"}, respectively.}
#'    }}  
#'     
#'  @author Yoni Gavish <gavishyoni@@gmail.com>




# Extract the importance values for each local classifer using importance of RandomForest- mean decrease in accuracy
Hie_Importance=function(hie.RF,  # Object of class Hier.Random.Forest the output of Run_HRF
                        format.out = c("4.col.out"), 
                        scale.imp=FALSE,   # if FALSE, variable importance is not scaled by the standard error.
                        ...)
{ # start function
  
  #require(randomForest)
  
  #### check input
  
  # check class of Hie_RF
  if(class(hie.RF)!="Hier.Random.Forest")
  {stop(paste("\n","Hie_Importance:  hie.RF should be of class Hier.Random.Forest","\n",sep=""))}
  
  # check format.out
  if(length(intersect(format.out,c("4.col.out","table.out")))==0)
  {stop(paste("\n","Hie_Importance:  format.out should be either 4.col.out, table.out or both.  ","\n",sep=""))}
  
  if (length(intersect(format.out,c("4.col.out","table.out")))==1 & 
        length(format.out)>1)
  {stop(paste("\n","Hie_Importance:  format.out should be either 4.col.out, table.out or both.  ","\n",sep=""))}
  
  if(!is.logical(scale.imp))
  {cat(paste("\n", "Hie_Importance: scale.imp should be Logical. default of scale.imp=FALSE is used","\n",sep=""))
   scale.imp <- FALSE}
  
  # extract the required information from hie.RF
  
  train.data.ready <- hie.RF$Train_Data_Ready
  exp.var <- hie.RF$Exp_Var_2
  lRF.info <-  hie.RF$Hier_Struc$LRF_Info
  all.local.RF <- hie.RF$All_Local_RF
  
  
  # create the imp.val.table data frame
  imp.val.table           <- matrix(nrow=dim(lRF.info)[1], 
                                    ncol=length(exp.var))
  imp.val.table           <- as.data.frame(imp.val.table)
  colnames(imp.val.table) <- colnames(train.data.ready[exp.var])
  
  # create the Imp_Var_4_col data frame
  imp.var.4.col <- cbind(lRF.info[1,c(1,3)],
                         colnames(train.data.ready[exp.var]),
                         row.names = NULL)
  
  for (k in 2:dim(lRF.info)[1])
  { k1               <- cbind(lRF.info[k,c(1,3)],
                              colnames(train.data.ready[exp.var]),
                              row.names = NULL)
    imp.var.4.col <- rbind(imp.var.4.col,
                              k1)
  }
  
  imp.var.4.col                <- as.data.frame(imp.var.4.col)
  imp.var.4.col[,4]            <- NA
  colnames(imp.var.4.col)[3:4] <- c("Expl_Var","Mean_Dec_Accu")
     i=2                      
  for (i in 1: dim(lRF.info)[1]) # loop the runs on each local classifer
  {
    #Extract the importance values for the local clasifer 
    local.RF      <- all.local.RF[[i]]$Local_RF # the focal local randomForest object
    local.imp     <- as.data.frame(randomForest::importance(local.RF,scale=scale.imp))  # the importance data frame for the local RF  
    mean.dec.accu <- local.imp$MeanDecreaseAccuracy # the mean decrease in accuracy
    
    # insert the importance values in the two output data frames
    imp.val.table[i,]                          <- mean.dec.accu[]
    imp.var.4.col[c((1+length(exp.var)*(i-1)):(i*length(exp.var))),4] <- mean.dec.accu[]
  }
  
  # add a column with Classifier_ID to the imp.val.table data frame
  imp.val.table              <- cbind(lRF.info[,1], imp.val.table)
  colnames(imp.val.table)[1] <- "Classifier_ID"
  
  # create the return list
  
  if(length(intersect(format.out,c("4.col.out","table.out")))==2){
    imp.val <- list(imp.var.4.col = imp.var.4.col,
                    imp.val.table = imp.val.table)}
  if(length(format.out)==1){
    if(format.out==c("4.col.out")){imp.val <- imp.var.4.col}
    if(format.out==c("table.out")){imp.val <- imp.val.table}
  }
 
  
  return(imp.val)
  
} # end Hie_Importance function