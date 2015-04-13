#' The importance value of each explanatory variable in each local classifier
#' 
#' This function extracts the importance value of each variable for each local 
#' classifier. Requires setting \code{importance = TRUE} when running
#' \code{\link{RunHRF}}. Relies on the function \code{\link{importance}} in the
#' \code{randomForest} package.
#' 
#' 
#' @param hie.RF  Object of class \code{"HRF"} - the output of \code{RunHRF}.
#'   
#' @param format.out The format of the output. Can be either \code{"col.4.out"}
#'   (default), \code{"table.out"} or both. See Value for details.
#'   
#' @param scale.imp  Logical, if \code{TRUE}, variable importance is scaled by
#'   the standard error. Default is \code{FALSE}.
#'   
#' See \code{\link{importance}} in the \code{randomForest} package for details.
#' @param \dots   Optional parameters to be passed to low level functions.
#'   
#' @details  Returns the mean decrease in accuracy (\code{type=1} in
#'   \code{randomForest::importance}).
#'   
#' @return
#' \item{imp.val}{A data frame or list containing the importance of each
#' explanatory variable in each local classifier. Output structure depends on
#' the settings of \code{format.out}.}
#' 
#'  \describe{ 
#'  \item{\strong{1.}   If \code{format.out = "col.4.out"}}{Data frame with four
#'  columns:
#'  
#'      \tabular{llllllllll}{
#'      \tab \tab \tab \tab \tab \tab \tab \code{"classifier.ID"} \tab \tab The
#'      name of the local classifier. \cr
#'      
#'      \tab \tab \tab \tab \tab \tab \tab \code{"par.level"}     \tab \tab The
#'      name of the parent node in the local classifier. \cr
#'      
#'      \tab \tab \tab \tab \tab \tab \tab \code{"expl.var"}      \tab \tab The
#'      name of the explanatory variable. \cr
#'      
#'      \tab \tab \tab \tab \tab \tab \tab \code{"mean.dec.accu"} \tab \tab the
#'      mean decrease in accuracy.  \cr }}
#'      
#'  \item{\strong{2.}   If \code{format.out = "table.out"}}{Data frame, with the
#'  first column containing the name of the local classifier and the rest of the
#'  columns the names and variable importance of each explanatory variable.}
#'  
#'  \item{\strong{3.}   If \code{format.out = c("col.4.out","table.out")}}{A
#'  list containing the two data frames, under \code{"var.imp.4.col"} and
#'  \code{"var.imp.table"}, respectively.}}  
#'     
#'  @author Yoni Gavish <gavishyoni@@gmail.com>
#'  
#'  @seealso \code{\link{PlotImportanceHie}} for a \code{ggplo2} based plotting
#'    of hierarchical importance values.
#'    
#'  @examples
#' data(OliveOilHie)
#' hie.RF.OO <- RunHRF(train.data       = OliveOilHie,
#'                    case.ID           = "case.ID",
#'                    hie.levels        = c(2:4),
#'                    mtry              = "tuneRF2",
#'                    internal.end.path = TRUE)
#'
#' impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
#'                                  format.out = c("col.4.out"))
#'
#' plot(hie.RF.OO)
#' PlotImportanceHie(input.data = impor.hie.RF.OO,
#'                   X.data     = 2,                                     
#'                   Y.data     = 3,                            
#'                   imp.data   = 4,
#'                   plot.type  = "Tile",
#'                   X.Title = c("Parent node Name"),
#'                   Y.Title = c("Explanatory variable"),
#'                   imp.title = c("Mean \n Decrease \n in \n Accuracy"))
#'
#' # table format
#' impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
#'                                  format.out = c("table.out"))
#'
#' # both output formats
#' impor.hie.RF.OO <- ImportanceHie(hie.RF     = hie.RF.OO,
#'                                  format.out = c("col.4.out","table.out"))
#' 
#' import.col.4.for <- impor.hie.RF.OO$imp.var.4.col
#' import.table.for <- impor.hie.RF.OO$imp.val.table
#'  
#'  @export 
#'  




# Extract the importance values for each local classifier using 
# importance of RandomForest- mean decrease in accuracy
ImportanceHie=function(hie.RF,  # Object of class HRF the output of RunHRF
                       format.out = c("col.4.out"), 
                       scale.imp=FALSE,   # if FALSE, variable importance is not scaled by the standard error.
                       ...)
{ # start function
  
  #require(randomForest)
  
  #### check input
  
  # check class of hie.RF
  if(class(hie.RF) != "HRF")
  {stop(paste("\n", "ImportanceHie:  hie.RF should be of class HRF", "\n", sep=""))}
  
  # check format.out
  if(length(intersect(format.out, c("col.4.out", "table.out"))) == 0)
  {stop(paste("\n", "ImportanceHie:  format.out should be either col.4.out, table.out or both.  ", "\n", sep=""))}
  
  if (length(intersect(format.out, c("col.4.out", "table.out"))) == 1 && length(format.out) > 1)
  {stop(paste("\n", "ImportanceHie:  format.out should be either col.4.out, table.out or both.  ", "\n", sep=""))}
  
  if(!is.logical(scale.imp))
  {cat(paste("\n", "ImportanceHie: scale.imp should be Logical. default of scale.imp=FALSE is used", "\n", sep=""))
   scale.imp <- FALSE}
  
  # extract the required information from hie.RF
  
  train.data.ready <- hie.RF$train.data.ready
  exp.var          <- hie.RF$exp.var
  lRF.info         <- hie.RF$hier.struc$lRF.info
  all.local.RF     <- hie.RF$all.local.RF
  
  # create the imp.val.table data frame
  imp.val.table           <- matrix(nrow = dim(lRF.info)[1], 
                                    ncol = length(exp.var))
  imp.val.table           <- as.data.frame(imp.val.table)
  colnames(imp.val.table) <- colnames(train.data.ready[exp.var])
  
  # create the imp.var.4.col data frame
  imp.var.4.col <- cbind(lRF.info[1, c(1, 3)],
                         colnames(train.data.ready[exp.var]),
                         row.names = NULL)
  
  for (k in 2:dim(lRF.info)[1])
  { k1               <- cbind(lRF.info[k, c(1, 3)],
                              colnames(train.data.ready[exp.var]),
                              row.names = NULL)
    imp.var.4.col <- rbind(imp.var.4.col, k1)
  }
  
  imp.var.4.col                <- as.data.frame(imp.var.4.col)
  imp.var.4.col[,4]            <- NA
  colnames(imp.var.4.col)[3:4] <- c("expl.var", "mean.dec.accu")
                         
  for (i in 1: dim(lRF.info)[1]) # loop the runs on each local classifier
  {
    #Extract the importance values for the local clasifer 
    local.RF      <- all.local.RF[[i]]$local.RF # the focal local randomForest object
    local.imp     <- as.data.frame(importance(local.RF, scale=scale.imp))  # the importance data frame for the local RF from the package randomForest  
    mean.dec.accu <- local.imp$MeanDecreaseAccuracy # the mean decrease in accuracy
    
    # insert the importance values in the two output data frames
    imp.val.table[i, ]                          <- mean.dec.accu[]
    imp.var.4.col[c((1 + length(exp.var) * (i - 1)):(i * length(exp.var))), 4] <- mean.dec.accu[]
  }
  
  # add a column with classifier.ID to the imp.val.table data frame
  imp.val.table              <- cbind(lRF.info[, 1], imp.val.table)
  colnames(imp.val.table)[1] <- "classifier.ID"
  
  # create the return list
  
  if(length(intersect(format.out, c("col.4.out", "table.out"))) == 2){
    imp.val <- list(imp.var.4.col = imp.var.4.col,
                    imp.val.table = imp.val.table)}
  if(length(format.out) == 1){
    if(format.out == c("col.4.out")){imp.val <- imp.var.4.col}
    if(format.out == c("table.out")){imp.val <- imp.val.table}
  }
 
  
  return(imp.val)
  
} # end ImportanceHie function