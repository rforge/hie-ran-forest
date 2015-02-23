#' the tuneRF function of randomForest after correcting for error relating to
#' errorOld=0
#' 
#' The \code{\link{tuneRF}} function of the \code{randomForest} package may 
#' return an error when \code{errorOld} reaches 0 when exploring \code{mtry}. 
#' This function returns the \code{mtry} reached before the error. In addition,
#' the default option for \code{"plot"} in \code{tuneRF} is changes to
#' \code{FALSE}.
#' 
#' @param x             See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param y             See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param mtryStart     See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param ntreeTry      See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param stepFactor    See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param improve       See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param trace         See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param plot          See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param doBest        See \code{\link{tuneRF}} in the package
#'   \code{randomForest} for details.
#'   
#' @param \dots   Optional parameters to be passed to low level functions.
#' 
#' @return See \code{\link{tuneRF}} in the package \code{randomForest} for
#'   details.
#'   
#' @examples
#' data(OliveOilHie)
#' set.seed(250)
#' # note the "error in if (Improve > improve)..."
#' # further note the OOB error=0% and the 'Nan' in the printed info from tuneRF
#' # don't run
#' # hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#' #                     case.ID           = "case.ID",
#' #                     hie.levels        = c(2:4),
#' #                     mtry              = "tuneRF",
#' #                     internal.end.path = TRUE)
#' 
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE)
#' 
#' @aliases tuneRF
#' @export
#' 

# function, tuneRF of randomForest, after correcting for error relating to errorOld=0

tuneRF2=function (x, y, mtryStart = if (is.factor(y)) floor(sqrt(ncol(x))) else floor(ncol(x)/3), 
          ntreeTry = 50, stepFactor = 2, improve = 0.05, trace = TRUE, 
          plot = FALSE, doBest = FALSE, ...) 
{
  if (improve < 0) 
    stop("improve must be non-negative.")
  classRF <- is.factor(y)
  errorOld <- if (classRF) {
    randomForest::randomForest(x, y, mtry = mtryStart, ntree = ntreeTry, 
                 keep.forest = FALSE, ...)$err.rate[ntreeTry, 1]
  }
  else {
    randomForest::randomForest(x, y, mtry = mtryStart, ntree = ntreeTry, 
                 keep.forest = FALSE, ...)$mse[ntreeTry]
  }
  if (errorOld < 0) 
    stop("Initial setting gave 0 error and no room for improvement.")
  if (trace) {
    cat("mtry =", mtryStart, " OOB error =", if (classRF) 
      paste(100 * round(errorOld, 4), "%", sep = "")
      else errorOld, "\n")
  }
  oobError <- list()
  oobError[[1]] <- errorOld
  names(oobError)[1] <- mtryStart
  for (direction in c("left", "right")) {
    if (trace) 
      cat("Searching", direction, "...\n")
    Improve <- 1.1 * improve
    mtryBest <- mtryStart
    mtryCur <- mtryStart
    while (Improve >= improve) {
      mtryOld <- mtryCur
      mtryCur <- if (direction == "left") {
        max(1, ceiling(mtryCur/stepFactor))
      }
      else {
        min(ncol(x), floor(mtryCur * stepFactor))
      }
      if (mtryCur == mtryOld) 
        break
      errorCur <- if (classRF) {
        randomForest::randomForest(x, y, mtry = mtryCur, ntree = ntreeTry, 
                     keep.forest = FALSE, ...)$err.rate[ntreeTry, 
                                                        "OOB"]
      }
      else {
        randomForest::randomForest(x, y, mtry = mtryCur, ntree = ntreeTry, 
                     keep.forest = FALSE, ...)$mse[ntreeTry]
      }
      if (trace) {
        cat("mtry =", mtryCur, "\tOOB error =", if (classRF) 
          paste(100 * round(errorCur, 4), "%", sep = "")
          else errorCur, "\n")
      }
      oobError[[as.character(mtryCur)]] <- errorCur
      #####
      # this is the line I've added and edited. when errorOLd==o the original function returns NA  
      # for errorCur/errorOld. 
      if(errorCur==0){Improve <- improve}
      if(errorCur!=0){Improve <- 1 - errorCur/errorOld}
      
      ########
      cat(Improve, improve, "\n")
      if (Improve > improve) {
        errorOld <- errorCur
        mtryBest <- mtryCur
      }
    }
  }
  mtry <- sort(as.numeric(names(oobError)))
  res <- unlist(oobError[as.character(mtry)])
  res <- cbind(mtry = mtry, OOBError = res)
  if (plot) {
    plot(res, xlab = expression(m[try]), ylab = "OOB Error", 
         type = "o", log = "x", xaxt = "n")
    axis(1, at = res[, "mtry"])
  }
  if (doBest) 
    res <- randomForest::randomForest(x, y, mtry = res[which.min(res[, 
                                                       2]), 1], ...)
  res
}