#' Plot the Hierarchical class structure
#' 
#' This function takes as input an object of class \code{"HRF"}, and plots the
#' structure of the class hierarchy.
#' 
#' @param x                      Object of class \code{"HRF"} - the output
#'   of \code{RunHRF}.
#'   
#' @param rect.width                  Numeric, the width of boxes for each node,
#'   at width=1, the boxes overlap.
#'   
#' @param rect.hieght                 Numeric, the height of the boxes for each
#'   node, centre to centre distance betwen levels is 1.
#'   
#' @param text.size                   Numeric, a scaling number for the size of
#'   text.
#'   
#' @param text.angle                  Numeric, the angle of the text of the
#'   nodes.
#'   
#' @param split.text                  Numeric, the number of characters in nodes
#'   names above which the text is broken to two lines.
#'   
#' @param int.col                     Character, specifying the color of the
#'   fill of the internal nodes.
#'   
#' @param term.col                    Character, specifying the color of the
#'   fill of the terminal nodes.
#'   
#' @param root.col                    Character, specifying the color of the
#'   fill of the \code{tree.root} node.
#'   
#' @param Y.title                     Character, title of the y axis
#' 
#' @param label.nodes                 Logical, if \code{TRUE} (default), the
#'   nodes are labelled.
#'   
#' @param label.classifiers           Logical, if \code{TRUE} (default), the
#'   local classifiers are labelled.
#'   
#' @param class.ID.rect.col           Character, specifying the border color of
#'   the classifer.ID boxes.
#'   
#' @param class.ID.rect.fill          Character, specifying the fill color of
#'   the classifer.ID boxes.
#'   
#' @param class.ID.rect.linetype      Character, specifying the linetype of the
#'   classifer.ID boxes.
#'   
#' @param class.ID.rect.hieght.scale  Numeric, the scaling of the classifer.ID
#'   box relative to the nodes boxe.
#'   
#' @param class.ID.text.size.scale    Numeric, the scaling of the classifer.ID
#'   text relative to the nodes text
#'   
#' @param class.ID.text.color         Character, specifying the color of the
#'   text for the classifer.ID.
#'   
#' @param \dots  Optional parameters to be passed to the low level functions. 
#' 
#' @details Based on \code{\link{ggplot2}}, the function plots the class
#'   hierarchy along with the local classifiers.
#' @return  The function returns a plot. If the function is saved to a new
#'    object, that the plot can be accessed and further edited using $plot on
#'    the new object (see examples for details).
#' @examples
#' # create random HRF data
#' set.seed(354)
#' random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
#' train.data <- random.hRF$train.data
#' # run HRF
#' hie.RF.random <- RunHRF(train.data = train.data, 
#'                         case.ID    = "case.ID", 
#'                         hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
#'
#' # S3 method for plot
#' plot.hie.RF.random <- plot(hie.RF.random)
#' 
#' # further editing of the plot with ggplot2
#' plot.hRF.tree.ran <- plot.hie.RF.random$plot
#' class(plot.hRF.tree.ran)
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'    ggtitle("The Class Hierarchy, internal and 
#'             terminal nodes and all local classifiers")
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      theme(plot.title   = element_text(lineheight=.8, 
#'                                                        face="bold", 
#'                                                        color="red"))
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      theme(axis.title.y = element_text(size = 20, 
#'                                                        colour="blue"))
#' plot.hRF.tree.ran
#' # the plot uses the following coordinates:
#' # x runs from 1 to the number of terminal nodes + 1
#' # y runs from 1 to the number of levels in the class hierarchy + 1
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      geom_text(data=NULL, 
#'                                aes(x     = 1, 
#'                                    y     = random.hRF$call$tree.depth + 1, 
#'                                    label = "Top-\nLeft"),
#'                                size = 5,
#'                                color = "magenta")
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      geom_text(data=NULL, 
#'                                aes(x     = 1, 
#'                                    y     = 1, 
#'                                    label = "Bottom-\nLeft"),
#'                                size = 5,
#'                                color = "magenta")
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      geom_text(data=NULL, 
#'                                aes(x = random.hRF$call$num.term.nodes + 1, 
#'                                    y = random.hRF$call$tree.depth + 1, 
#'                                    label = "Top-\nRight"),
#'                                size = 5,
#'                                color = "magenta")
#' plot.hRF.tree.ran <- plot.hRF.tree.ran + 
#'                      geom_text(data=NULL, 
#'                                aes(x = random.hRF$call$num.term.nodes + 1, 
#'                                    y = 1,
#'                                    label = "Bottom-\nRight"),
#'                                size = 5,
#'                                color = "magenta")
#' plot.hRF.tree.ran 
#' 
#' ################
#' #example with a the OliveOilHie dataset
#' data(OliveOilHie)
#' hie.RF.OO <- RunHRF(train.data        = OliveOilHie,
#'                     case.ID           = "case.ID",
#'                     hie.levels        = c(2:4),
#'                     mtry              = "tuneRF2",
#'                     internal.end.path = TRUE,
#'                     ntree=20)
#' 
#' plot(x          = hie.RF.OO,
#'      text.size  = 9,
#'      split.text = 10)         
#'
#' @author Yoni Gavish <gavishyoni@@gmail.com>    
#' @seealso \code{\link{RunHRF}} for running a hierarchical randomForest,
#'   \code{\link{PerformanceHRF}} for assessing the performance and accuracy of
#'   the HRF.
#'   
#'   
#' @method plot HRF
#' @export 
#' @import ggplot2
#' 



plot.HRF = function(x, 
                    rect.width                 = 0.8,                      # Numeric, the width of boxes for each node, at width=1, the boxes overlap
                    rect.hieght                = 0.5,                      # Numeric, the hieght of the boxes for each node, center to center distance betwen levels is 1. 
                    text.size                  = 7.5,                      # Numeric, a scaling number for the size of text 
                    text.angle                 = 0,                        # Numeric, the angle of the text of the nodes
                    split.text                 = 6,                        # Numeric, the number of characthers in nodes names above which the text is broken to two lines
                    int.col                    = "white",                  # Character, specifying the color of the fill of the internal nodes
                    term.col                   = "gray80",                 # Character, specifying the color of the fill of the terminal nodes
                    root.col                   = "gray50",                 # Character, specifying the color of the fill of the tree root node
                    Y.title                    = "Hierarchical Level\n",   # Character, title of the y axis
                    label.nodes                = TRUE,                     # Logical, should the nodes be labeled?
                    label.classifiers          = TRUE,                     # Logical, should the names of the local classifers be added to the tree?
                    class.ID.rect.col          = "black",                  # Character, specifying the color of the border of the classifer ID boxes
                    class.ID.rect.fill         = "gold",                   # Character, specifying the color of the fill of the classifer ID boxes
                    class.ID.rect.linetype     = "dotted",                 # Character, specifying the linetype of the classifer ID boxes
                    class.ID.rect.hieght.scale = 0.25,                     # Numeric, a scaling of the classifer ID box relative to the nodes boxes 
                    class.ID.text.size.scale   = 0.8,                      # Numeric, a scaling of the classifer ID text relative to the nodes text 
                    class.ID.text.color        = "black",                  # Character, specifying the color of the text for the classifer ID
                    ...)
{
  
  X.min <- X.max <- Y.min <- Y.max <- term.int.node <- label.node <- label.class <- NULL
  #require(ggplot2)
  hie.RF <- x
  # define the local environment
  localenv <- environment()
  
  # extract from hie.RF
  nodes.info  <- hie.RF$hier.struc$nodes.info
  unique.path <- hie.RF$hier.struc$unique.path
  
  # add the num.node column
  work.data.1              <- cbind(c(1:dim(nodes.info)[1]), nodes.info)
  colnames(work.data.1)[1] <- "num.node"
  
  # add the num.path column
  work.data.2              <- cbind(c(1:dim(unique.path)[1]), unique.path)
  colnames(work.data.2)[1] <- "num.path"
  
  # add the X and Y locations for each node
  for (i in 1:dim(work.data.1)[1])
  {
    work.data.1$Y[i] <- max(work.data.1$node.level) - work.data.1$node.level[i] + 1
    work.node        <- work.data.1$node.name[i]
    work.node.level  <- work.data.1$node.level[i] + 2
    work.data        <- subset(work.data.2, work.data.2[, work.node.level] == work.node)
    work.data.1$X[i] <- mean(work.data$num.path) 
  }
  
  # add the X.end and Y.end for each node according to its parent
  
  # the root gets NAs
  work.data.1$X.end[1] <- NA
  work.data.1$Y.end[1] <- NA
  
  # the other nodes
  for (k in 2:dim(work.data.1)[1])
  {
    node.par.name        <- work.data.1$node.par.name[k]
    work.data            <- subset(work.data.1[work.data.1[, 2] == node.par.name, ])
    work.data.1$X.end[k] <- work.data$X
    work.data.1$Y.end[k] <- work.data$Y 
  }
  
  # the names for the levels on the Y axis
  levels.name <- rev(colnames(work.data.2)[2:(dim(work.data.2)[2] - 1)])
  
  ###################################
  # create the segments data frames #
  ###################################
  
  segments.data <- work.data.1[2:dim(work.data.1)[1], c("X", "Y", "X.end", "Y.end")]
  seg.new       <- segments.data[1, ]
  seg.new[1, ]  <- NA
  
  for (k2 in 1:dim(segments.data)[1])
  {
    
    local.seg       <- segments.data[k2, ]
    local.seg$Y     <- local.seg$Y     + rect.hieght / 2
    local.seg$Y.end <- local.seg$Y.end - rect.hieght / 2
    
    X               <- local.seg$X
    Y               <- local.seg$Y
    X.end           <- local.seg$X.end
    Y.end           <- local.seg$Y.end
    Y.mid           <- Y + (Y.end - Y) / 2   
    
    seg.new[(3 * k2 - 2) , c("X", "Y", "X.end", "Y.end")] <- c(X    , Y    , X    , Y.mid)    # vertical child segment
    seg.new[(3 * k2 - 1) , c("X", "Y", "X.end", "Y.end")] <- c(X    , Y.mid, X.end, Y.mid)    # horizontal segment
    seg.new[(3 * k2    ) , c("X", "Y", "X.end", "Y.end")] <- c(X.end, Y.mid, X.end, Y.end)    # vertical parent segment
  }
  
  
  ######################################################################
  # create the rectangles data frames for all nodes (except root node) #
  ######################################################################
  
  rect.data <- data.frame(X.min = c(NA),    
                          X.max = c(NA),         
                          Y.min = c(NA),         
                          Y.max = c(NA),       
                          term.int.node = c(NA))
   
                                         
  for (k3 in 2:dim(work.data.1)[1])
  {
    rect.data[k3 - 1, "X.min"]         <- work.data.1[k3, "X"] - rect.width  / 2
    rect.data[k3 - 1, "X.max"]         <- work.data.1[k3, "X"] + rect.width  / 2
    rect.data[k3 - 1, "Y.min"]         <- work.data.1[k3, "Y"] - rect.hieght / 2
    rect.data[k3 - 1, "Y.max"]         <- work.data.1[k3, "Y"] + rect.hieght / 2
    rect.data[k3 - 1, "term.int.node"] <- work.data.1[k3, "term.int.node"]
  }  
   
  fillScaleValues <- c("int.node"  = int.col,
                       "term.node" = term.col)
  
  ######################################################
  # create the rectangle data frames for the root node #
  ######################################################
  
  rect.data.root <- data.frame(X.min    = c(NA),    
                               X.max    = c(NA),         
                               Y.min    = c(NA),         
                               Y.max    = c(NA),       
                               root.col = c(NA))
  
  rect.data.root[1, "X.min"]    <- work.data.1[1, "X"] - rect.width  * 2
  rect.data.root[1, "X.max"]    <- work.data.1[1, "X"] + rect.width  * 2
  rect.data.root[1, "Y.min"]    <- work.data.1[1, "Y"] - rect.hieght / 2
  rect.data.root[1, "Y.max"]    <- work.data.1[1, "Y"] + rect.hieght / 2
  rect.data.root[1, "root.col"] <- root.col
  
  ##########################################
  # create the nodes name text data frames #
  ##########################################
  
  node.text <- data.frame(X          = c(NA),    
                          Y          = c(NA),         
                          label.node = c(NA))
  
  for (k4 in 1:dim(work.data.1)[1])
  {
    node.text[k4, "X"]  <- work.data.1[k4, "X"]
    node.text[k4, "Y"]  <- work.data.1[k4, "Y"]  
    
    if(nchar(work.data.1[k4, "node.name"]) <= split.text)
      {node.text[k4, "label.node"] <- work.data.1[k4, "node.name"]}
    
    if(nchar(work.data.1[k4, "node.name"]) > split.text)
      {
      end.chr                     <- round(nchar(work.data.1[k4, "node.name"]) / 2, 0)
      first.term                  <- substr(work.data.1[k4, "node.name"], 1, end.chr)
      second.term                 <- substr(work.data.1[k4, "node.name"], end.chr + 1, nchar(work.data.1[k4, "node.name"]))
      node.text[k4, "label.node"] <- paste(first.term, second.term, sep="\n")
      }
  
  }  
  

  ############################################
  # create the classifer.ID text data frames #
  ############################################
  
  if (label.classifiers==TRUE)
   {
    local.classifiers <- subset(work.data.1, work.data.1$clas.yes.no == "yes")
    
    local.RF.text     <- data.frame(X           = c(NA),    
                                    Y           = c(NA),         
                                    label.class = c(NA))
    
    rect.classifier   <- data.frame(X.min = c(NA),    
                                    X.max = c(NA),         
                                    Y.min = c(NA),         
                                    Y.max = c(NA))
    
    for(k5 in 1:dim(local.classifiers)[1])
    { # start k5 loop
      local.RF.text[k5, "X"]           <- local.classifiers[k5, "X"]
      local.RF.text[k5, "Y"]           <- local.classifiers[k5, "Y"] - 0.5
      local.RF.text[k5, "label.class"] <- local.classifiers[k5, "classifier.ID"]
      
      rect.classifier[k5, "X.min"]     <- local.RF.text[k5, "X"] - rect.width  / 2.5
      rect.classifier[k5, "X.max"]     <- local.RF.text[k5, "X"] + rect.width  / 2.5
      rect.classifier[k5, "Y.min"]     <- local.RF.text[k5, "Y"] - rect.hieght * class.ID.rect.hieght.scale
      rect.classifier[k5, "Y.max"]     <- local.RF.text[k5, "Y"] + rect.hieght * class.ID.rect.hieght.scale  
    } # end k5 loop 
  } # end if label.classifiers==TRUE 
  
  
  ####################
  # Start the ggplot #
  ####################
  
  
  p <- ggplot() 
  
  # add the segments
  p <- p + geom_segment(data = seg.new,
                        aes(x    = X, 
                            y    = Y, 
                            xend = X.end, 
                            yend = Y.end),
                        environment = localenv)
  
  # add the boxes for all nodes except the Root
  p <- p + geom_rect(data = rect.data,
                     aes(xmin = X.min, 
                         xmax = X.max, 
                         ymin = Y.min, 
                         ymax = Y.max,
                         fill = term.int.node),
                     color = "black",
                     environment = localenv)
  
  # add the box for the Root
  p <- p + geom_rect(data  = rect.data.root,
                     aes(xmin = X.min, 
                         xmax = X.max, 
                         ymin = Y.min, 
                         ymax = Y.max),
                     fill  = root.col,
                     color ="black",
                     environment = localenv)
  
  # add the Nodes names inside the boxes
  if(label.nodes == TRUE)
  {
    p <- p + geom_text(data  = node.text,
                       aes(x     = X, 
                           y     = Y,
                           label = label.node),
                       size  = rect.hieght * text.size,
                       angle = text.angle,
                       environment = localenv)
  } # end if statement: (label.nodes==TRUE)
  
  ### the classifiers ID ###
  if(label.classifiers == TRUE)
  {
    # add the classifer ID boxes 
    p <- p + geom_rect(data     = rect.classifier,
                       aes(xmin = X.min, 
                           xmax = X.max, 
                           ymin = Y.min, 
                           ymax = Y.max),
                       color    = class.ID.rect.col,
                       fill     = class.ID.rect.fill,
                       linetype = class.ID.rect.linetype,
                       environment = localenv)
    
    
    # add the classifer ID 
    p <- p + geom_text(data  = local.RF.text,
                       aes(x     = X, 
                           y     = Y,
                           label = label.class),
                       size  = rect.hieght * text.size * class.ID.text.size.scale,
                       color = class.ID.text.color,
                       environment = localenv)
  } # end if statement: (label.classifiers==TRUE) 
  
  # arrange the theme
  p <- p + theme(axis.text.x     = element_blank(),
                 legend.position = "none",
                 axis.title.x    = element_blank(),
                 axis.title.y    = element_text(size   = rect.hieght * text.size * 4,
                                                colour ="black"),
                 axis.text.y     = element_text(size   = rect.hieght * text.size * 4,
                                                colour = "black"))
  
  # add the y title
  p <- p + ylab(Y.title)
  
  # change the labels for the Y axis
  p <- p + scale_y_discrete (limit  = levels.name)
  
  # adjust the colors for the fill of the boxes according to internal vs. terminal nodes
  p <- p + scale_fill_manual(values = fillScaleValues)
  
  # return the ggplot object
  return(print(p)) # ggplot object
  
} # end Function