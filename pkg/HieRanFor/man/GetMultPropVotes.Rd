% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/GetMultPropVotes.R
\name{GetMultPropVotes}
\alias{GetMultPropVotes}
\title{For each case, the multiplicative proportion of votes}
\usage{
GetMultPropVotes(prop.vote, unique.path, all.levels = FALSE, ...)
}
\arguments{
\item{prop.vote}{Data frame, the proportion of votes that each case
  received in each local classifier. The output of \code{\link{predict.HRF}}.}

\item{unique.path}{Data frame, one of the output data frames of
  \code{RunHRF}. Contains information on each path from the \code{tree.root}
  to each of the terminal nodes.}

\item{all.levels}{Logical, if \code{TRUE}, a data frame with the
  predicted probabilities is returned for each level of the class hierarchy.
  If \code{FALSE} (default), multiplicative proportion of votes are evaluated
  only for the entire class hierarchy.}

\item{\dots}{Optional parameters to be passed to low level functions.}
}
\value{
The function returns a list with a single data frame if
   \code{all.levels}=\code{FALSE} or a list with length equalling the class
   hierarchy tree depth if \code{all.levels}=\code{TRUE}.
   \describe{Each object within the list contains a data frame with the
   following columns:

     {\tabular{lll}{
      \code{"train.or.test"} \tab \tab whether the case was from the training
      dataset or from \code{new.data}. \cr

      \code{"case.ID"}       \tab \tab The case.ID of the case. \cr

      \code{other columns}     \tab \tab a column for each terminal node for
      the given level, containing the multiplicative proportion of votes.
      Values over all nodes should sum to 1. \cr }}}
}
\description{
This function takes as input the proportion of votes that each case received
in each local classifier (See: \code{\link{predict.HRF}}) and returns the
multiplication of votes, along each path down the class hierarchy.
}
\details{
The function prints an error message for cases whose sum over all
  terminal nodes for a given hierarchy level is different from 1 (up to 10
  digits accuracy). Setting \code{all.levels = TRUE} enables exploration of
  proportion of votes, crisp classes and accuracy at specific tree depth.
}
\examples{
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
}
\author{
Yoni Gavish <gavishyoni@gmail.com>
}
\seealso{
\code{\link{RunHRF}} for running HRF,
\code{\link{predict.HRF}} for predicting the proportion of votes,
\code{\link{PerformanceHRF}} for various performance measures.
}

