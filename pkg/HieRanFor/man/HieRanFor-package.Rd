\name{HieRanFor-package}
\alias{HieRanFor-package}
\alias{HieRanFor}
\docType{package}
\title{
runs randomForest as the local classificiation algorithm for each parent node along a pre-defined hierarhcial tree like class strucutre.
}
\description{
The package apllies the \code{\link{randomForest}} algorthm as the lcoal classifier along a pre-dfined class hierarchy.
}
\details{
\tabular{ll}{
Package: \tab HieRanFor\cr
Type: \tab Package\cr
Version: \tab 1.0 \cr
Date: \tab 2015-01-27\cr
License: \tab GPL-2 \cr
}

}
\author{
Yoni Gavish
Maintainer: Yoni Gavish <gavishyoni@gmail.com>

}
\references{
Breiman, L. 2001. Random forests. \emph{Machine Learning} 45:5-32.}



\seealso{
\code{\link{randomForest}}
}
\examples{
# create random data
# random.hRF <- RandomHRF(num.term.nodes = 20, tree.depth = 4)
# hie.RF <- RunHRF(train.data = random.hRF$train.data,
#                  case.ID = "case.ID",
#                  hie.levels = c(2:(random.hRF$call$tree.depth + 1)))
}
