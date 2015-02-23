#' @title Modification of the \code{oliveoil} dataset from package \code{pdfCluster}.
#' @name  OliveOilHie
#'
#' @description The original dataset (\code{\link{oliveoil}}) contains information on eight
#' chemical measurements on different specimen of olive oil produced in various
#' regions in Italy (northern Apulia, southern Apulia, Calabria, Sicily, inland
#' Sardinia and coast Sardinia, eastern and western Liguria, Umbria) and further
#' classifiable into three macro-areas: Centre-North, South, Sardinia". 
#' Here the hierarchy is slightly modified to include three hierarchical levels 
#' by adding a level 2 class for the Liguria and Apulia regions. The level 3 
#' classes for these two regions are further seperated to east/west and 
#' north/south, for Liguria and Apulia, respectively. Other terminal nodes are in
#' level 2, resulting with a class hierarchy with terminal nodes at differnet
#' levels. The default of \code{END.PATH} is used for \code{end.path.name}.
#' 
#' @format {A data frame with 572 rows and 12 variables:
#' \tabular{lll}{
#'   \code{case.ID}     \tab \tab {A unique case.ID value for each case}    \cr
#'   \code{L1, L2, L3}  \tab \tab {the three levels of the class hierarchy} \cr
#'   \code{...}tab      \tab \tab {The eight chemical measurments}          \cr
#' }}
#' @source see details in \code{\link{oliveoil}} in the package \code{\link{pdfCluster}}
#' @docType data
#' @usage data(OliveOilHie)
#' 
#' 
NULL


