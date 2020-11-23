# Author: Ali Santacruz Yunzhe Zhu
# Date :  December 2019
# Version 1.2
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x, ...)
		standardGeneric("plot"))
}

setMethod("plot", signature='Roc',
          definition = function(x, labelThres=FALSE, nlabelThres=NULL, labelID=NULL, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, ...)  {
            .plotROC(x, labelThres=labelThres, nlabelThres=nlabelThres, labelID=labelID, modelLeg=modelLeg, digits=digits, nticks=nticks, digitsL=digitsL, posL = posL, offsetL = offsetL, addAUC = addAUC, digitsAUC=digitsAUC, AUClableX = AUClableX, AUClableY = AUClableY,  ...)
            return(invisible(NULL))
          }
)

setMethod("plot", signature='list',
          definition = function(x, labelThres=FALSE, nlabelThres=NULL, labelID=NULL, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, addCC = FALSE, ...)  {
            .plots(x, labelThres=labelThres, nlabelThres=nlabelThres, labelID=labelID, modelLeg=modelLeg, digits=digits, nticks=nticks, digitsL=digitsL, posL = posL, offsetL = offsetL, addAUC = addAUC, digitsAUC=digitsAUC, AUClableX = AUClableX, AUClableY = AUClableY, addCC = addCC, ...)
            return(invisible(NULL))
          }
)

setMethod("plot", signature ='Toc',
          definition = function(x, labelThres=FALSE, nlabelThres=NULL, labelID=NULL, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, addCC = FALSE, ...)  {
            .plotTOC(x, labelThres=labelThres, nlabelThres=nlabelThres, labelID=labelID, modelLeg=modelLeg, digits=digits, nticks=nticks, digitsL=digitsL, posL = posL, offsetL = offsetL, addAUC = addAUC, digitsAUC=digitsAUC, AUClableX = AUClableX, AUClableY = AUClableY, addCC = addCC, ...)
            return(invisible(NULL))
          }
)
