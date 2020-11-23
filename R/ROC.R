# Author: Ali Santacruz Yunzhe Zhu
# Date :  December 2019
# Version 1.1
# Licence GPL v3


if (!isGeneric("ROC")) {
	setGeneric("ROC", function(index, boolean, ...)
		standardGeneric("ROC"))
}

setMethod("ROC", signature=c('numeric', 'numeric'),
          definition = function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, progress=TRUE, sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL)  {
            tocd <- .ROCnosp(index, boolean, mask=mask, nthres=nthres, thres=thres, NAval=NAval, progress=progress, sort=sort, thresorder=thresorder, ordinal=ordinal, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
            return(tocd)
          }
          )

setMethod("ROC", signature=c('RasterLayer', 'RasterLayer'),
          definition = function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, progress=TRUE, sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL)  {
            tocd <- .ROCsp(index, boolean, mask=mask, nthres=nthres, thres=thres, NAval=NAval, progress=progress, sort=sort, thresorder=thresorder, ordinal=ordinal, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
            return(tocd)
          }
          )
