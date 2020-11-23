# Author: Ali Santacruz Yunzhe Zhu
# Date :  December 2019
# Version 1.2
# Licence GPL v3


if (!isGeneric("TOC")) {
	setGeneric("TOC", function(index, boolean, ...)
		standardGeneric("TOC"))
}

setMethod("TOC", signature=c('numeric', 'numeric'),
          definition = function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, P=NA, Q=NA, progress=TRUE, units=character(0), sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL){
            tocd <- .TOCnosp(index, boolean, mask=mask, nthres=nthres, thres=thres, NAval=NAval, P=P, Q=Q, progress=progress, units=units, sort=sort, thresorder=thresorder, ordinal=ordinal, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
            return(tocd)
          }
          )

setMethod("TOC", signature=c('RasterLayer', 'RasterLayer'),
          definition = function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, P=NA, Q=NA, progress=TRUE, sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL)  {
            tocd <- .TOCsp(index, boolean, mask=mask, nthres=nthres, thres=thres, NAval=NAval, P=P, Q=Q, progress=progress, sort=sort, thresorder=thresorder , ordinal=ordinal, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
            return(tocd)
          }
          )
