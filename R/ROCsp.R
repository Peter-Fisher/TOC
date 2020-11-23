.ROCsp <- function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, progress=TRUE, sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL) {

  if(!is.null(nthres) & !is.null(thres)) stop("Enter nthres OR thres as input to define thresholds, not both at the same time. Or just sort the thresholds decreasing")
  if(!(is.null(nthres) | is.null(thres)) & !is.null(thresorder)) stop("Enter nthres OR thres as input to define thresholds or just sort them decrease, not both at the same time")

  # extract cell values from the boolean and index maps
  boolval <- getValues(boolean)
  indval <- getValues(index)

  # extract cell values from the mask map if given
  if(!is.null(mask)) mask <- getValues(mask)

  # calculate basic roc (toc) table
  tocd <- .ROCnosp(indval, boolval, mask=mask, nthres=nthres, thres=thres, NAval=NAval, progress=progress,
                   ones.bool=NULL, zeros.bool=NULL, sort=sort, thresorder=thresorder, ordinal=ordinal, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)

  return(tocd)

}
