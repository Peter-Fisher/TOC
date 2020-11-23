.ROCnosp <- function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, progress=TRUE,
                     ones.bool=NULL, zeros.bool=NULL, sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL) {

  if(!is.null(nthres) & !is.null(thres)) stop("Enter nthres OR thres as input to define thresholds, not both at the same time. Or just sort the thresholds decreasing")
  if(!(is.null(nthres) | is.null(thres)) & !is.null(thresorder)) stop("Enter nthres OR thres as input to define thresholds or just sort them decrease, not both at the same time")

  # calculate basic roc table
  tocd0 <- roctable(index, boolean, maskval=mask, nthres=nthres, thres=thres, NAval=NAval, progress=progress,
                    ones.bool=ones.bool, zeros.bool=zeros.bool, sort=sort, thresorder=thresorder, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
  tocd <- tocd0$tocd
  minInd <- tocd0$minInd

  tocd1 <- tocd
  #tocd1[nrow(tocd1), "Threshold"] <- paste("<= ", minInd)

  id <- order(tocd$falseAlarms1)
  AUC <- sum(tocd$Model1[-length(tocd$Model1)] * diff(tocd$falseAlarms1)) + sum(diff(tocd$falseAlarms1[id]) * diff(tocd$Model1[id]))/2

  # calculate uncertainty
  if(!is.null(mask)) {
    mask[mask == NAval] <- NA
    index <- index*mask
  }
  uncertain <- uncertainty(index, tocd, sort=sort)

  # output
  output <- new("Roc", table=tocd1[, c(1,6,7)], AUC=AUC, maxAUC = AUC + uncertain/2, minAUC = AUC - uncertain/2)

  return(output)

}
