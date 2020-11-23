.TOCnosp <- function(index, boolean, mask=NULL, nthres=NULL, thres=NULL, NAval=0, P=NA, Q=NA, progress=TRUE,
                     ones.bool=NULL, zeros.bool=NULL, population=NULL, units=character(0), sort='DECREASE', thresorder=TRUE, ordinal=FALSE, FirstThres=NULL, LastThres=NULL, Increment=NULL) {

  if(!is.null(nthres) & !is.null(thres)) stop("Enter nthres OR thres as input to define thresholds, not both at the same time. Or just sort the thresholds decreasing")
  if(!(is.null(nthres) | is.null(thres)) & !is.null(thresorder)) stop("Enter nthres OR thres as input to define thresholds or just sort them decrease, not both at the same time")

  boolval <- boolean
  # calculate population if not given
  if (is.null(population)){
    if(!is.null(mask)){
      mask[mask == NAval] <- NA
      boolval <- boolean*mask
    }
    # extract total number of no NA cells
    boolvals <- boolval[!is.na(boolval)]
    population <- length(boolvals)
    if(!is.na(P) & !is.na(Q)){
      population <- P + Q
    }
  }

  if(ordinal){}

  # calculate basic roc (toc) table
  tocd0 <- roctable(index, boolean, maskval=mask, nthres=nthres, thres=thres, NAval=NAval, progress=progress,
                    ones.bool=ones.bool, zeros.bool=zeros.bool, sort=sort, thresorder=thresorder, FirstThres=FirstThres, LastThres=LastThres, Increment=Increment)
  tocd <- tocd0$tocd
  minInd <- tocd0$minInd

  # calculate additional columns for toc
  maxA <- tocd$A[nrow(tocd)]
  maxB <- tocd$B[nrow(tocd)]
  tocd$m <- (maxA - tocd$A)/(maxA + maxB)
  tocd$h <- tocd$A/(maxA + maxB)
  tocd$f <- tocd$B/(maxA + maxB)
  tocd$c <- 1 - tocd$m - tocd$h -tocd$f
  prevalence <- tocd$h[nrow(tocd)]

  tocd$Hits <- tocd$Model1 * prevalence * population
  tocd$hitsFalseAlarms <- tocd$Hits + tocd$falseAlarms1*(1-prevalence)*population
  tocd$hitsMisses <- prevalence*population
  tocd$maximum <- pmin(tocd$hitsMisses, tocd$hitsFalseAlarms)
  tocd$minimum <- pmax(0, tocd$hitsFalseAlarms + tocd$hitsMisses - population)
  tocd$Uniform1 <- tocd$hitsMisses * tocd$hitsFalseAlarms / population

  tocd1 <- tocd
  #tocd1[nrow(tocd1), "Threshold"] <- paste("<= ", minInd)

  tocd2 <- tocd1[, c("Threshold", "hitsFalseAlarms", "Hits")]

  # adjustment to population if user provides P and Q
  if(!is.na(P) & !is.na(Q)){
    tocd1$hitsFalseAlarmsP <- P * tocd1$Model1 + Q * tocd1$falseAlarms1
    tocd1$HitsP <- P * tocd1$Model1
    tocd2 <- tocd1[, c("Threshold", "hitsFalseAlarms", "Hits", "hitsFalseAlarmsP", "HitsP")]
  }



  # calculate totalAUC in data units and AUC as a proportion
  id <- order(tocd2$hitsFalseAlarms)
  totalAUC <- sum(tocd2$Hits[-length(tocd2$Hits)] * diff(tocd2$hitsFalseAlarms)) +
    sum(diff(tocd2$hitsFalseAlarms[id])*diff(tocd2$Hits[id]))/2 - ((prevalence * population)^2)/2

  AUC <- totalAUC/(population * prevalence * population - (prevalence * population)^2)

  colnames(tocd2)[2] <- "Hits+FalseAlarms"
  if (any(colnames(tocd2) == "hitsFalseAlarmsP")) colnames(tocd2)[4] <- "Hits+FalseAlarmsP"

  # calculate uncertainty
  if(!is.null(mask)) index <- index*mask
  uncertain <- uncertainty(index, tocd, sort=sort)

  # calculate Correct Corner
  tocd3 <- tocd1[, c("Threshold", "Hits", "hitsFalseAlarms")]
  tocd3$Diff <- tocd2$Hits[length(tocd2$Hits)] - tocd3$hitsFalseAlarms
  tocd3[nrow(tocd3)+1,] <- c(0,0,tocd2$Hits[length(tocd2$Hits)],0)

  tocd4 <- tocd3[sort.list(tocd3$Diff),]

  k <- (tocd4$Hits[which(tocd4$Diff==0)+1]-tocd4$Hits[which(tocd4$Diff==0)-1])/(tocd4$hitsFalseAlarms[which(tocd4$Diff==0)+1]-tocd4$hitsFalseAlarms[which(tocd4$Diff==0)-1])
  b <- tocd4$Hits[which(tocd4$Diff==0)+1] - k*tocd4$hitsFalseAlarms[which(tocd4$Diff==0)+1]

  tocd4$Hits[which(tocd4$Diff==0)] <- k*tocd2$Hits[length(tocd2$Hits)]+b
  tocd4$Threshold[which(tocd4$Diff==0)] <- ((tocd4$Threshold[which(tocd4$Diff==0)+1]-tocd4$Threshold[which(tocd4$Diff==0)-1])/(tocd4$hitsFalseAlarms[which(tocd4$Diff==0)+1]-tocd4$hitsFalseAlarms[which(tocd4$Diff==0)-1]))*(tocd4$hitsFalseAlarms[which(tocd4$Diff==0)]-tocd4$hitsFalseAlarms[which(tocd4$Diff==0)+1])+tocd4$Threshold[which(tocd4$Diff==0)+1]

  # output
  output <- new("Toc", table=tocd2, prevalence=prevalence*population, population=population, units=units, CorrectCorner = tocd4[(which(tocd4$Diff==0)),1:3],
                AUC=AUC, maxAUC = AUC + uncertain/2, minAUC = AUC - uncertain/2)
  return(output)

}
