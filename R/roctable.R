roctable <- function(indval, boolval, maskval=NULL, nthres=NULL, thres=NULL, NAval=0, progress=FALSE,
                     ones.bool=NULL, zeros.bool=NULL, sort='DECREASE', thresorder=TRUE, FirstThres=NULL, LastThres=NULL, Increment=NULL){

  # mask out nodata cells in the index and boolean vectors if a mask vector is given
  if(!is.null(maskval)){
    maskval[maskval == NAval] <- NA
    indval <- indval*maskval
    boolval <- boolval*maskval
  }
  if(is.null(ones.bool) | is.null(zeros.bool)){
    # extract total number of cells with ones and zeros in the boolean vector
    boolvals <- boolval[!is.na(boolval)]
    ones.bool <- sum(as.bit(boolvals))
    zeros.bool <- length(boolvals) - ones.bool
  }

  # generic function for crosstabing two boolean vectors
  func_logical3   <- function(v1,v2){
    r1  <- sum(v1 & v2)
    r2  <- sum(v1 & !v2)
    return(c(r1, r2))
  }

  #generic function for calculating the baisc table
  func_calculateTable1 <- function(boolval,indval,ones.bool,zeros.bool,newThres){
    zeroIndVal <- indval*0

    # create results data.frame
    res <- cbind(newThres, "Hits"=0, "HitsRate"=0, "falseAlarms"=0, "falseAlarmsRate"=0)

    # loop for reclassifying the index vector for each new threshold
    # and then perform the crosstab with the boolean vector

    for (j in 2:(nrow(res))){
      i <- newThres[j]
      zeroIndVal[which(indval == i)]  <- 1

      xb <- as.bit(zeroIndVal)
      yb <- as.bit(boolval)
      crsstb <- func_logical3(xb,yb)

      res[j,"Hits"] <- crsstb[1]+res[j-1,"Hits"]
      res[j,"HitsRate"] <- res[j,"Hits"]/ones.bool*100
      res[j,"falseAlarms"] <- crsstb[2]+res[j-1,"falseAlarms"]
      res[j,"falseAlarmsRate"] <- res[j,"falseAlarms"]/zeros.bool*100
      zeroIndVal <- indval*0

      if(progress){
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
        setTxtProgressBar(pb, round(j/nrow(res)*100, 0))
      }
    }
    res[nrow(res),]<-c(newThres[length(newThres)], ones.bool, 100, zeros.bool, 100)
    tocd <- as.data.frame(res)

    return(tocd)
  }

  #generic function for calculating the baisc table
  func_calculateTable2 <- function(boolval,indval,ones.bool,zeros.bool,newThres){
    zeroIndVal <- indval*0

    # create results data.frame
    res <- cbind(newThres, "Hits"=0, "HitsRate"=0, "falseAlarms"=0, "falseAlarmsRate"=0)

    # loop for reclassifying the index vector for each new threshold
    # and then perform the crosstab with the boolean vector

    for (j in 2:(nrow(res))){
      i <- newThres[j]
      zeroIndVal[which(indval >= i)]  <- 1

      xb <- as.bit(zeroIndVal)
      yb <- as.bit(boolval)
      crsstb <- func_logical3(xb,yb)

      res[j,"Hits"] <- crsstb[1]+res[j-1,"Hits"]
      res[j,"HitsRate"] <- crsstb[1]/ones.bool*100
      res[j,"falseAlarms"] <- crsstb[2]+res[j-1,"falseAlarms"]
      res[j,"falseAlarmsRate"] <- crsstb[2]/zeros.bool*100
      zeroIndVal <- indval*0

      if(progress){
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
        setTxtProgressBar(pb, round(j/nrow(res)*100, 0))
      }
    }
    res[nrow(res),]<-c(newThres[length(newThres)], ones.bool, 100, zeros.bool, 100)
    tocd <- as.data.frame(res)

    return(tocd)
  }

  #generic function for calculating the baisc table
  func_calculateTable3 <- function(boolval,indval,ones.bool,zeros.bool,newThres){
    zeroIndVal <- indval*0

    # create results data.frame
    res <- cbind(newThres, "Hits"=0, "HitsRate"=0, "falseAlarms"=0, "falseAlarmsRate"=0)

    # loop for reclassifying the index vector for each new threshold
    # and then perform the crosstab with the boolean vector

    for (j in 2:(nrow(res))){
      i <- newThres[j]
      zeroIndVal[which(indval <= i)]  <- 1

      xb <- as.bit(zeroIndVal)
      yb <- as.bit(boolval)
      crsstb <- func_logical3(xb,yb)

      res[j,"Hits"] <- crsstb[1]+res[j-1,"Hits"]
      res[j,"HitsRate"] <- crsstb[1]/ones.bool*100
      res[j,"falseAlarms"] <- crsstb[2]+res[j-1,"falseAlarms"]
      res[j,"falseAlarmsRate"] <- crsstb[2]/zeros.bool*100
      zeroIndVal <- indval*0

      if(progress){
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
        setTxtProgressBar(pb, round(j/nrow(res)*100, 0))
      }
    }
    res[nrow(res),]<-c(newThres[length(newThres)], ones.bool, 100, zeros.bool, 100)
    tocd <- as.data.frame(res)

    return(tocd)
  }

  # extract only no NA values from the boolean and index vectors
  # length of (not NA) boolean and index vectors must be equal as the mask was applied previously
  if(any(ls()=='boolvals')){
    boolval <- boolvals
  } else{
    boolval <- boolval[!is.na(boolval)]
  }

  indval <- indval[!is.na(indval)]

  if (length(boolval)!=length(indval)) {stop("different NA values in input maps")}

  maxInd <- max(indval, na.rm=TRUE)
  uniqInd <- unique(indval)

  ifelse(!is.null(thres),
         {newThres <- thres; minInd <- min(thres);},
         minInd <- min(indval, na.rm=TRUE))

  ifelse(!is.null(nthres),
         newThres <- (maxInd - minInd)/(nthres-2)*(0:(nthres-2)) + minInd,
         ifelse(!is.null(thres),
                {newThres <- thres;},
                newThres <- uniqInd))

  # define the thresholds vector
  if(is.null(Increment) & (!is.null(FirstThres) | !is.null(LastThres))){

    if(!is.null(FirstThres)){
      if(FirstThres>min(uniqInd, na.rm=TRUE) & FirstThres < max(uniqInd, na.rm=TRUE)){
        newThres <- newThres[newThres>=FirstThres]
        newThres <- c(0,newThres)
      }
      }

    if(!is.null(LastThres)){
      if(LastThres<max(uniqInd, na.rm=TRUE) & LastThres > min(uniqInd, na.rm=TRUE)){
        newThres <- newThres[newThres<=LastThres]
        newThres <- c(newThres,maxInd)
      }
      }

  }else if(!is.null(Increment)){

    if(!is.null(FirstThres)){
      if(FirstThres>min(uniqInd, na.rm=TRUE) & FirstThres < max(uniqInd, na.rm=TRUE)){
        minInd1=FirstThres
      }else{
        minInd1=min(indval, na.rm=TRUE)
      }
    }else{
      minInd1=min(newThres, na.rm=TRUE)
    }

    if(!is.null(LastThres)){
      if(LastThres<max(uniqInd, na.rm=TRUE) & LastThres > min(uniqInd, na.rm=TRUE)){
        maxInd1=LastThres
      }else{
        maxInd1=max(indval, na.rm=TRUE)
      }
    }else{
      maxInd1=max(newThres, na.rm=TRUE)
    }

    #a vector of even numbers
    newThres <- seq(minInd1, maxInd1, by=Increment) # Explicitly specifying "by" only to increase readability
    newThres <- c(0,newThres,maxInd)

  }


  if(any(is.element(newThres, uniqInd)==FALSE) & sort=='DECREASE'){

    newThres <- sort(newThres, decreasing=TRUE)
    newThres <- c(NA,newThres)
    tocd <- func_calculateTable2 (boolval,indval,ones.bool,zeros.bool,newThres)

  }else if(any(is.element(newThres, uniqInd)==FALSE) & sort=='INCREASE'){

    newThres <- sort(newThres, decreasing=FALSE)
    newThres <- c(NA,newThres)
    tocd <- func_calculateTable3 (boolval,indval,ones.bool,zeros.bool,newThres)

  }else if(!is.null(thres) & is.null(nthres) & length(newThres)==length(uniqInd)){

    newThres <- c(NA,newThres)
    tocd <- func_calculateTable1 (boolval,indval,ones.bool,zeros.bool,newThres)

  }else if(sort=='DECREASE'){

    newThres <- sort(newThres, decreasing=TRUE)
    newThres <- c(NA,newThres)
    tocd <- func_calculateTable2 (boolval,indval,ones.bool,zeros.bool,newThres)

  }else if(sort=='INCREASE'){

    newThres <- sort(newThres, decreasing=FALSE)
    newThres <- c(NA,newThres)
    tocd <- func_calculateTable3 (boolval,indval,ones.bool,zeros.bool,newThres)

  }

  if(!thresorder){

    tocd$slops <- (tocd$Hits-append(tocd$Hits[1:(length(tocd$Hits)-1)], 0, after = 0)) / (tocd$Hits+tocd$falseAlarms - append(tocd$falseAlarms[1:(length(tocd$falseAlarms)-1)], 0, after = 0)-append(tocd$Hits[1:(length(tocd$Hits)-1)], 0, after = 0))
    sort(tocd$slops, decreasing=TRUE)
    tocdd <- tocd[order(-tocd$slops),]
    newThres <- tocdd$newThres[1:length(tocdd$newThres)-1]
    newThres <- c(NA,newThres)
    tocd <- func_calculateTable1 (boolval,indval,ones.bool,zeros.bool,newThres)

  }

  names(tocd) <- c("Threshold", "A", "HitsRate", "B", "falseAlarmsRate")

  # calculate ROC table as shown in TOCfigure1.xlsx created by Pontius
  tocd$Model1 <- tocd$HitsRate/100
  tocd$falseAlarms1 <- tocd$falseAlarmsRate/100
  tocd$Uniform <- tocd$falseAlarms1

  return(list(tocd=tocd, minInd=minInd))
}
