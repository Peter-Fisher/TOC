.plotsTOC <- function(object, labelThres=FALSE, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, addCC = FALSE, ...){

  old.opt <- options()
  options(digits=digits)
  old.par <- par(no.readonly = TRUE)
  par(oma = c(0, 0, 0, 4))
  par(mgp = c(1.5, 1, 0))

  population <- object[[1]]@population
  prevalence <- object[[1]]@prevalence/population
  units <- object[[1]]@units
  AUC <- object[[1]]@AUC
  CorrectCorner <- object[[1]]@CorrectCorner

  tocd <- object[[1]]@table
  if((!is.null(tocd$HitsP) & !is.null(tocd$"Hits+FalseAlarmsP"))==TRUE){
    tocd$Hits <- tocd$HitsP
    tocd$"Hits+FalseAlarms" <- tocd$"Hits+FalseAlarmsP"
  }

  graphics::plot(c(0, population*(1-prevalence), population), c(0, 0, prevalence * population), type="l", lty="dashed",
                 xlab=paste0("Hits+False Alarms (", units, ")"), ylab=paste0("Hits (", units, ")"),
                 lwd=2, col=rgb(128,100,162, maxColorValue=255), bty="n", xaxt="n", yaxt="n", xlim=c(0, population),
                 ylim=c(0, prevalence * population), asp=1/prevalence, ...)

  xlabels <- c(0, format((1:nticks)*population/nticks, digits))
  ylabels <- c(0, format((1:nticks)*prevalence * population/nticks, digits))

  axis(1, pos = 0, labels=xlabels, at=xlabels, xaxp = c(0, population, nticks), cex.axis=0.9, ...)
  axis(2, pos = 0, labels=ylabels, at=ylabels, yaxp = c(0, prevalence * population, nticks), cex.axis=0.9, ...)


  # maximum
  lines(c(0, prevalence * population, population), c(0, prevalence * population, prevalence * population),
        lty="dotdash", lwd=2, col=rgb(79,129,189, maxColorValue=255))

  # hits+misses
  lines(c(0, population), rep(prevalence*population, 2), lwd=3, col=rgb(146,208,80, maxColorValue=255))

  # uniform
  lines(c(0, population), c(0, prevalence*population), lty="dotted", lwd=2, col=rgb(0,0,255, maxColorValue=255))

  #lines(tocd$"Hits+FalseAlarms", tocd$maximum, lty="dotdash", lwd=2, col=rgb(79,129,189, maxColorValue=255))

  c1 <- 255
  c2 <- 0
  c3 <- 0
  p1 <- 17

  # model
  lines(tocd$"Hits+FalseAlarms", tocd$Hits, lwd=2, col=rgb(c1,c2,c3, maxColorValue=255))
  points(tocd$"Hits+FalseAlarms", tocd$Hits, pch=p1, col=rgb(c1,c2,c3, maxColorValue=255))
  if(labelThres == TRUE) text(tocd$"Hits+FalseAlarms", tocd$Hits, round(as.numeric(tocd$Threshold), digitsL), pos = posL, offset = offsetL, ...)

  if(addCC){
    lines(c(CorrectCorner$hitsFalseAlarms,CorrectCorner$hitsFalseAlarms), c(0,prevalence * population), lty="dashed", lwd=1, col=rgb(0,0,255, maxColorValue=255))
    lines(c(CorrectCorner$Hits,CorrectCorner$Hits+(1-prevalence)*population), c(CorrectCorner$Hits,CorrectCorner$Hits), lty="dashed", lwd=1, col=rgb(0,0,255, maxColorValue=255))
    points(CorrectCorner$hitsFalseAlarms, CorrectCorner$Hits, pch=3,cex = 3, col=rgb(255,0,0, maxColorValue=255))
  }

  if(addAUC){text(AUClableX*population, (AUClableY+0.05*(length(object)-1))*prevalence * population,paste("AUC: ",round(as.numeric(AUC), digitsAUC)),adj = c(0,1))}

  legends <- c("Hits+Misses", "Maximum", paste(modelLeg,1))
  cols <- c(rgb(146,208,80, maxColorValue=255), rgb(79,129,189, maxColorValue=255), rgb(255,0,0, maxColorValue=255))
  pchs <- c(NA, NA, 17)
  ltys= c(1, 4, 1)
  lwds <- c(3, 2, 2)

  for (j in 2:length(object)) {
    x <- object[[j]]
    tocd <- x@table
    CorrectCorner <- x@CorrectCorner
    population <- x@population
    prevalence <- x@prevalence/population
    AUC[j] <- x@AUC

    c1[j] <- runif(1, min = 0, max = 255)
    c2[j] <- runif(1, min = 0, max = 255)
    c3[j] <- runif(1, min = 0, max = 255)
    p1[j] <- runif(1, min = 0, max = 25)

    # model
    lines(tocd$"Hits+FalseAlarms", tocd$Hits, lwd=2, col=rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    points(tocd$"Hits+FalseAlarms", tocd$Hits, pch=p1[j], col=rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    if(labelThres == TRUE) text(tocd$"Hits+FalseAlarms", tocd$Hits, round(as.numeric(tocd$Threshold), digitsL), pos = posL, offset = offsetL)

    if(addCC){
      lines(c(CorrectCorner$hitsFalseAlarms,CorrectCorner$hitsFalseAlarms), c(0,prevalence * population), lty="dashed", lwd=1, col=rgb(0,0,255, maxColorValue=255))
      lines(c(CorrectCorner$Hits,CorrectCorner$Hits+(1-prevalence)*population), c(CorrectCorner$Hits,CorrectCorner$Hits), lty="dashed", lwd=1, col=rgb(0,0,255, maxColorValue=255))
      points(CorrectCorner$hitsFalseAlarms, CorrectCorner$Hits, pch=3,cex = 3, col=rgb(255,0,0, maxColorValue=255))
    }
    #legend("right", paste("Model",j),col = rgb(255,0,0, maxColorValue=255), lty = 1, pch = pi, merge = TRUE, bty="n", lwd=2)

    if(addAUC){text(AUClableX*population, (AUClableY+0.05*(length(object))-0.05*j)*prevalence * population,paste("AUC: ",round(as.numeric(AUC[j]), digitsAUC)),adj = c(0,1))}

    legends <- c(legends, paste(modelLeg,as.numeric(j)))
    cols <- c(cols, rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    pchs <- c(pchs, p1[j])
    ltys <- c(ltys, 3)
    lwds <- c(lwds, 2)
  }

  legends <- c(legends, "Uniform", "Minimum")
  cols <- c(cols,  rgb(0,0,255, maxColorValue=255), rgb(128,100,162, maxColorValue=255))
  pchs <- c(pchs, NA, NA)
  ltys <- c(ltys, 3, 2)
  lwds <- c(lwds, 2,2)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  legend("right", legends,
         col = cols, lty = ltys , pch = pchs,
         merge = TRUE, bty="n", lwd=lwds)

  par(old.par)
  options(old.opt)


}
