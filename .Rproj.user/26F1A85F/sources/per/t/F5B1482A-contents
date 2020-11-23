.plotsROC <- function(object, labelThres=FALSE, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, ...){

  rocd <- object[[1]]@table
  AUC <- object[[1]]@AUC

  old.par <- par(no.readonly = TRUE)
  par(oma = c(0, 0, 0, 4))
  par(mgp = c(1.5, 1, 0))

  c1 <- 255
  c2 <- 0
  c3 <- 0
  p1 <- 17

  graphics::plot(rocd$falseAlarms1, rocd$Model1, type="l", lty=1,
                 xlab=paste("False Alarms/(False Alarms + Correct Rejections)"), ylab=paste("Hits/(Hits+Misses)"), lwd=2,
                 col=rgb(c1,c2,c3, maxColorValue=255), bty="n", xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), asp=1, ...)
  axis(1, pos=0, xaxp = c(0, 1, nticks), cex.axis=0.9, ...)
  axis(2, pos=0, xaxp = c(0, 1, nticks), cex.axis=0.9, ...)
  points(rocd$falseAlarms1, rocd$Model1, pch=p1, col=rgb(255,0,0, maxColorValue=255))
  if(labelThres == TRUE) text(rocd$falseAlarms1, rocd$Model1, round(as.numeric(rocd$Threshold), digitsL), pos = posL, offset = offsetL, ...)

  if(addAUC){text(AUClableX,AUClableY+0.05*(length(object)-1),paste("AUC: ",round(as.numeric(AUC), digitsAUC)),adj = c(0,1))}

  legends <- paste(modelLeg,1)
  cols <- rgb(0,0,255, maxColorValue=255)
  pchs <- 17
  ltys= 1
  lwds <- 2

  for (j in 2:length(object)) {
    x <- object[[j]]
    rocd <- x@table
    AUC[j] <- x@AUC

    c1[j] <- runif(1, min = 0, max = 255)
    c2[j] <- runif(1, min = 0, max = 255)
    c3[j] <- runif(1, min = 0, max = 255)
    p1[j] <- runif(1, min = 0, max = 25)

    # model
    lines(rocd$falseAlarms1, rocd$Model1, lwd=2, col=rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    points(rocd$falseAlarms1, rocd$Model1, pch=p1[j], col=rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    if(labelThres == TRUE) text(rocd$falseAlarms1, rocd$Model1, round(as.numeric(rocd$Threshold), digitsL), pos = posL, offset = offsetL)

    #legend("right", paste("Model",j),col = rgb(255,0,0, maxColorValue=255), lty = 1, pch = pi, merge = TRUE, bty="n", lwd=2)

    if(addAUC){text(AUClableX,AUClableY+0.05*(length(object))-0.05*j,paste("AUC: ",round(as.numeric(AUC[j]), digitsAUC)),adj = c(0,1))}

    legends <- c(legends, paste(modelLeg,as.numeric(j)))
    cols <- c(cols, rgb(c1[j],c2[j],c3[j], maxColorValue=255))
    pchs <- c(pchs, p1[j])
    ltys <- c(ltys, 3)
    lwds <- c(lwds, 2)

  }

  legends <- c(legends, "Uniform")
  cols <- c(cols, rgb(0,0,255, maxColorValue=255))
  pchs <- c(pchs, NA)
  ltys <- c(ltys, 3)
  lwds <- c(lwds, 2)

  # maximum
  lines(c(0, 1, 1), c(1, 1, 0), lwd=1, col="black")

  # uniform
  lines(c(0, 1), c(0, 1), lty="dotted", lwd=2, col=rgb(0,0,255, maxColorValue=255))


  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")



  legend("right", legends,
         col = cols, lty = ltys , pch = pchs,
         merge = TRUE, bty="n", lwd=lwds)
  par(old.par)

}

