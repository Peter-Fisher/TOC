.plots <- function(object, labelThres=FALSE, modelLeg="Model", digits=3, nticks=5, digitsL=1, posL = NULL, offsetL = 0.5, addAUC = TRUE, digitsAUC=2, AUClableX = 0.6, AUClableY = 0.1, addCC = FALSE, ...){

  b <- numeric(length(object))
  for (i in 1:length(object)) {
    test <- object[[i]]
    if(class(test)=="Toc"){
      b[i]=1
    }else if(class(test)=="Roc"){
      b[i]=0
    }else{
      b[i]=2
    }
  }

  if(any(b==0)&!any(b==1)){#ROC
    .plotsROC(object, labelThres=labelThres, modelLeg=modelLeg, digits=digits, nticks=nticks, digitsL=digitsL, posL = posL, offsetL = offsetL, addAUC = addAUC, digitsAUC=digitsAUC, AUClableX = AUClableX, AUClableY = AUClableY, ...)
  }else if(!any(b==0)&any(b==1)){#TOC
    .plotsTOC(object, labelThres=labelThres, modelLeg=modelLeg, digits=digits, nticks=nticks, digitsL=digitsL, posL = posL, offsetL = offsetL, addAUC = addAUC, digitsAUC=digitsAUC, AUClableX = AUClableX, AUClableY = AUClableY, addCC = addCC, ...)
  }else if(any(b==0)&any(b==1)){#ROC and TOC mix
    stop("You can not plot the ROC curve and the TOC curve together. Please try multiplot with the same type of data frame")
  }else if(any(b==2)){#something else
    stop("Come on! Please put a bunch of TOC or ROC inside me and try again!")
  }

}
