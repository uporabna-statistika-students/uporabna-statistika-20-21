plotCCA <- function(ccRes, xTitle = "X", yTitle = "Y", inColors = TRUE, scaleLabelsFactor = 1/2, 
                    what = "reg", nDigits = 2){
  try(dev.off(), silent = TRUE)
  corr <- ccRes$cor
  if (what == "reg") {
    Xcoef <- ccRes$xcoef[,1:length(corr)]
    Ycoef <- ccRes$ycoef[,1:length(corr)]
  }
  if (what == "cor") {
    Xcoef <- ccRes$scores$corr.X.xscores[,1:length(corr)]
    Ycoef <- ccRes$scores$corr.Y.yscores[,1:length(corr)]
    if (is.null(Xcoef)){
      warning("There are no correlations between the canonical correlations and variables in ccRes object. Set useCCApackage = FALSE in cancorPlus function. \n Regression coefficients are ploted instead.")
      Xcoef <- ccRes$xcoef[,1:length(corr)]
      Ycoef <- ccRes$ycoef[,1:length(corr)]
      } 
  }

  nVarX <- nrow(Xcoef) 
  nVarY <- nrow(Ycoef)
  nCorr <- length(corr)
  
  yL <- c(1, 1 + nVarX + nVarY + 3)
  xL <- c(0.9, max(nCorr, ncol(Ycoef)) + 1)
  
  if (is.null(rownames(ccRes$xcoef))) {namesVarX <- paste0("Var X", 1:nVarX)} else {namesVarX <- rownames(ccRes$xcoef)}
  if (is.null(rownames(ccRes$ycoef))) {namesVarY <- paste0("Var Y", 1:nVarY)} else {namesVarY <- rownames(ccRes$ycoef)}
  
  varNames <- c(namesVarX, "", "", "", namesVarY)
  
  barveX <- matrix(rgb(red = 0, green = 0, blue = 0, maxColorValue = 1), 
                   nrow = nVarX, 
                   ncol = nCorr)
  barveY <- matrix(rgb(red = 0, green = 0, blue = 0, maxColorValue = 1), 
                   nrow = nVarY, 
                   ncol = ncol(Ycoef))
  
  if (inColors == TRUE) {
    maxColor <- max(abs(c(as.vector(Xcoef), as.vector(Ycoef))))
    for (i in 1:nrow(barveX)){
      for (j in 1:ncol(barveX)){
        if (Xcoef[i,j] > 0) barveX[i,j] <- rgb(red = 0, green = 0, blue = abs(Xcoef[i,j]), maxColorValue = maxColor)
        if (Xcoef[i,j] < 0) barveX[i,j] <- rgb(red = abs(Xcoef[i,j]), green = 0, blue = 0, maxColorValue = maxColor)
      }
    }
    for (i in 1:nrow(barveY)){
      for (j in 1:ncol(barveY)){
        if (Ycoef[i,j] > 0) barveY[i,j] <- rgb(red = 0, green = 0, blue = abs(Ycoef[i,j]), maxColorValue = maxColor)
        if (Ycoef[i,j] < 0) barveY[i,j] <- rgb(red = abs(Ycoef[i,j]), green = 0, blue = 0, maxColorValue = maxColor)
      }
    }
  }
  
  par(mar = c(1, 2, 1, 1))
  plot("1", ylim = yL, xlim = xL, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  text(x = 2:xL[2], y = yL[2], labels = paste0("CC", 1:(xL[2]-1)), font = 2)
  text(x = 1, y = 1:length(varNames), labels = varNames, pos = 4, font = 2)
  text(x = rep(2:(ncol(Ycoef)+1), each = nVarY), 
       y = rep((nVarX+1):((nVarX+nVarY))+3, 3), 
       labels = format(round(Ycoef, nDigits), digits=2, nsmall=2), cex = abs(Ycoef)**scaleLabelsFactor+0.2, 
       col = barveY, xpd = TRUE)
  text(x = rep(2:(nCorr+1), each = nVarX), 
       y = rep(1:nVarX, nCorr), 
       labels = format(round(Xcoef, nDigits), digits=2, nsmall=2), cex = abs(Xcoef)**scaleLabelsFactor+0.2, 
       col = barveX, xpd = TRUE)
  
  arrows(x0 = rep(2:xL[2]), x1 = rep(2:xL[2]), y1 = nVarX+3, y0 = nVarX+1, length = 0.1)
  arrows(x0 = rep(2:xL[2]), x1 = rep(2:xL[2]), y1 = nVarX+1, y0 = nVarX+3, length = 0.1)
  text(x = 2:(ncol(Ycoef)+1), y = nVarX+2, 
       labels = format(round(corr, nDigits), digits=2, nsmall=2), 
       font = 2, 
       col = ifelse(sign(corr), yes = "blue", no = "red"))
  
  text(x = 0.7, y = mean(1:nVarX), label = xTitle, xpd = TRUE, srt = 90)
  text(x = 0.7, y = mean(1:nVarY) + nVarX+3, label = yTitle, xpd = TRUE, srt = 90)
}
