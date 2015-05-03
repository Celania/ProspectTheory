if(!exists("simulateMeasuringGain", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSetGain", mode="function")) source("TradeOffMethod.R")

analyseGains <- function(p, x0, g, G, n, r){
  #print("STARTING GAINS")
  experimentalSet <- simulateMeasuringGain(g, G, p, x0, n)
  #print(experimentalSet)
  
  experimentalUtilitySet <- (experimentalSet^alpha)
  #print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetGain(experimentalSet, floor(length(experimentalSet)/2),r)
  #print(estimatedSet)
  
  #plot(experimentalSet, experimentalUtilitySet, type="l")
  #plot(experimentalSet, estimatedSet, type="l")

  return (list(experimentalSet=experimentalSet, estimatedSet=estimatedSet))
  
}

analyseLosses <- function(p, y0, l, L, n, r) {
  experimentalSet <- simulateMeasuringLoss(l, L, p, y0, n)
  #print("STARTING LOSSES")
  #print(experimentalSet)
  
  experimentalUtilitySet <- (-lambda*(-experimentalSet)^Gbeta)
  #print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetLoss(experimentalSet, floor(length(experimentalSet)/2),r)
  #print(estimatedSet)
  
  #plot(experimentalSet, experimentalUtilitySet, type="l")
  #plot(experimentalSet, estimatedSet, type="l")
  
  return (list(experimentalSet=experimentalSet, estimatedSet=estimatedSet))    
}

analyseLinking <- function(Gains, Losses, p, n, r) {
  
  #print("START LINKING")
  
  deltaULoss <- 0
  for (i in seq(1,n)){
  d <- (prospectTheoreticalValueMixed(Losses[[1]][i+1], Losses[[1]][i], p, Gains[[1]][i])) 
  k <- findFirstElementMixed(Gains[[1]], d)
  sz <- calculatesz(r, k, d, Gains[[1]])
  deltaULoss <- deltaULoss + calculateStepSizeLinking (sz, Gains[[2]][i+1] - Gains[[2]][i])
  }
  
  deltaULoss <- deltaULoss/n
  
  for (i in seq(2, length(Losses[[2]]))){
    Losses[[2]][i] <- (Losses[[2]][i-1] - deltaULoss)
  }

  estimatedx <- c(rev(Losses[[1]]),Gains[[1]])
  estimatedy <- c(rev(Losses[[2]]), Gains[[2]])
  
  rEstimated <- calculateRelation(estimatedy)
  rReal <- calculateRelation(c(sapply(rev(Losses[[1]]), function(x) (-lambda*(-x)^Gbeta)), sapply(Gains[[1]],function(x) x^alpha)))
  #print (sum(abs(rEstimated - rReal)))
  
  plot(c(rev(Losses[[1]]),Gains[[1]]), c(rev(Losses[[2]]), Gains[[2]]), type="l")
  plot(c(rev(Losses[[1]]),Gains[[1]]),c(sapply(rev(Losses[[1]]), function(x) (-lambda*(-x)^Gbeta)), sapply(Gains[[1]],function(x) x^alpha)),type = "l")
  return (sum(abs(rEstimated - rReal)))
}

MeasureError <- function(alpha, beta, gammaGain, gammaLoss, lambda, errorGain, errorLoss, errorTolerance, rounding, #Global Params
                         pGain, x0, g, G, nGain, rGain, #Params for Gains
                         pLoss, y0, l, L, nLoss, rLoss, #Params for Losses
                         pLinking, nLinking, rLinking){ #Params for Linking
  times <- 1
  
  alpha <<- alpha
  Gbeta <<- beta
  gammaGain <<- gammaGain
  gammaLoss <<- gammaLoss
  lambda <<- lambda
  errorGain <<- errorGain
  errorLoss <<- errorLoss
  errorTolerance <<- errorTolerance
  rounding <<- rounding
  
  relA <- 0
  for (i in seq(1,times)){
  dataGain <- (analyseGains(pGain, x0, g, G, nGain, rGain))
  dataLoss <- (analyseLosses(pLoss, y0, l, L, nLoss, rLoss))
  relA <- relA + analyseLinking(dataGain, dataLoss, pLinking, nLinking, rLinking)
  }
  return(relA/times)
}

#MeasureError(0.88, 0.88, 0.61, 0.69, 2.25, 0.05, 0.05, 0.00001, TRUE,
#             0.5,1500,300,1000,10,0.5,
#             0.5,-2000,-500,-1000,10,0.5,
#             0.5, 3, 0.5)

MeasureImport <- function(){
  Input <- read.csv(file="Input.csv", head=TRUE, sep="," )
  Output <- NULL
  for (i in seq(1, nrow(Input))){
  Output <- c(Output, MeasureError(Input[i,1],Input[i,2],Input[i,3], Input[i,4],Input[i,5],Input[i,6],Input[i,7],Input[i,8],Input[i,9],
                Input[i,10],Input[i,11],Input[i,12],Input[i,13],Input[i,14],Input[i,15],
                Input[i,16],Input[i,17],Input[i,18],Input[i,19],Input[i,20],Input[i,21],
                Input[i,22],Input[i,23],Input[i,24])) 
  }
  print(Output)
}

MeasureImport()