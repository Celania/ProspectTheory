if(!exists("simulateMeasuringGain", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSetGain", mode="function")) source("TradeOffMethod.R")

analyseGains <- function(p, x0, g, G, n, r){
  print("STARTING GAINS")
  experimentalSet <- simulateMeasuringGain(g, G, p, x0, n)
  print(experimentalSet)
  
  experimentalUtilitySet <- (experimentalSet^0.88)
  print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetGain(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
  
  plot(calculateSlope(experimentalUtilitySet), type="l")
  plot(calculateSlope(estimatedSet), type="l")
  
  plot(experimentalSet, experimentalUtilitySet, type="l")
  plot(experimentalSet, estimatedSet, type="l")

  #return (data.frame(experimentalSet, estimatedSet))
  return (list(experimentalSet=experimentalSet, estimatedSet=estimatedSet))
  
}

analyseLosses <- function(p, y0, l, L, n, r) {
  experimentalSet <- simulateMeasuringLoss(l, L, p, y0, n)
  print("STARTING LOSSES")
  print(experimentalSet)
  
  experimentalUtilitySet <- (-2.25*(-experimentalSet)^0.88)
  print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetLoss(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
  
  plot(calculateSlope(experimentalUtilitySet), type="l")
  plot(calculateSlope(estimatedSet), type="l")
  
  plot(experimentalSet, experimentalUtilitySet, type="l")
  plot(experimentalSet, estimatedSet, type="l")
  
  #return (data.frame(experimentalSet, estimatedSet))
  return (list(experimentalSet=experimentalSet, estimatedSet=estimatedSet))    
}

analyseLinking <- function(Gains, Losses, p, n, r) {
  
  print("START LINKING")
  d <- (prospectTheoreticalValueMixed(Losses[[1]][2], Losses[[1]][1], p, Gains[[1]][1])) 
  #print(d)
  
  #print(Gains[[1]])
  
  k <- findFirstElementMixed(Gains[[1]], d)
  #print(k)
  sz <- calculatesz(r, k, d, Gains[[1]])
  
  deltaULoss <- calculateStepSizeLinking (sz, Gains[[2]][2] - Gains[[2]][1])
  #print(deltaULoss)
  
  print(Losses[[2]])
  for (i in seq(2, length(Losses[[2]]))){
    Losses[[2]][i] <- (Losses[[2]][i-1] - deltaULoss)
  }
  
  plot(c(rev(Losses[[1]]),Gains[[1]]), c(rev(Losses[[2]]), Gains[[2]]), type="l")
}

# combine in one function to ensure the right data for linking

dataGain <- (analyseGains(0.5,1500,300,1000,10,0.5))
dataLoss <- (analyseLosses(0.5,-2000,-500,-1000,10,0.5))
analyseLinking(dataGain, dataLoss, 0.5, 10, 0.5)

plot(c(rev(dataLoss[[1]]),dataGain[[1]]),c(sapply(rev(dataLoss[[1]]), function(x) (-2.25*(-x)^0.88)), sapply(dataGain[[1]],function(x) x^0.88)),type = "l")
